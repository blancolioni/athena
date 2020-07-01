with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;

with Athena.Logging;
with Athena.Random;

with Athena.Server;
with Athena.Updates.Events;

with Athena.Handles.Design_Module;

with Athena.Handles.Vectors;

package body Athena.Handles.Ship is

   Signal_Ship_Activity_Changed : constant String :=
                                    "signal-ship-activity-changed";

   package Ship_Action_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Ship_Action'Class);

   package Module_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Module_Reference);

   type Ship_Record is
     new Athena.Movers.Mover_Interface with
      record
         Identifier        : Object_Identifier;
         Name              : Ada.Strings.Unbounded.Unbounded_String;
         Alive             : Boolean;
         Executing         : Boolean;
         Managed           : Boolean;
         Experience        : Non_Negative_Real;
         Location          : Athena.Movers.Mover_Location;
         Destination       : Athena.Movers.Mover_Location;
         Owner             : Athena.Handles.Empire.Empire_Handle;
         Design            : Athena.Handles.Design.Design_Handle;
         Modules           : Module_Lists.List;
         Drives            : Module_Lists.List;
         Jump_Drive        : Module_Reference;
         Power             : Module_Lists.List;
         Tank_Size         : Non_Negative_Real;
         Cargo_Space       : Non_Negative_Real;
         Fleet             : Fleet_Reference;
         Manager           : Athena.Handles.Manager_Class;
         Cargo             : Athena.Cargo.Cargo_Container;
         Activity          : Ship_Activity;
         Actions           : Ship_Action_Lists.List;
         Action_Started    : Athena.Calendar.Time;
         Action_Finished   : Athena.Calendar.Time;
         Next_Update       : Athena.Calendar.Time;
         Script            : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Location
     (Rec : Ship_Record)
      return Athena.Movers.Mover_Location
   is (Rec.Location);

   overriding function Has_Destination
     (Rec : Ship_Record)
      return Boolean
   is (not Athena.Movers."="
       (Rec.Destination.Loc_Type, Athena.Movers.Nowhere));

   overriding function Destination
     (Rec : Ship_Record)
      return Athena.Movers.Mover_Location
   is (Rec.Destination);

   overriding function Progress
     (Rec : Ship_Record)
      return Unit_Real;

   package Ship_Vectors is
     new Athena.Handles.Vectors
       (Real_Ship_Reference, Ship_Record);

   Vector : Ship_Vectors.Vector;

   overriding function Identifier
     (Ship : Ship_Handle)
      return Object_Identifier
   is (Vector (Ship.Reference).Identifier);

   overriding function Name
     (Ship : Ship_Handle)
      return String
   is (-(Vector (Ship.Reference).Name));

   function Design
     (Ship : Ship_Handle)
      return Athena.Handles.Design.Design_Handle
   is (Vector (Ship.Reference).Design);

   function Owner
     (Ship : Ship_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Vector (Ship.Reference).Owner);

   function Is_Alive
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Alive);

   function Is_Idle
     (Ship : Ship_Handle)
      return Boolean
   is (not Ship.Has_Destination
       and then Vector (Ship.Reference).Actions.Is_Empty);

   function Has_Manager
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Managed);

   function Manager
     (Ship : Ship_Handle)
      return Manager_Class
   is (Vector (Ship.Reference).Manager);

   overriding function Location
     (Ship : Ship_Handle)
      return Athena.Movers.Mover_Location
   is (Vector (Ship.Reference).Location);

   overriding function Has_Destination
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Destination.Loc_Type not in
         Athena.Movers.Nowhere);

   overriding function Destination
     (Ship : Ship_Handle)
      return Athena.Movers.Mover_Location
   is (Vector (Ship.Reference).Destination);

   function Has_Fleet
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Fleet /= Null_Fleet_Reference);

   function Fleet
     (Ship : Ship_Handle)
      return Fleet_Reference
   is (Vector (Ship.Reference).Fleet);

   function Jump_Drive
     (Ship : Ship_Handle)
      return Athena.Handles.Module.Module_Handle
   is (Athena.Handles.Module.Get (Vector (Ship.Reference).Jump_Drive));

   overriding function Current_Tonnage
     (Ship     : Ship_Handle;
      Category : Athena.Cargo.Cargo_Category)
      return Non_Negative_Real
   is (Vector (Ship.Reference).Cargo.Tonnage (Category));

   overriding function Current_Quantity
     (Ship     : Ship_Handle;
      Item     : Athena.Cargo.Cargo_Interface'Class)
      return Non_Negative_Real
   is (Vector (Ship.Reference).Cargo.Quantity (Item));

   function Current_Activity
     (Ship : Ship_Handle)
      return String
   is (if Ship.Has_Actions
       then Ship.First_Action.Image
       else "idle");

   function Has_Actions (Ship : Ship_Handle) return Boolean
   is (not Vector (Ship.Reference).Actions.Is_Empty);

   function First_Action (Ship : Ship_Handle) return Root_Ship_Action'Class
   is (Vector (Ship.Reference).Actions.First_Element);

   function Tank_Size (Ship : Ship_Handle) return Non_Negative_Real
   is (Vector (Ship.Reference).Tank_Size);

   function Moving_To_Star
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
      return Boolean
   is (for some Action of Vector (Ship.Reference).Actions =>
          Action.Is_Moving_To (Star));

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Ship : Ship_Handle)
   is
      use type Athena.Calendar.Time;

      Ship_Update : Ship_Update_Handle := Ship.Update;

   begin
      if Ship.Has_Destination then
         Ship.Log ("activating; destination "
                   & Ship.Destination_Name);
      else
         Ship.Log ("activating: current activity "
                   & Ship.Current_Activity);
      end if;

      if Vector (Ship.Reference).Executing
        and then Vector (Ship.Reference).Action_Finished
        > Athena.Calendar.Clock
      then
         Ship.Log
           ("current action finishes at "
            & Athena.Calendar.Image
              (Vector (Ship.Reference).Action_Finished, True));
         return;
      end if;

      if Vector (Ship.Reference).Executing then
         Vector (Ship.Reference).Actions.First_Element
           .On_Finished (Ship_Update);
         Ship_Update.Finish_Action;
         Ship_Update.Commit;
      end if;

      if not Vector (Ship.Reference).Actions.Is_Empty then
         declare
            Execution_Time : constant Duration :=
                               Vector (Ship.Reference)
                               .Actions.First_Element.Start (Ship_Update);
         begin
            Ship_Update.Start_Action (Execution_Time);
            Ship_Update.Commit;
            Athena.Updates.Events.Update_With_Delay
              (Execution_Time, Ship);
            return;
         end;
      elsif Ship.Has_Manager then
         Ship_Update.Set_Activity (Idle);
         Ship_Update.Commit;
         Ship.Owner.Send_Signal (Ship.Manager);
         return;
      end if;

      Ship_Update.Set_Activity (Idle);
      Ship_Update.Commit;

      Athena.Updates.Events.Update_With_Delay
        (Athena.Calendar.Days (1), Ship);

   end Activate;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Ship   : Ship_Handle;
      Action : Root_Ship_Action'Class)
   is
      procedure Update (Rec : in out Ship_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Ship_Record) is
         Actions  : Ship_Action_Lists.List renames Rec.Actions;
         Schedule : constant Boolean := Actions.Is_Empty;
      begin
         Actions.Append (Action);
         if Schedule then
            Athena.Updates.Events.Update_With_Delay (0.0, Ship);
         end if;
      end Update;

   begin
      Vector.Update (Ship.Reference, Update'Access);
   end Add_Action;

   ---------------
   -- Add_Cargo --
   ---------------

   overriding procedure Add_Cargo
     (Ship      : Ship_Handle;
      Item      : Athena.Cargo.Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is
      procedure Update (Rec : in out Ship_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Ship_Record) is
      begin
         Rec.Cargo.Add_Cargo (Item, Quantity);
      end Update;

   begin
      Vector.Update (Ship.Reference, Update'Access);
   end Add_Cargo;

   --------------------
   -- Add_Experience --
   --------------------

   procedure Add_Experience
     (Ship : Ship_Handle;
      XP   : Non_Negative_Real)
   is
      procedure Update (Rec : in out Ship_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Ship_Record) is
      begin
         Rec.Experience := Rec.Experience + XP;
      end Update;

   begin
      Vector.Update (Ship.Reference, Update'Access);
   end Add_Experience;

   -----------------
   -- Cargo_Space --
   -----------------

   overriding function Cargo_Space
     (Ship     : Ship_Handle;
      Category : Athena.Cargo.Cargo_Category)
      return Non_Negative_Real
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      case Category is
         when Athena.Cargo.Commodity =>
            return Rec.Cargo_Space;
         when Athena.Cargo.Fuel =>
            return Rec.Tank_Size;
         when Athena.Cargo.People =>
            return Rec.Design.Passenger_Berths;
      end case;
   end Cargo_Space;

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Ship        : in out Ship_Update_Handle)
   is
   begin
      Ship.Updates.Append
        ((Destination_Update, (Loc_Type => Athena.Movers.Nowhere)));
   end Clear_Destination;

   ------------
   -- Commit --
   ------------

   procedure Commit (Ship : in out Ship_Update_Handle) is

      New_Activity : Boolean := False;

      procedure Do_Commit (Rec : in out Ship_Record);

      ---------------
      -- Do_Commit --
      ---------------

      procedure Do_Commit (Rec : in out Ship_Record) is
      begin
         for Item of Ship.Updates loop
            case Item.Update is
               when Action_Finished =>
                  Rec.Executing := False;
                  Rec.Actions.Delete_First;
               when Action_Started =>
                  Rec.Executing := True;
                  Rec.Action_Started := Item.Start_Time;
                  Rec.Action_Finished := Item.Finish_Time;
               when Activity_Update =>
                  New_Activity := True;
                  Rec.Activity := Item.Activity;
               when Destination_Update =>
                  Rec.Destination := Item.Destination;
               when Fleet_Update =>
                  Rec.Fleet := Item.Fleet;
               when Location_Update =>
                  Rec.Location := Item.Location;
               when Name_Update =>
                  Rec.Name := Item.Name;
            end case;
         end loop;

         Athena.Logging.Log
           (Rec.Identifier & " " & (-Rec.Name)
            & ": location " & Rec.Current_Location_Name
            & "; destination "
            & (if Rec.Has_Destination
              then Rec.Destination_Name
              else "none"));
      end Do_Commit;

   begin
      Vector.Update (Ship.Reference, Do_Commit'Access);
      if New_Activity then
         Athena.Server.Emit
           (Source      => Ship,
            Signal      => Ship_Activity_Changed,
            Signal_Data => Athena.Signals.Null_Signal_Data);
      end if;
      Ship.Updates.Clear;
   end Commit;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String;
      World       : Athena.Handles.World.World_Handle;
      Owner       : Athena.Handles.Empire.Empire_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Fleet       : Fleet_Reference;
      Manager     : Athena.Handles.Manager_Class;
      Script      : String)
      return Ship_Handle'Class
   is
      use Athena.Calendar;
      Rec : Ship_Record :=
              Ship_Record'
                (Identifier        => Next_Identifier,
                 Name              => +Name,
                 Alive             => True,
                 Executing         => False,
                 Managed           => True,
                 Experience        => 0.0,
                 Location          => (Athena.Movers.World_Orbit, World),
                 Destination       => <>,
                 Owner             => Owner,
                 Design            => Design,
                 Modules           => <>,
                 Drives            => <>,
                 Jump_Drive        => Null_Module_Reference,
                 Power             => <>,
                 Tank_Size         => Design.Tank_Size,
                 Cargo_Space       => Design.Free_Space,
                 Cargo             => <>,
                 Fleet             => Fleet,
                 Manager           => Manager,
                 Next_Update       =>
                   Clock + Days (Athena.Random.Unit_Random),
                 Action_Started    => Clock,
                 Action_Finished   => Clock,
                 Actions           => <>,
                 Activity          => Idle,
                 Script            => +Script);

      procedure Add_Design_Module
        (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle);

      -----------------------
      -- Add_Design_Module --
      -----------------------

      procedure Add_Design_Module
        (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle)
      is
         Module : constant Athena.Handles.Module.Module_Handle :=
                    Athena.Handles.Module.Create (Design_Module);
      begin
         Rec.Modules.Append (Module.Reference);
         if Module.Component.Has_Jump
           and then Rec.Jump_Drive = Null_Module_Reference
         then
            Rec.Jump_Drive := Module.Reference;
         end if;
         if Module.Component.Has_Impulse then
            Rec.Drives.Append (Module.Reference);
         end if;
         if Module.Component.Has_Power_Output then
            Rec.Power.Append (Module.Reference);
         end if;
      end Add_Design_Module;

      Reference : Ship_Reference;

   begin
      Design.Iterate_Design_Modules (Add_Design_Module'Access);

      Vector.Append (Rec, Reference);

      return Handle : constant Ship_Handle := (True, Reference) do
         Athena.Updates.Events.Update_At
           (Clock  => Rec.Next_Update,
            Update => Handle);
      end return;
   end Create;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Ship_Handle)
      return Non_Negative_Real
   is
      Rec  : constant Ship_Record := Vector.Element (Ship.Reference);
      Mass : Non_Negative_Real := Rec.Design.Mass;
   begin
      Mass := Mass + Rec.Cargo.Total_Mass;
      return Mass;
   end Current_Mass;

   -----------------
   -- Destination --
   -----------------

   overriding function Destination
     (Ship : Ship_Update_Handle)
      return Athena.Movers.Mover_Location
   is
   begin
      for Item of reverse Ship.Updates loop
         if Item.Update = Destination_Update then
            return Item.Destination;
         end if;
      end loop;
      return Ship_Handle (Ship).Destination;
   end Destination;

   -------------------
   -- Finish_Action --
   -------------------

   procedure Finish_Action (Ship : in out Ship_Update_Handle) is
   begin
      Ship.Updates.Append ((Update => Action_Finished));
   end Finish_Action;

   -----------------
   -- Get_Journey --
   -----------------

   procedure Get_Journey
     (Ship        : Ship_Handle;
      Origin      : out Athena.Movers.Mover_Location;
      Destination : out Athena.Movers.Mover_Location;
      Current     : out Athena.Movers.Mover_Location)
   is
      use Athena.Movers;
      Rec : constant Ship_Record := Vector.Element (Ship.Reference);
   begin
      Origin := Rec.Location;
      Destination :=
        (if Rec.Has_Destination then Rec.Destination
         else (Loc_Type => Athena.Movers.Nowhere));
      Current :=
        (if not Rec.Has_Destination then Rec.Location
         else Rec.Current_Location);
   end Get_Journey;

   ---------------------
   -- Has_Destination --
   ---------------------

   overriding function Has_Destination
     (Ship : Ship_Update_Handle)
      return Boolean
   is
      use type Athena.Movers.Mover_Location_Type;
   begin
      for Item of reverse Ship.Updates loop
         if Item.Update = Destination_Update then
            return Item.Destination.Loc_Type /= Athena.Movers.Nowhere;
         end if;
      end loop;
      return Ship_Handle (Ship).Has_Destination;
   end Has_Destination;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Process : not null access
        procedure (Ship : Ship_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Ship_Handle (Get (Reference)));
      end loop;
   end Iterate_All;

   -----------------------------
   -- Iterate_Maneuver_Drives --
   -----------------------------

   procedure Iterate_Maneuver_Drives
     (Ship    : Ship_Handle;
      Process : not null access
        procedure (Maneuver : Athena.Handles.Module.Module_Handle))
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      for Drive of Rec.Drives loop
         Process (Athena.Handles.Module.Get (Drive));
      end loop;
   end Iterate_Maneuver_Drives;

   ---------------------------
   -- Iterate_Power_Modules --
   ---------------------------

   procedure Iterate_Power_Modules
     (Ship    : Ship_Handle;
      Process : not null access
        procedure (Power : Athena.Handles.Module.Module_Handle))
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      for Power of Rec.Power loop
         Process (Athena.Handles.Module.Get (Power));
      end loop;
   end Iterate_Power_Modules;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Vector.Read (Stream);
      for Reference in 1 .. Vector.Last_Index loop
         if Vector (Reference).Actions.Is_Empty then
            Athena.Updates.Events.Update_At
              (Vector (Reference).Next_Update,
               Get (Reference));
         else
            Athena.Updates.Events.Update_At
              (Vector (Reference).Action_Finished,
               Get (Reference));
         end if;
      end loop;
   end Load;

   --------------
   -- Location --
   --------------

   overriding function Location
     (Ship : Ship_Update_Handle)
      return Athena.Movers.Mover_Location
   is
   begin
      for Item of reverse Ship.Updates loop
         if Item.Update = Location_Update then
            return Item.Location;
         end if;
      end loop;

      return Ship_Handle (Ship).Location;
   end Location;

   ------------------------
   -- Move_To_Deep_Space --
   ------------------------

   procedure Move_To_Deep_Space
     (Ship : in out Ship_Update_Handle)
   is
   begin
      Ship.Updates.Append
        (Update_Record'
           (Update      => Location_Update,
            Location    =>
              (Athena.Movers.Deep_Space,
               (Ship.Location.Star.X, Ship.Location.Star.Y, 0.0))));
   end Move_To_Deep_Space;

   --------------------------
   -- Move_To_System_Space --
   --------------------------

   procedure Move_To_System_Space
     (Ship : in out Ship_Update_Handle)
   is
   begin
      Ship.Updates.Append
        (Update_Record'
           (Update      => Location_Update,
            Location    =>
              (Athena.Movers.System_Space, Ship.Location.World.Star,
               Ship.Location.World.Current_Global_Position)));
   end Move_To_System_Space;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Ship : Ship_Handle)
      return Unit_Real
   is
   begin
      return Vector.Element (Ship.Reference).Progress;
   end Progress;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Rec : Ship_Record)
      return Unit_Real
   is
      use type Athena.Calendar.Time;
      Action_Started  : constant Athena.Calendar.Time :=
                          Rec.Action_Started;
      Action_Finished : constant Athena.Calendar.Time :=
                          Rec.Action_Finished;
      Total_Time    : constant Real :=
                        Real (Action_Finished - Action_Started);
      Elapsed_Time    : constant Real :=
                          Real (Athena.Calendar.Clock - Action_Started);
   begin
      if Total_Time = 0.0 then
         return 0.0;
      else
         return Unit_Clamp (Elapsed_Time / Total_Time);
      end if;
   end Progress;

   ------------------
   -- Remove_Cargo --
   ------------------

   overriding procedure Remove_Cargo
     (Ship      : Ship_Handle;
      Item      : Athena.Cargo.Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      Vector (Ship.Reference).Cargo.Remove_Cargo (Item, Quantity);
   end Remove_Cargo;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Vector.Write (Stream);
   end Save;

   ------------------
   -- Set_Activity --
   ------------------

   procedure Set_Activity
     (Ship     : in out Ship_Update_Handle;
      Activity : Ship_Activity)
   is
   begin
      Ship.Updates.Append ((Activity_Update, Activity));
   end Set_Activity;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship  : in out Ship_Update_Handle;
      World : Athena.Handles.World.World_Handle)
   is
   begin
      Ship.Updates.Append ((Destination_Update,
                           (Athena.Movers.World_Orbit, World)));
   end Set_Destination;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship : in out Ship_Update_Handle;
      Star : Athena.Handles.Star.Star_Handle)
   is
   begin
      Ship.Updates.Append ((Destination_Update,
                           (Athena.Movers.Deep_Space,
                            (Star.X, Star.Y, 0.0))));
   end Set_Destination;

   ---------------
   -- Set_Fleet --
   ---------------

   procedure Set_Fleet
     (Ship  : in out Ship_Update_Handle;
      Fleet : Fleet_Reference)
   is
   begin
      Ship.Updates.Append ((Fleet_Update, Fleet));
   end Set_Fleet;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Ship     : in out Ship_Update_Handle;
      New_Name : String)
   is
   begin
      Ship.Updates.Append ((Name_Update, +New_Name));
   end Set_Name;

   -----------------------
   -- Set_Star_Location --
   -----------------------

   procedure Set_Star_Location
     (Ship  : in out Ship_Update_Handle;
      Star  : Athena.Handles.Star.Star_Handle;
      Rho   : Non_Negative_Real;
      Theta : Athena.Trigonometry.Angle;
      Error : Non_Negative_Real)
   is
      X   : constant Real := Rho * Athena.Trigonometry.Cos (Theta)
              + (if Error = 0.0 then 0.0
                 else Athena.Random.Normal_Random (Error));
      Y   : constant Real := Rho * Athena.Trigonometry.Sin (Theta)
              + (if Error = 0.0 then 0.0
                 else Athena.Random.Normal_Random (Error));
   begin
      Ship.Updates.Append
        ((Location_Update,
         (Athena.Movers.System_Space, Star, (X, Y, 0.0))));
   end Set_Star_Location;

   ----------------------------------
   -- Set_System_Space_Destination --
   ----------------------------------

   procedure Set_System_Space_Destination
     (Ship  : in out Ship_Update_Handle;
      Rho   : Non_Negative_Real;
      Theta : Athena.Trigonometry.Angle)
   is
      Star : constant Athena.Handles.Star.Star_Handle :=
               Ship.Location_Star;
      X    : constant Real := Rho * Athena.Trigonometry.Cos (Theta);
      Y    : constant Real := Rho * Athena.Trigonometry.Sin (Theta);
   begin
      Ship.Updates.Append
        ((Destination_Update,
         (Athena.Movers.System_Space, Star,
            (X, Y, 0.0))));
   end Set_System_Space_Destination;

   ------------------------
   -- Set_World_Location --
   ------------------------

   procedure Set_World_Location
     (Ship  : in out Ship_Update_Handle;
      World : Athena.Handles.World.World_Handle)
   is
   begin
      Ship.Updates.Append
        ((Location_Update, (Athena.Movers.World_Orbit, World)));
   end Set_World_Location;

   ---------------------------
   -- Ship_Activity_Changed --
   ---------------------------

   function Ship_Activity_Changed return Athena.Signals.Signal_Type is
   begin
      return Athena.Signals.Signal (Signal_Ship_Activity_Changed);
   end Ship_Activity_Changed;

   ------------------
   -- Start_Action --
   ------------------

   procedure Start_Action
     (Ship            : in out Ship_Update_Handle;
      Action_Duration : Duration)
   is
      use type Athena.Calendar.Time;
   begin
      Ship.Updates.Append
        ((Action_Started, Athena.Calendar.Clock,
         Athena.Calendar.Clock + Action_Duration));
   end Start_Action;

begin

   Athena.Signals.Create_Signal (Signal_Ship_Activity_Changed);

end Athena.Handles.Ship;
