with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Athena.Calendar;
with Athena.Random;

with Athena.Server;
with Athena.Updates.Events;

with Athena.Handles.Design_Module;

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
      record
         Identifier        : Object_Identifier;
         Name              : Ada.Strings.Unbounded.Unbounded_String;
         Alive             : Boolean;
         Executing         : Boolean;
         Managed           : Boolean;
         Experience        : Non_Negative_Real;
         Location          : Athena.Movers.Mover_Location;
         Destination       : Athena.Movers.Mover_Location;
         Owner             : Empire_Reference;
         Design            : Design_Reference;
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

   package Ship_Vectors is
     new Ada.Containers.Vectors
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
   is (Athena.Handles.Design.Get (Vector (Ship.Reference).Design));

   function Owner
     (Ship : Ship_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (Ship.Reference).Owner));

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
   begin
      if Ship.Has_Destination then
         Ship.Log ("activating; destination "
                   & Ship.Destination_Name);
      else
         Ship.Log ("activating: current activity "
                   & Ship.Current_Activity);
      end if;

      declare
         procedure Report (Cargo : Athena.Cargo.Cargo_Interface'Class;
                           Quantity : Non_Negative_Real);

         ------------
         -- Report --
         ------------

         procedure Report (Cargo    : Athena.Cargo.Cargo_Interface'Class;
                           Quantity : Non_Negative_Real)
         is
         begin
            Ship.Log ("cargo: " & Image (Quantity) & " " & Cargo.Tag);
         end Report;

      begin
         Vector (Ship.Reference).Cargo.Iterate (Report'Access);
      end;

      if Vector (Ship.Reference).Executing then
         if Vector (Ship.Reference).Action_Finished
           > Athena.Calendar.Clock
         then
            Ship.Log
              ("current action finishes at "
               & Athena.Calendar.Image
                 (Vector (Ship.Reference).Action_Finished, True));
            return;
         end if;

         Vector (Ship.Reference).Action_Started :=
           Athena.Calendar.Clock;
         Ship.First_Action.On_Finished (Ship);
         Ship.Delete_First_Action;
         Vector (Ship.Reference).Executing := False;
      end if;

      if Ship.Has_Actions then
         declare
            Execution_Time : constant Duration :=
              Ship.First_Action.Start (Ship);
         begin
            Vector (Ship.Reference).Executing := True;
            Vector (Ship.Reference).Action_Started :=
              Athena.Calendar.Clock;
            Vector (Ship.Reference).Action_Finished :=
              Athena.Calendar.Clock + Execution_Time;
            Athena.Updates.Events.Update_With_Delay
              (Execution_Time, Ship);
            return;
         end;
      elsif Ship.Has_Manager then
         Ship.Set_Activity (Idle);
         Ship.Log ("signaling manager " & Ship.Manager'Image);
         Ship.Owner.Send_Signal (Ship.Manager);
         return;
      end if;

      Ship.Set_Activity (Idle);
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
      Actions : Ship_Action_Lists.List renames Vector (Ship.Reference).Actions;
      Schedule : constant Boolean := Actions.Is_Empty;
   begin
      Actions.Append (Action);
      if Schedule then
         Athena.Updates.Events.Update_With_Delay (0.0, Ship);
      end if;
   end Add_Action;

   ---------------
   -- Add_Cargo --
   ---------------

   overriding procedure Add_Cargo
     (Ship      : Ship_Handle;
      Item      : Athena.Cargo.Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      Vector (Ship.Reference).Cargo.Add_Cargo (Item, Quantity);
   end Add_Cargo;

   --------------------
   -- Add_Experience --
   --------------------

   procedure Add_Experience
     (Ship : Ship_Handle;
      XP   : Non_Negative_Real)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Experience := Rec.Experience + XP;
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
            return Athena.Handles.Design.Get (Rec.Design).Passenger_Berths;
      end case;
   end Cargo_Space;

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Ship        : Ship_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Destination := (Loc_Type => Athena.Movers.Nowhere);
   end Clear_Destination;

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
      return Ship_Handle
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
                 Owner             => Owner.Reference,
                 Design            => Design.Reference,
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

   begin
      Design.Iterate_Design_Modules (Add_Design_Module'Access);
      Vector.Append (Rec);

      return Handle : constant Ship_Handle :=
        (True, Vector.Last_Index)
      do
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
      Rec  : Ship_Record renames Vector (Ship.Reference);
      Mass : Non_Negative_Real := Ship.Design.Mass;
   begin
      Mass := Mass + Rec.Cargo.Total_Mass;
      return Mass;
   end Current_Mass;

   -------------------------
   -- Delete_First_Action --
   -------------------------

   procedure Delete_First_Action (Ship : Ship_Handle) is
   begin
      Vector (Ship.Reference).Actions.Delete_First;
   end Delete_First_Action;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Process : not null access
        procedure (Ship : Ship_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Get (Reference));
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
      Ship_Vectors.Vector'Read (Stream, Vector);
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

   ------------------------
   -- Move_To_Deep_Space --
   ------------------------

   procedure Move_To_Deep_Space
     (Ship : Ship_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Location :=
        (Athena.Movers.Deep_Space,
         (Rec.Location.Star.X, Rec.Location.Star.Y, 0.0));
   end Move_To_Deep_Space;

   --------------------------
   -- Move_To_System_Space --
   --------------------------

   procedure Move_To_System_Space
     (Ship : Ship_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Location :=
        (Athena.Movers.System_Space, Rec.Location.World.Star,
         Rec.Location.World.Current_Global_Position);
   end Move_To_System_Space;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Ship : Ship_Handle)
      return Unit_Real
   is
      use type Athena.Calendar.Time;
      Action_Started  : constant Athena.Calendar.Time :=
                          Vector (Ship.Reference).Action_Started;
      Action_Finished : constant Athena.Calendar.Time :=
                          Vector (Ship.Reference).Action_Finished;
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
      Ship_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ------------------
   -- Set_Activity --
   ------------------

   procedure Set_Activity
     (Ship     : Ship_Handle;
      Activity : Ship_Activity)
   is
   begin
      if Activity /= Vector (Ship.Reference).Activity then
         Ship.Log ("activity: " & Activity'Image);
         Vector (Ship.Reference).Activity := Activity;
         Athena.Server.Emit
           (Source      => Ship,
            Signal      => Ship_Activity_Changed,
            Signal_Data => Athena.Signals.Null_Signal_Data);
      end if;
   end Set_Activity;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship  : Ship_Handle;
      World : Athena.Handles.World.World_Handle)
   is
      Rec      : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Destination :=
        (Athena.Movers.World_Orbit, World);
   end Set_Destination;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Destination := (Athena.Movers.Deep_Space,
                          (Star.X, Star.Y, 0.0));
   end Set_Destination;

   ---------------
   -- Set_Fleet --
   ---------------

   procedure Set_Fleet
     (Ship  : Ship_Handle;
      Fleet : Fleet_Reference)
   is
   begin
      Vector (Ship.Reference).Fleet := Fleet;
   end Set_Fleet;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Ship     : Ship_Handle;
      New_Name : String)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Name := +New_Name;
   end Set_Name;

   -----------------------
   -- Set_Star_Location --
   -----------------------

   procedure Set_Star_Location
     (Ship  : Ship_Handle;
      Star  : Athena.Handles.Star.Star_Handle;
      Rho   : Non_Negative_Real;
      Theta : Athena.Trigonometry.Angle;
      Error : Non_Negative_Real)
   is
      X : constant Real := Rho * Athena.Trigonometry.Cos (Theta)
            + (if Error = 0.0 then 0.0
               else Athena.Random.Normal_Random (Error));
      Y : constant Real := Rho * Athena.Trigonometry.Sin (Theta)
            + (if Error = 0.0 then 0.0
               else Athena.Random.Normal_Random (Error));
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Location := (Athena.Movers.System_Space, Star, (X, Y, 0.0));
   end Set_Star_Location;

   ----------------------------------
   -- Set_System_Space_Destination --
   ----------------------------------

   procedure Set_System_Space_Destination
     (Ship  : Ship_Handle;
      Rho   : Non_Negative_Real;
      Theta : Athena.Trigonometry.Angle)
   is
      Star : constant Athena.Handles.Star.Star_Handle :=
               Ship.Location_Star;
      X : constant Real := Rho * Athena.Trigonometry.Cos (Theta);
      Y : constant Real := Rho * Athena.Trigonometry.Sin (Theta);
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Destination :=
        (Athena.Movers.System_Space, Star,
         (X, Y, 0.0));
   end Set_System_Space_Destination;

   ------------------------
   -- Set_World_Location --
   ------------------------

   procedure Set_World_Location
     (Ship  : Ship_Handle;
      World : Athena.Handles.World.World_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Location := (Athena.Movers.World_Orbit, World);
   end Set_World_Location;

   ---------------------------
   -- Ship_Activity_Changed --
   ---------------------------

   function Ship_Activity_Changed return Athena.Signals.Signal_Type is
   begin
      return Athena.Signals.Signal (Signal_Ship_Activity_Changed);
   end Ship_Activity_Changed;

begin

   Athena.Signals.Create_Signal (Signal_Ship_Activity_Changed);

end Athena.Handles.Ship;
