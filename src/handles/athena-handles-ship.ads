private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Athena.Calendar;

with Ada.Streams.Stream_IO;

with Athena.Cargo;
with Athena.Movers;
with Athena.Trigonometry;

with Athena.Updates;

with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.Module;
with Athena.Handles.Star;
with Athena.Handles.World;

package Athena.Handles.Ship is

   function Ship_Activity_Changed return Athena.Signals.Signal_Type;

   type Ship_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface
     and Has_Identifier_Interface
     and Athena.Cargo.Cargo_Holder_Interface
     and Athena.Movers.Mover_Interface
     and Athena.Updates.Update_Interface
   with private;

   function Reference (Ship : Ship_Handle) return Ship_Reference;
   function Get (Ship : Ship_Reference) return Ship_Handle'Class;
   function Empty_Handle return Ship_Handle;

   overriding function Name
     (Ship : Ship_Handle)
      return String;

   function Owner
     (Ship : Ship_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   function Is_Alive
     (Ship : Ship_Handle)
      return Boolean;

   function Is_Idle
     (Ship : Ship_Handle)
      return Boolean;

   overriding function Location
     (Ship : Ship_Handle)
      return Athena.Movers.Mover_Location;

   overriding function Has_Destination
     (Ship : Ship_Handle)
      return Boolean;

   overriding function Destination
     (Ship : Ship_Handle)
      return Athena.Movers.Mover_Location;

   overriding function Progress
     (Ship : Ship_Handle)
      return Unit_Real;

   procedure Get_Journey
     (Ship        : Ship_Handle;
      Origin      : out Athena.Movers.Mover_Location;
      Destination : out Athena.Movers.Mover_Location;
      Current     : out Athena.Movers.Mover_Location);

   function Moving_To_Star
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function Has_Fleet
     (Ship : Ship_Handle)
      return Boolean;

   function Fleet
     (Ship : Ship_Handle)
      return Fleet_Reference;

   function Has_Manager
     (Ship : Ship_Handle)
      return Boolean;

   function Manager
     (Ship : Ship_Handle)
      return Manager_Class;

   function Design
     (Ship : Ship_Handle)
      return Athena.Handles.Design.Design_Handle;

   function Jump_Drive
     (Ship : Ship_Handle)
      return Athena.Handles.Module.Module_Handle;

   procedure Iterate_Maneuver_Drives
     (Ship : Ship_Handle;
      Process : not null access
        procedure (Maneuver : Athena.Handles.Module.Module_Handle));

   procedure Iterate_Power_Modules
     (Ship    : Ship_Handle;
      Process : not null access
        procedure (Power : Athena.Handles.Module.Module_Handle));

   function Current_Mass
     (Ship : Ship_Handle)
      return Non_Negative_Real;

   function Tank_Size
     (Ship : Ship_Handle)
      return Non_Negative_Real;

   function Current_Fuel
     (Ship : Ship_Handle)
      return Non_Negative_Real;

   type Root_Ship_Action is abstract tagged private;

   function Complete
     (Action : Root_Ship_Action'Class)
      return Boolean;

   function Is_Moving_To
     (Action : Root_Ship_Action;
      Star   : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function Has_Actions (Ship : Ship_Handle) return Boolean;

   function First_Action (Ship : Ship_Handle) return Root_Ship_Action'Class
     with Pre => Ship.Has_Actions;

   procedure Add_Action
     (Ship : Ship_Handle;
      Action : Root_Ship_Action'Class)
     with Post => Ship.Has_Actions;

   --  procedure Delete_First_Action (Ship : Ship_Handle)
   --    with Pre => Ship.Has_Actions;

   function Current_Activity
     (Ship : Ship_Handle)
      return String;

   procedure Add_Experience
     (Ship : Ship_Handle;
      XP   : Non_Negative_Real);

   function Create
     (Name        : String;
      World       : Athena.Handles.World.World_Handle;
      Owner       : Athena.Handles.Empire.Empire_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Fleet       : Fleet_Reference;
      Manager     : Athena.Handles.Manager_Class;
      Script      : String)
      return Ship_Handle'Class;

   type Ship_Update_Handle is
     new Ship_Handle with private;

   overriding function Empty_Handle
      return Ship_Update_Handle;

   overriding function Location
     (Ship : Ship_Update_Handle)
      return Athena.Movers.Mover_Location;

   overriding function Has_Destination
     (Ship : Ship_Update_Handle)
      return Boolean;

   overriding function Destination
     (Ship : Ship_Update_Handle)
      return Athena.Movers.Mover_Location;

   function Update
     (Reference : Ship_Reference)
      return Ship_Update_Handle;

   function Update
     (Handle : Ship_Handle'Class)
      return Ship_Update_Handle;

   procedure Commit (Ship : in out Ship_Update_Handle);

   procedure Set_Name
     (Ship     : in out Ship_Update_Handle;
      New_Name : String);

   function Start
     (Action : Root_Ship_Action;
      Ship   : in out Ship_Update_Handle'Class)
      return Duration
      is abstract;

   procedure On_Finished
     (Action : Root_Ship_Action;
      Ship   : in out Ship_Update_Handle'Class)
   is null;

   procedure Set_Fleet
     (Ship  : in out Ship_Update_Handle;
      Fleet : Fleet_Reference);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Iterate_All
     (Process : not null access
        procedure (Ship : Ship_Handle));

private

   type Ship_Activity is
     (Idle, Loading, Unloading, Departing, Jumping, Arriving);

   type Root_Ship_Action is abstract tagged
      record
         Complete : Boolean := False;
      end record;

   function Image (Action : Root_Ship_Action) return String
   is ("action");

   function Complete
     (Action : Root_Ship_Action'Class)
      return Boolean
   is (Action.Complete);

   function Is_Moving_To
     (Action : Root_Ship_Action;
      Star   : Athena.Handles.Star.Star_Handle)
      return Boolean
   is (False);

   type Ship_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
     and Athena.Cargo.Cargo_Holder_Interface
     and Athena.Movers.Mover_Interface
     and Athena.Updates.Update_Interface
     and Has_Name_Interface with
      record
         Reference : Ship_Reference := 0;
      end record;

   overriding procedure Activate
     (Ship : Ship_Handle);

   overriding function Identifier
     (Ship : Ship_Handle)
      return Object_Identifier;

   overriding function Short_Name
     (Ship : Ship_Handle)
      return String
   is (Ship.Owner.Adjective & " ship "
       & Ship.Identifier & " " & Ship.Name
       & " " & Ship.Current_Location_Name);

   overriding function Cargo_Space
     (Ship     : Ship_Handle;
      Category : Athena.Cargo.Cargo_Category)
      return Non_Negative_Real;

   overriding function Current_Tonnage
     (Ship     : Ship_Handle;
      Category : Athena.Cargo.Cargo_Category)
      return Non_Negative_Real;

   overriding function Current_Quantity
     (Ship     : Ship_Handle;
      Item     : Athena.Cargo.Cargo_Interface'Class)
      return Non_Negative_Real;

   overriding procedure Add_Cargo
     (Ship      : Ship_Handle;
      Item      : Athena.Cargo.Cargo_Interface'Class;
      Quantity  : Non_Negative_Real);

   overriding procedure Remove_Cargo
     (Ship      : Ship_Handle;
      Item      : Athena.Cargo.Cargo_Interface'Class;
      Quantity  : Non_Negative_Real);

   procedure Move_To_System_Space
     (Ship : in out Ship_Update_Handle)
     with Pre => Ship.Orbiting_World;

   procedure Move_To_Deep_Space
     (Ship : in out Ship_Update_Handle)
     with Pre => Ship.In_System_Space;

   procedure Set_System_Space_Destination
     (Ship  : in out Ship_Update_Handle;
      Rho   : Non_Negative_Real;
      Theta : Athena.Trigonometry.Angle);

   --  procedure Set_Deep_Space_Destination
   --    (Ship  : Ship_Handle;
   --     X, Y  : Real);

   procedure Clear_Destination
     (Ship : in out Ship_Update_Handle);

   procedure Set_Destination
     (Ship  : in out Ship_Update_Handle;
      World : Athena.Handles.World.World_Handle);

   procedure Set_Destination
     (Ship  : in out Ship_Update_Handle;
      Star  : Athena.Handles.Star.Star_Handle);

   function Reference (Ship : Ship_Handle) return Ship_Reference
   is (Ship.Reference);

   function Get (Ship : Ship_Reference) return Ship_Handle'Class
   is (Ship_Handle'(Ship /= 0, Ship));

   function Empty_Handle return Ship_Handle
   is (False, 0);

   function Current_Fuel
     (Ship : Ship_Handle)
      return Non_Negative_Real
   is (Ship.Current_Tonnage (Athena.Cargo.Fuel));

   type Update_Type is (Action_Started, Action_Finished,
                        Activity_Update,
                        Destination_Update, Fleet_Update,
                        Location_Update, Name_Update);

   type Update_Record (Update : Update_Type) is
      record
         case Update is
            when Action_Started =>
               Start_Time  : Athena.Calendar.Time;
               Finish_Time : Athena.Calendar.Time;
            when Action_Finished =>
               null;
            when Activity_Update =>
               Activity    : Ship_Activity;
            when Destination_Update =>
               Destination : Athena.Movers.Mover_Location;
            when Fleet_Update =>
               Fleet       : Fleet_Reference;
            when Location_Update =>
               Location    : Athena.Movers.Mover_Location;
            when Name_Update =>
               Name        : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Update_Record);

   type Ship_Update_Handle is
     new Ship_Handle with
      record
         Updates             : Update_Lists.List;
      end record;

   procedure Start_Action
     (Ship            : in out Ship_Update_Handle;
      Action_Duration : Duration);

   procedure Finish_Action (Ship : in out Ship_Update_Handle);

   procedure Set_Activity
     (Ship     : in out Ship_Update_Handle;
      Activity : Ship_Activity);

   procedure Set_World_Location
     (Ship  : in out Ship_Update_Handle;
      World : Athena.Handles.World.World_Handle);

   procedure Set_Star_Location
     (Ship  : in out Ship_Update_Handle;
      Star  : Athena.Handles.Star.Star_Handle;
      Rho   : Non_Negative_Real;
      Theta : Athena.Trigonometry.Angle;
      Error : Non_Negative_Real);

   overriding function Empty_Handle
     return Ship_Update_Handle
   is (False, 0, others => <>);

   function Update
     (Reference : Ship_Reference)
      return Ship_Update_Handle
   is (True, Reference, others => <>);

   function Update
     (Handle : Ship_Handle'Class)
      return Ship_Update_Handle
   is (Handle.Has_Element, Handle.Reference, others => <>);

end Athena.Handles.Ship;
