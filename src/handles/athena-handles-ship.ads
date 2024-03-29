with Ada.Streams.Stream_IO;

with Athena.Cargo;

with Athena.Updates;

with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.Module;
with Athena.Handles.Star;

package Athena.Handles.Ship is

   function Ship_Activity_Changed return Athena.Signals.Signal_Type;

   type Ship_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface
     and Has_Identifier_Interface
     and Athena.Cargo.Cargo_Holder_Interface
     and Athena.Updates.Update_Interface
   with private;

   function Reference (Ship : Ship_Handle) return Ship_Reference;
   function Get (Ship : Ship_Reference) return Ship_Handle;
   function Empty_Handle return Ship_Handle;

   overriding function Name
     (Ship : Ship_Handle)
      return String;

   overriding procedure Set_Name
     (Ship     : Ship_Handle;
      New_Name : String);

   function Owner
     (Ship : Ship_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   function Is_Alive
     (Ship : Ship_Handle)
      return Boolean;

   function Is_Idle
     (Ship : Ship_Handle)
      return Boolean;

   function Has_Star_Location
     (Ship : Ship_Handle)
      return Boolean;

   function Has_Deep_Space_Location
     (Ship : Ship_Handle)
      return Boolean;

   function Is_Jumping
     (Ship : Ship_Handle)
      return Boolean;

   function Has_Destination
     (Ship : Ship_Handle)
      return Boolean;

   function Star_Location
     (Ship : Ship_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Origin
     (Ship : Ship_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Destination
     (Ship : Ship_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Progress
     (Ship : Ship_Handle)
      return Unit_Real;

   function Has_Fleet
     (Ship : Ship_Handle)
      return Boolean;

   function Fleet
     (Ship : Ship_Handle)
      return Fleet_Reference;

   procedure Set_Fleet
     (Ship  : Ship_Handle;
      Fleet : Fleet_Reference);

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

   function Start
     (Action : Root_Ship_Action;
      Ship   : Ship_Handle'Class)
     return Duration
   is abstract;

   procedure On_Finished
     (Action : Root_Ship_Action;
      Ship   : Ship_Handle'Class)
   is null;

   function Has_Actions (Ship : Ship_Handle) return Boolean;

   function First_Action (Ship : Ship_Handle) return Root_Ship_Action'Class
     with Pre => Ship.Has_Actions;

   procedure Add_Action
     (Ship : Ship_Handle;
      Action : Root_Ship_Action'Class)
     with Post => Ship.Has_Actions;

   procedure Delete_First_Action (Ship : Ship_Handle)
     with Pre => Ship.Has_Actions;

   function Current_Activity
     (Ship : Ship_Handle)
      return String;

   procedure Add_Experience
     (Ship : Ship_Handle;
      XP   : Non_Negative_Real);

   function Create
     (Name        : String;
      Star        : Athena.Handles.Star.Star_Handle;
      Owner       : Athena.Handles.Empire.Empire_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Fleet       : Fleet_Reference;
      Manager     : Athena.Handles.Manager_Class;
      Destination : Athena.Handles.Star.Star_Handle;
      Script      : String)
      return Ship_Handle;

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

   type Ship_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
     and Athena.Cargo.Cargo_Holder_Interface
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
       & (if Ship.Has_Star_Location
          then " at " & Ship.Star_Location.Name
          else " in deep space"));

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

   procedure Set_Star_Location
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle);

   procedure Set_Destination
     (Ship        : Ship_Handle;
      Destination : Athena.Handles.Star.Star_Handle);

   procedure Clear_Destination
     (Ship        : Ship_Handle);

   procedure Set_Activity
     (Ship     : Ship_Handle;
      Activity : Ship_Activity);

   function Reference (Ship : Ship_Handle) return Ship_Reference
   is (Ship.Reference);

   function Get (Ship : Ship_Reference) return Ship_Handle
   is (Ship /= 0, Ship);

   function Empty_Handle return Ship_Handle
   is (False, 0);

   function Current_Fuel
     (Ship : Ship_Handle)
      return Non_Negative_Real
   is (Ship.Current_Tonnage (Athena.Cargo.Fuel));

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

end Athena.Handles.Ship;
