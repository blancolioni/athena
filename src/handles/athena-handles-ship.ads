with Ada.Streams.Stream_IO;

with Athena.Updates;

with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.Module;
with Athena.Handles.Star;

package Athena.Handles.Ship is

   type Ship_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface
     and Has_Identifier_Interface
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

   function Alive
     (Ship : Ship_Handle)
      return Boolean;

   function Idle
     (Ship : Ship_Handle)
      return Boolean;

   function Has_Star_Location
     (Ship : Ship_Handle)
      return Boolean;

   function Has_Deep_Space_Location
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

   function Power_Module
     (Ship : Ship_Handle)
      return Athena.Handles.Module.Module_Handle;

   procedure Iterate_Maneuver_Drives
     (Ship : Ship_Handle;
      Process : not null access
        procedure (Maneuver : Athena.Handles.Module.Module_Handle));

   function Current_Cargo
     (Ship : Ship_Handle;
      Cargo : Cargo_Class)
      return Non_Negative_Real;

   procedure Set_Current_Cargo
     (Ship     : Ship_Handle;
      Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real);

   type Root_Ship_Action is abstract tagged private;

   function Complete
     (Action : Root_Ship_Action'Class)
      return Boolean;

   function Execute
     (Action : Root_Ship_Action;
      Ship   : Ship_Handle'Class)
     return Boolean
   is abstract;

   function Has_Actions (Ship : Ship_Handle) return Boolean;

   function First_Action (Ship : Ship_Handle) return Root_Ship_Action'Class
     with Pre => Ship.Has_Actions;

   procedure Add_Action
     (Ship : Ship_Handle;
      Action : Root_Ship_Action'Class)
     with Post => Ship.Has_Actions;

   procedure Delete_First_Action (Ship : Ship_Handle)
     with Pre => Ship.Has_Actions;

   procedure Set_Progress
     (Ship     : Ship_Handle;
      Progress : Unit_Real)
     with Pre => Progress < 1.0;

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

   type Ship_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
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

   procedure Set_Star_Location
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle);

   procedure Set_Destination
     (Ship        : Ship_Handle;
      Destination : Athena.Handles.Star.Star_Handle);

   procedure Clear_Destination
     (Ship        : Ship_Handle);

   function Reference (Ship : Ship_Handle) return Ship_Reference
   is (Ship.Reference);

   function Get (Ship : Ship_Reference) return Ship_Handle
   is (Ship /= 0, Ship);

   function Empty_Handle return Ship_Handle
   is (False, 0);

   type Root_Ship_Action is abstract tagged
      record
         Complete : Boolean := False;
      end record;

   function Complete
     (Action : Root_Ship_Action'Class)
      return Boolean
   is (Action.Complete);

end Athena.Handles.Ship;
