with Ada.Streams.Stream_IO;

with Athena.Calendar;
with Athena.Signals;
with Athena.Updates;

with Athena.Handles.Commodity;
with Athena.Handles.Empire;
with Athena.Handles.Facility;
with Athena.Handles.Installation;
with Athena.Handles.World;

package Athena.Handles.Colony is

   function Colony_Owner_Changed return Athena.Signals.Signal_Type;

   type Colony_Handle is
     new Root_Athena_Handle
     and Athena.Updates.Update_Interface
     and Athena.Handles.Facility.Production_Context
     and Athena.Handles.Commodity.Stock_Interface
   with private;

   function Reference (Colony : Colony_Handle) return Colony_Reference;
   function Get (Colony : Colony_Reference) return Colony_Handle;
   function Empty_Handle return Colony_Handle;

   function World
     (Colony : Colony_Handle)
      return Athena.Handles.World.World_Handle;

   function Founded
     (Colony : Colony_Handle)
      return Athena.Calendar.Time;

   function Population
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Employed
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Construct
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Industry
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Material
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Owner
     (Colony : Colony_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   procedure Add_Installation
     (Colony       : Colony_Handle;
      Installation : Athena.Handles.Installation.Installation_Handle);

   overriding function Get_Stock
     (Colony    : Colony_Handle;
      Commodity : Athena.Handles.Commodity.Commodity_Handle'Class)
      return Non_Negative_Real;

   overriding procedure Set_Stock
     (Colony    : Colony_Handle;
      Commodity : Athena.Handles.Commodity.Commodity_Handle'Class;
      Quantity  : Non_Negative_Real);

   procedure Set_Population
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Construct
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Industry
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Material
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Owner
     (Colony    : Colony_Handle;
      New_Owner : Athena.Handles.Empire.Empire_Handle);

   type Root_Colony_Action is abstract tagged private;

   function Execute
     (Action : Root_Colony_Action;
      Colony   : Colony_Handle'Class)
      return Boolean
      is abstract;

   function Has_Actions (Colony : Colony_Handle) return Boolean;

   function First_Action
     (Colony : Colony_Handle)
      return Root_Colony_Action'Class
     with Pre => Colony.Has_Actions;

   procedure Add_Action
     (Colony   : Colony_Handle;
      Action : Root_Colony_Action'Class)
     with Post => Colony.Has_Actions;

   procedure Delete_First_Action (Colony : Colony_Handle)
     with Pre => Colony.Has_Actions;

   function Create
     (World     : Athena.Handles.World.World_Handle;
      Owner     : Athena.Handles.Empire.Empire_Handle;
      Pop       : Non_Negative_Real := 0.0;
      Industry  : Non_Negative_Real := 0.0;
      Material  : Non_Negative_Real := 0.0)
      return Colony_Handle;

   procedure Iterate_All
     (Process : not null access
        procedure (Colony : Colony_Handle));

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Colony_Owner_Changed
     (Colony    : Colony_Handle;
      Old_Owner : Athena.Handles.Empire.Empire_Handle;
      New_Owner : Athena.Handles.Empire.Empire_Handle);

private

   type Colony_Handle is
     new Root_Athena_Handle
     and Athena.Updates.Update_Interface
     and Athena.Handles.Facility.Production_Context
     and Athena.Handles.Commodity.Stock_Interface with
      record
         Reference : Colony_Reference := 0;
      end record;

   overriding procedure Activate
     (Colony : Colony_Handle);

   overriding function Short_Name
     (Colony : Colony_Handle)
      return String
   is (Colony.Owner.Adjective & " colony on " & Colony.World.Name);

   overriding function Habitability
     (Colony : Colony_Handle)
      return Unit_Real
   is (Colony.World.Habitability);

   overriding function Available_Resources
     (Colony : Colony_Handle)
      return Athena.Handles.Commodity.Commodity_Array;

   overriding function Extract_Resource
     (Colony : Colony_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle;
      Size     : Non_Negative_Real)
      return Non_Negative_Real;

   function Reference (Colony : Colony_Handle) return Colony_Reference
   is (Colony.Reference);

   function Get (Colony : Colony_Reference) return Colony_Handle
   is (Colony /= 0, Colony);

   function Empty_Handle return Colony_Handle
   is (False, 0);

   type Root_Colony_Action is abstract tagged
      record
         null;
      end record;

end Athena.Handles.Colony;
