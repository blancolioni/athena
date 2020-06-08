with Ada.Streams.Stream_IO;

with Athena.Handles.Commodity;

package Athena.Handles.Facility is

   type Production_Context is interface;

   function Habitability
     (Context : Production_Context) return Unit_Real
      is abstract;

   function Available_Resources
     (Context  : Production_Context)
      return Athena.Handles.Commodity.Commodity_Array
      is abstract;

   function Extract_Resource
     (Context : Production_Context;
      Resource : Athena.Handles.Commodity.Commodity_Handle;
      Size     : Non_Negative_Real)
      return Non_Negative_Real
      is abstract;

   type Facility_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference (Handle : Facility_Handle) return Facility_Reference;
   function Get (Reference : Facility_Reference) return Facility_Handle;
   function Empty_Handle return Facility_Handle;

   function Output_Commodity
     (Handle : Facility_Handle)
      return Athena.Handles.Commodity.Commodity_Handle;

   function Output_Quantity
     (Handle : Facility_Handle)
      return Non_Negative_Real;

   function Employment (Handle : Facility_Handle) return Non_Negative_Real;

   function Required_Quantity
     (Handle    : Facility_Handle;
      Size      : Non_Negative_Real;
      Commodity : Athena.Handles.Commodity.Commodity_Handle)
      return Non_Negative_Real;

   procedure Daily_Production
     (Handle    : Facility_Handle;
      Id        : Object_Identifier;
      Size      : Non_Negative_Real;
      Context   : Production_Context'Class;
      Employees : Non_Negative_Real;
      Stock     : Athena.Handles.Commodity.Stock_Interface'Class);

   function Exists (Tag : String) return Boolean;

   function Get_By_Tag
     (Tag : String)
      return Facility_Handle
     with Pre => Exists (Tag);

   function Create
     (Tag       : String;
      Commodity : Athena.Handles.Commodity.Commodity_Handle;
      Quantity  : Non_Negative_Real;
      Employees : Non_Negative_Real)
      return Facility_Handle;

   procedure Add_Constant_Constraint
     (Handle : Facility_Handle;
      Value  : Non_Negative_Real);

   procedure Add_Agriculture_Constraint
     (Handle : Facility_Handle);

   procedure Add_Habitability_Constraint
     (Handle : Facility_Handle);

   procedure Add_Input
     (Handle    : Facility_Handle;
      Commodity : Athena.Handles.Commodity.Commodity_Handle'Class;
      Quantity  : Non_Negative_Real);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   type Facility_Array is array (Positive range <>) of Facility_Handle;

   function All_Facilities return Facility_Array;

private

   type Facility_Handle is
     new Root_Athena_Handle
     and Localised_Interface with
      record
         Reference : Facility_Reference := 0;
      end record;

   overriding function Tag
     (Facility : Facility_Handle)
      return String;

   overriding function Short_Name
     (Facility : Facility_Handle)
      return String
   is (Facility.Tag);

   function Reference (Handle : Facility_Handle) return Facility_Reference
   is (Handle.Reference);

   function Get (Reference : Facility_Reference) return Facility_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Facility_Handle
   is (False, 0);

   type Zone_Type is
     (Agricultural, Mining, Industrial, Commercial,
      Offices, Government, Urban);

end Athena.Handles.Facility;
