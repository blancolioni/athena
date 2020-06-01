with Ada.Streams.Stream_IO;

with Athena.Handles.Commodity;
with Athena.Handles.Star;

package Athena.Handles.Facility is

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
      return Athena.Handles.Commodity.Commodity_Handle;

   function Employment (Handle : Facility_Handle) return Non_Negative_Real;

   procedure Daily_Facility
     (Handle : Facility_Handle;
      Size   : Non_Negative_Real;
      Star   : Athena.Handles.Star.Star_Handle'Class;
      Stock  : Athena.Handles.Commodity.Stock_Interface'Class);

   function Get_By_Tag
     (Tag : String)
      return Facility_Handle;

   function Create
     (Tag       : String;
      Commodity : Athena.Handles.Commodity.Commodity_Handle;
      Quantity  : Non_Negative_Real;
      Employees : Non_Negative_Real)
      return Facility_Handle;

   procedure Add_Constant_Constraint
     (Handle : Facility_Handle;
      Value  : Non_Negative_Real);

   procedure Add_Habitability_Constraint
     (Handle : Facility_Handle);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   type Facility_Array is array (Positive range <>) of Facility_Handle;

   function All_Facility return Facility_Array;

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

end Athena.Handles.Facility;
