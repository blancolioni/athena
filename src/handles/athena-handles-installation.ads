with Ada.Streams.Stream_IO;

with Athena.Handles.Commodity;
with Athena.Handles.Facility;

package Athena.Handles.Installation is

   type Installation_Handle is
     new Root_Athena_Handle
     and Athena.Handles.Commodity.Stock_Interface
   with private;

   function Reference
     (Installation : Installation_Handle)
      return Installation_Reference;

   function Get
     (Installation : Installation_Reference)
      return Installation_Handle;

   function Empty_Handle return Installation_Handle;

   function Facility
     (Installation : Installation_Handle)
      return Athena.Handles.Facility.Facility_Handle;

   function Size
     (Installation : Installation_Handle)
      return Non_Negative_Real;

   function Create
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Size     : Non_Negative_Real)
      return Installation_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Installation_Handle is
     new Root_Athena_Handle
     and Athena.Handles.Commodity.Stock_Interface with
      record
         Reference : Installation_Reference := 0;
      end record;

   overriding function Get_Stock
     (Installation : Installation_Handle;
      Commodity    : Athena.Handles.Commodity.Commodity_Handle'Class)
      return Non_Negative_Real;

   overriding procedure Set_Stock
     (Installation : Installation_Handle;
      Commodity    : Athena.Handles.Commodity.Commodity_Handle'Class;
      Quantity     : Non_Negative_Real);

   overriding function Short_Name
     (Handle : Installation_Handle)
      return String
   is (Handle.Facility.Short_Name & " size=" & Image (Handle.Size));

   function Reference
     (Installation : Installation_Handle)
      return Installation_Reference
   is (Installation.Reference);

   function Get (Installation : Installation_Reference) return Installation_Handle
   is (Installation /= 0, Installation);

   function Empty_Handle return Installation_Handle
   is (False, 0);

end Athena.Handles.Installation;
