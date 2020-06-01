with Ada.Streams.Stream_IO;

with Athena.Handles.Commodity;
with Athena.Handles.Star;

package Athena.Handles.Production is

   type Production_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference (Handle : Production_Handle) return Production_Reference;
   function Get (Reference : Production_Reference) return Production_Handle;
   function Empty_Handle return Production_Handle;

   function Commodity
     (Handle : Production_Handle)
      return Athena.Handles.Commodity.Commodity_Handle;

   function Employment (Handle : Production_Handle) return Non_Negative_Real;

   procedure Daily_Production
     (Handle : Production_Handle;
      Size   : Non_Negative_Real;
      Star   : Athena.Handles.Star.Star_Handle'Class;
      Stock  : Athena.Handles.Commodity.Stock_Interface'Class);

   function Get_By_Tag
     (Tag : String)
      return Production_Handle;

   function Create
     (Tag        : String;
      Commodity  : Athena.Handles.Commodity.Commodity_Handle;
      Employment : Non_Negative_Real)
      return Production_Handle;

   procedure Add_Constant_Constraint
     (Handle : Production_Handle;
      Value  : Non_Negative_Real);

   procedure Add_Habitability_Constraint
     (Handle : Production_Handle);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   type Production_Array is array (Positive range <>) of Production_Handle;

   function All_Production return Production_Array;

private

   type Production_Handle is
     new Root_Athena_Handle
     and Localised_Interface with
      record
         Reference : Production_Reference := 0;
      end record;

   overriding function Tag
     (Production : Production_Handle)
      return String;

   overriding function Short_Name
     (Production : Production_Handle)
      return String
   is (Production.Tag);

   function Reference (Handle : Production_Handle) return Production_Reference
   is (Handle.Reference);

   function Get (Reference : Production_Reference) return Production_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Production_Handle
   is (False, 0);

end Athena.Handles.Production;
