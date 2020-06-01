with Ada.Streams.Stream_IO;

package Athena.Handles.Commodity is

   type Commodity_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference (Handle : Commodity_Handle) return Commodity_Reference;
   function Get (Reference : Commodity_Reference) return Commodity_Handle;
   function Empty_Handle return Commodity_Handle;

   function Get_By_Tag
     (Tag : String)
      return Commodity_Handle;

   function Food return Commodity_Handle;
   function Water return Commodity_Handle;

   procedure Create
     (Tag      : String;
      Tonnage  : Non_Negative_Real;
      Is_Food  : Boolean;
      Is_Water : Boolean);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   type Commodity_Array is array (Positive range <>) of Commodity_Handle;

   function All_Commodities return Commodity_Array;

   type Stock_Interface is interface;

   function Get_Stock
     (Stock     : Stock_Interface;
      Commodity : Commodity_Handle'Class)
      return Non_Negative_Real
      is abstract;

   procedure Set_Stock
     (Stock     : Stock_Interface;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
   is abstract;

   procedure Add_Stock
     (Stock     : Stock_Interface'Class;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real);

   procedure Remove_Stock
     (Stock     : Stock_Interface'Class;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
     with Pre => Quantity <= Stock.Get_Stock (Commodity);

private

   type Commodity_Handle is
     new Root_Athena_Handle
     and Localised_Interface with
      record
         Reference : Commodity_Reference := 0;
      end record;

   overriding function Tag
     (Commodity : Commodity_Handle)
      return String;

   overriding function Short_Name
     (Commodity : Commodity_Handle)
      return String
   is (Commodity.Tag);

   function Reference (Handle : Commodity_Handle) return Commodity_Reference
   is (Handle.Reference);

   function Get (Reference : Commodity_Reference) return Commodity_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Commodity_Handle
   is (False, 0);

end Athena.Handles.Commodity;
