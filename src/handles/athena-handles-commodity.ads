private with Ada.Containers.Vectors;

with Ada.Streams.Stream_IO;

with Athena.Resources;

package Athena.Handles.Commodity is

   type Commodity_Class is
     (Food, Fuel, Manufactured, Power, Resource, Water);

   type Commodity_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference (Handle : Commodity_Handle) return Commodity_Reference;
   function Get (Reference : Commodity_Reference) return Commodity_Handle;
   function Empty_Handle return Commodity_Handle;

   function Class
     (Handle : Commodity_Handle)
      return Commodity_Class;

   function Is_Abstract
     (Handle : Commodity_Handle)
      return Boolean;

   function Density
     (Handle : Commodity_Handle)
      return Non_Negative_Real;

   function Exists
     (Tag : String)
      return Boolean;

   function Get_By_Tag
     (Tag : String)
      return Commodity_Handle
     with Pre => Exists (Tag);

   function Food return Commodity_Handle;
   function Fuel return Commodity_Handle;
   function Power return Commodity_Handle;
   function Water return Commodity_Handle;

   function Create
     (Tag         : String;
      Class       : Commodity_Class;
      Is_Abstract : Boolean;
      Density     : Non_Negative_Real)
      return Commodity_Handle;

   procedure Add_Resource_Frequency
     (Handle     : Commodity_Handle;
      Frequency  : Athena.Resources.Resource_Frequency);

   function Generator
     (Handle : Commodity_Handle)
      return Athena.Resources.Resource_Generator;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   type Commodity_Array is array (Positive range <>) of Commodity_Handle;

   function All_Commodities return Commodity_Array;
   function Resource_Commodities return Commodity_Array;

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

   procedure Clear_Stock
     (Stock     : Stock_Interface'Class);

   procedure Add_Stock
     (Stock     : Stock_Interface'Class;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real);

   procedure Remove_Stock
     (Stock     : Stock_Interface'Class;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
     with Pre => Quantity <= Stock.Get_Stock (Commodity);

   procedure Transfer
     (From      : Stock_Interface'Class;
      To        : Stock_Interface'Class;
      Commodity : Commodity_Handle);

   type Stock_Type is private;

   function Get_Stock
     (Stock     : Stock_Type;
      Commodity : Commodity_Handle'Class)
      return Non_Negative_Real;

   procedure Set_Stock
     (Stock     : in out Stock_Type;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real);

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

   package Stock_Vectors is
     new Ada.Containers.Vectors (Real_Commodity_Reference, Non_Negative_Real);

   type Stock_Type is
      record
         Vector : Stock_Vectors.Vector;
      end record;

   function Get_Stock
     (Stock     : Stock_Type;
      Commodity : Commodity_Handle'Class)
      return Non_Negative_Real
   is (if Stock.Vector.Is_Empty then 0.0
       else Stock.Vector.Element (Commodity.Reference));

end Athena.Handles.Commodity;
