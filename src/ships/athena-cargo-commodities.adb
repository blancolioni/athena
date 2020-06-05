package body Athena.Cargo.Commodities is

   type Commodity_Cargo_Type is
     new Cargo_Interface with
      record
         Reference : Athena.Handles.Commodity_Reference;
      end record;

   overriding function Tag
     (Cargo : Commodity_Cargo_Type)
      return String
   is (Athena.Handles.Commodity.Get (Cargo.Reference).Tag);

   overriding function Tonnage
     (Cargo : Commodity_Cargo_Type)
      return Non_Negative_Real
   is (1.0);

   overriding function Mass
     (Cargo : Commodity_Cargo_Type)
      return Non_Negative_Real
   is (Athena.Handles.Commodity.Get (Cargo.Reference).Density);

   overriding function Category
     (Cargo : Commodity_Cargo_Type)
      return Athena.Cargo.Cargo_Category;

   --------------
   -- Category --
   --------------

   overriding function Category
     (Cargo : Commodity_Cargo_Type)
      return Athena.Cargo.Cargo_Category
   is
      use Athena.Handles.Commodity;
      Handle : constant Commodity_Handle :=
                 Athena.Handles.Commodity.Get (Cargo.Reference);
   begin
      case Handle.Class is
         when Food =>
            return Commodity;
         when Fuel =>
            return Fuel;
         when Manufactured =>
            return Commodity;
         when Power =>
            return Commodity;
         when Resource =>
            return Commodity;
         when Water =>
            return Commodity;
      end case;
   end Category;

   ---------------------
   -- Commodity_Cargo --
   ---------------------

   function Commodity_Cargo
     (Commodity : Athena.Handles.Commodity.Commodity_Handle)
      return Cargo_Interface'Class
   is
   begin
      return Commodity_Cargo_Type'
        (Reference => Commodity.Reference);
   end Commodity_Cargo;

   -------------------
   -- Get_Commodity --
   -------------------

   function Get_Commodity
     (Cargo : Cargo_Interface'Class)
      return Athena.Handles.Commodity.Commodity_Handle
   is
   begin
      return Athena.Handles.Commodity.Get
        (Commodity_Cargo_Type (Cargo).Reference);
   end Get_Commodity;

   -------------------
   -- Has_Commodity --
   -------------------

   function Has_Commodity (Cargo : Cargo_Interface'Class) return Boolean is
   begin
      return Cargo in Commodity_Cargo_Type'Class;
   end Has_Commodity;

end Athena.Cargo.Commodities;
