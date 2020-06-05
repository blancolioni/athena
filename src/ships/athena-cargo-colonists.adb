package body Athena.Cargo.Colonists is

   type Colonist_Cargo_Type is
     new Cargo_Interface with
      record
         Empire : Athena.Handles.Empire_Reference;
      end record;

   overriding function Tag
     (Cargo : Colonist_Cargo_Type)
      return String
   is (Athena.Handles.Empire.Get (Cargo.Empire).Plural);

   overriding function Tonnage
     (Cargo : Colonist_Cargo_Type)
      return Non_Negative_Real
   is (1.0);

   overriding function Mass
     (Cargo : Colonist_Cargo_Type)
      return Non_Negative_Real
   is (0.2);

   overriding function Category
     (Cargo : Colonist_Cargo_Type)
      return Cargo_Category
   is (People);

   --------------------
   -- Colonist_Cargo --
   --------------------

   function Colonist_Cargo
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Cargo_Interface'Class
   is
   begin
      return Colonist_Cargo_Type'
        (Empire => Empire.Reference);
   end Colonist_Cargo;

   ----------------
   -- Get_Empire --
   ----------------

   function Get_Empire
     (Cargo : Cargo_Interface'Class) return Athena.Handles.Empire.Empire_Handle
   is
   begin
      return Athena.Handles.Empire.Get (Colonist_Cargo_Type (Cargo).Empire);
   end Get_Empire;

   -----------------
   -- Is_Colonist --
   -----------------

   function Is_Colonist (Cargo : Cargo_Interface'Class) return Boolean is
   begin
      return Cargo in Colonist_Cargo_Type'Class;
   end Is_Colonist;

end Athena.Cargo.Colonists;
