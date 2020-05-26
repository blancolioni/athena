with Ada.Streams.Stream_IO;

package Athena.Handles.Hull_Armor is

   type Hull_Armor_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference (Armor : Hull_Armor_Handle) return Hull_Armor_Reference;
   function Get (Armor : Hull_Armor_Reference) return Hull_Armor_Handle;

   function Tonnage_Fraction
     (Armor : Hull_Armor_Handle)
      return Unit_Real
     with Pre => Armor.Has_Element;

   function Price_Fraction
     (Armor : Hull_Armor_Handle)
      return Unit_Real
     with Pre => Armor.Has_Element;

   function Get_By_Tag
     (Tag : String)
      return Hull_Armor_Handle;

   procedure Create
     (Tag              : String;
      Tonnage_Fraction : Unit_Real;
      Price_Fraction   : Unit_Real);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Hull_Armor_Handle is
     new Root_Athena_Handle
     and Localised_Interface with
      record
         Reference : Hull_Armor_Reference := 0;
      end record;

   overriding function Tag
     (Armor : Hull_Armor_Handle)
      return String;

   overriding function Short_Name
     (Armor : Hull_Armor_Handle)
      return String
   is (Armor.Tag);

   function Reference (Armor : Hull_Armor_Handle) return Hull_Armor_Reference
   is (Armor.Reference);

   function Get (Armor : Hull_Armor_Reference) return Hull_Armor_Handle
   is (Armor /= 0, Armor);

end Athena.Handles.Hull_Armor;
