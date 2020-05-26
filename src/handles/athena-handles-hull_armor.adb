with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Athena.Handles.Hull_Armor is

   type Hull_Armor_Record is
      record
         Identifier       : Object_Identifier;
         Tag              : Ada.Strings.Unbounded.Unbounded_String;
         Tonnage_Fraction : Unit_Real;
         Price_Fraction   : Unit_Real;
      end record;

   package Hull_Armor_Vectors is
     new Ada.Containers.Vectors
       (Real_Hull_Armor_Reference, Hull_Armor_Record);

   package Hull_Armor_Maps is
     new WL.String_Maps (Real_Hull_Armor_Reference);

   Vector : Hull_Armor_Vectors.Vector;
   Map    : Hull_Armor_Maps.Map;

   function Tonnage_Fraction
     (Armor : Hull_Armor_Handle)
      return Unit_Real
   is (Vector (Armor.Reference).Tonnage_Fraction);

   function Price_Fraction
     (Armor : Hull_Armor_Handle)
      return Unit_Real
   is (Vector (Armor.Reference).Price_Fraction);

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag              : String;
      Tonnage_Fraction : Unit_Real;
      Price_Fraction   : Unit_Real)
   is
   begin
      Vector.Append
        (Hull_Armor_Record'
           (Identifier    => Next_Identifier,
            Tag           => +Tag,
            Tonnage_Fraction => Tonnage_Fraction,
            Price_Fraction   => Price_Fraction));
      Map.Insert (Tag, Vector.Last_Index);
   end Create;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Hull_Armor_Handle is
      use Hull_Armor_Maps;
      Position : constant Cursor := Map.Find (Tag);
   begin
      return Handle : Hull_Armor_Handle do
         if Has_Element (Position) then
            Handle.Reference := Element (Position);
            Handle.Has_Element := True;
         end if;
      end return;
   end Get_By_Tag;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Hull_Armor_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Tag), I);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Hull_Armor_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Armor : Hull_Armor_Handle) return String is
   begin
      return -(Vector (Armor.Reference).Tag);
   end Tag;

end Athena.Handles.Hull_Armor;
