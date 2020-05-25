with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Athena.Handles.Hull is

   type Hull_Record is
      record
         Identifier    : Object_Identifier;
         Tag           : Ada.Strings.Unbounded.Unbounded_String;
         Streamlined   : Boolean;
         Hull_Points   : Non_Negative_Real;
         Cost          : Non_Negative_Real;
         Comfort       : Non_Negative_Real;
         Armor_Tonnage : Non_Negative_Real;
      end record;

   package Hull_Vectors is
     new Ada.Containers.Vectors
       (Real_Hull_Reference, Hull_Record);

   package Hull_Maps is
     new WL.String_Maps (Real_Hull_Reference);

   Vector : Hull_Vectors.Vector;
   Map    : Hull_Maps.Map;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag           : String;
      Streamlined   : Boolean;
      Hull_Points   : Non_Negative_Real;
      Cost          : Non_Negative_Real;
      Comfort       : Non_Negative_Real;
      Armor_Tonnage : Non_Negative_Real)
   is
   begin
      Vector.Append
        (Hull_Record'
           (Identifier    => Next_Identifier,
            Tag           => +Tag,
            Streamlined   => Streamlined,
            Hull_Points   => Hull_Points,
            Cost          => Cost,
            Comfort       => Comfort,
            Armor_Tonnage => Armor_Tonnage));
   end Create;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Hull_Handle is
      use Hull_Maps;
      Position : constant Cursor := Map.Find (Tag);
   begin
      return Handle : Hull_Handle do
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
      Hull_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Tag), I);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Hull_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Hull : Hull_Handle) return String is
   begin
      return -(Vector (Hull.Reference).Tag);
   end Tag;

end Athena.Handles.Hull;
