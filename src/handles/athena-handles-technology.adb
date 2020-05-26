with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Athena.Handles.Technology is

   type Technology_Record is
      record
         Identifier    : Object_Identifier;
         Tag           : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Technology_Vectors is
     new Ada.Containers.Vectors
       (Real_Technology_Reference, Technology_Record);

   package Technology_Maps is
     new WL.String_Maps (Real_Technology_Reference);

   Vector : Technology_Vectors.Vector;
   Map    : Technology_Maps.Map;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Technology_Handle is
      use Technology_Maps;
      Position : constant Cursor := Map.Find (Tag);
   begin
      return Handle : Technology_Handle do
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
      Technology_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Tag), I);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Technology_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Technology : Technology_Handle) return String is
   begin
      return -(Vector (Technology.Reference).Tag);
   end Tag;

end Athena.Handles.Technology;
