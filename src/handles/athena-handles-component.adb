with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

package body Athena.Handles.Component is

   package Component_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Real_Component_Reference, Root_Component_Record'Class);

   package Component_Maps is
     new WL.String_Maps (Real_Component_Reference);

   Vector : Component_Vectors.Vector;
   Map    : Component_Maps.Map;

   overriding function Tag
     (Component : Component_Handle)
      return String
   is (-(Vector.Element (Component.Reference).Tag));

   function Tonnage
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference).Element.Tonnage);

   function Price
     (Component : Component_Handle)
      return Athena.Money.Price_Type
   is (Vector.Constant_Reference (Component.Reference).Element.Price);

   function Fuel_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference)
       .Element.Fuel_Consumption);

   function Power_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference)
       .Element.Power_Consumption);

   function Has_Jump
     (Component : Component_Handle)
      return Boolean
   is (Vector (Component.Reference).Jump > 0.0);

   function Jump
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector (Component.Reference).Jump);

   function Has_Impulse
     (Component : Component_Handle)
      return Boolean
   is (Vector (Component.Reference).Impulse > 0.0);

   function Impulse
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector (Component.Reference).Impulse);

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (Component : Root_Component_Record'Class)
   is
   begin
      Vector.Append (Component);
      Map.Insert (-Component.Tag, Vector.Last_Index);
   end Add_Component;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Component_Handle'Class is
      use Component_Maps;
      Position : constant Cursor := Map.Find (Tag);
   begin
      return Handle : Component_Handle do
         if Has_Element (Position) then
            Handle.Reference := Element (Position);
            Handle.Has_Element := True;
         end if;
      end return;
   end Get_By_Tag;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Component : Component_Handle)
      return Object_Identifier
   is
      Rec : Root_Component_Record'Class renames Vector (Component.Reference);
   begin
      return Rec.Identifier;
   end Identifier;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Component_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector.Element (I).Tag), I);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Component_Vectors.Vector'Write (Stream, Vector);
   end Save;

end Athena.Handles.Component;
