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

   function Empty_Mass
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference).Element.Mass);

   function Price
     (Component : Component_Handle)
      return Athena.Money.Price_Type
   is (Vector.Constant_Reference (Component.Reference).Element.Price);

   function Fuel_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference)
       .Element.Fuel_Consumption);

   function Active_Power_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference)
       .Element.Active_Power);

   function Idle_Power_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Constant_Reference (Component.Reference)
       .Element.Idle_Power);

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

   function Has_Power_Output
     (Component : Component_Handle)
      return Boolean
   is (Vector (Component.Reference).Power_Output > 0.0);

   function Power_Output
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector (Component.Reference).Power_Output);

   function Has_Berths
     (Component : Component_Handle)
      return Boolean
   is (Vector.Element (Component.Reference).Berths > 0.0);

   function Berths
     (Component : Component_Handle)
      return Non_Negative_Real
   is (Vector.Element (Component.Reference).Berths);

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

   ---------------------------
   -- Empty_Component_Array --
   ---------------------------

   function Empty_Component_Array return Component_Array is
   begin
      return Result : Component_Array (1 .. 0);
   end Empty_Component_Array;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Component_Handle is
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
