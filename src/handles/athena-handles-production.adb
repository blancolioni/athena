with Ada.Containers.Vectors;
with WL.String_Maps;

package body Athena.Handles.Production is

   type Production_Record is
      record
         Identifier   : Object_Identifier;
         Tag          : Ada.Strings.Unbounded.Unbounded_String;
         Commodity    : Commodity_Reference;
         Employment   : Non_Negative_Real;
         Factor       : Non_Negative_Real;
         Habitability : Boolean;
      end record;

   package Production_Vectors is
     new Ada.Containers.Vectors
       (Real_Production_Reference, Production_Record);

   package Production_Maps is
     new WL.String_Maps (Real_Production_Reference);

   Vector : Production_Vectors.Vector;
   Map    : Production_Maps.Map;

   function Commodity
     (Handle : Production_Handle)
      return Athena.Handles.Commodity.Commodity_Handle
   is (Athena.Handles.Commodity.Get (Vector (Handle.Reference).Commodity));

   function Employment (Handle : Production_Handle) return Non_Negative_Real
   is (Vector (Handle.Reference).Employment);

   -----------------------------
   -- Add_Constant_Constraint --
   -----------------------------

   procedure Add_Constant_Constraint
     (Handle : Production_Handle;
      Value  : Non_Negative_Real)
   is
   begin
      Vector (Handle.Reference).Factor := Value;
   end Add_Constant_Constraint;

   ---------------------------------
   -- Add_Habitability_Constraint --
   ---------------------------------

   procedure Add_Habitability_Constraint
     (Handle : Production_Handle)
   is
   begin
      Vector (Handle.Reference).Habitability := True;
   end Add_Habitability_Constraint;

   --------------------
   -- All_Production --
   --------------------

   function All_Production return Production_Array is
   begin
      return Result : Production_Array (1 .. Natural (Vector.Last_Index)) do
         for I in Result'Range loop
            Result (I) := Get (Production_Reference (I));
         end loop;
      end return;
   end All_Production;

   ------------
   -- Create --
   ------------

   function Create
     (Tag        : String;
      Commodity  : Athena.Handles.Commodity.Commodity_Handle;
      Employment : Non_Negative_Real)
      return Production_Handle
   is
   begin
      Vector.Append
        (Production_Record'
           (Identifier   => Next_Identifier,
            Tag          => +Tag,
            Commodity    => Commodity.Reference,
            Employment   => Employment,
            Factor       => 1.0,
            Habitability => False));
      Map.Insert (Tag, Vector.Last_Index);
      return (True, Vector.Last_Index);
   end Create;

   ----------------------
   -- Daily_Production --
   ----------------------

   procedure Daily_Production
     (Handle : Production_Handle;
      Size   : Non_Negative_Real;
      Star   : Athena.Handles.Star.Star_Handle'Class;
      Stock  : Athena.Handles.Commodity.Stock_Interface'Class)
   is
      Const : constant Non_Negative_Real :=
                Vector (Handle.Reference).Factor;
      Hab    : constant Unit_Real :=
                 (if Vector (Handle.Reference).Habitability
                  then Star.Habitability
                  else 1.0);
      Factor : constant Non_Negative_Real := Const * Hab;
   begin
      Handle.Log
        ("size " & Image (Size) & "; factor " & Image (Factor)
         & "; new " & Handle.Commodity.Tag & " "
         & Image (Size * Factor));
      Stock.Add_Stock (Handle.Commodity, Size * Factor);
   end Daily_Production;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Production_Handle is
   begin
      pragma Assert (Map.Contains (Tag), "unknown production tag: " & Tag);
      return Get (Map.Element (Tag));
   end Get_By_Tag;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Production_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Tag), I);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Production_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Production : Production_Handle) return String is
   begin
      return -(Vector (Production.Reference).Tag);
   end Tag;

end Athena.Handles.Production;
