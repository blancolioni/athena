with Ada.Containers.Vectors;
with WL.String_Maps;

package body Athena.Handles.Commodity is

   type Commodity_Record is
      record
         Identifier : Object_Identifier;
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Tonnage    : Non_Negative_Real;
         Is_Food    : Boolean;
         Is_Water   : Boolean;
      end record;

   package Commodity_Vectors is
     new Ada.Containers.Vectors
       (Real_Commodity_Reference, Commodity_Record);

   package Commodity_Maps is
     new WL.String_Maps (Real_Commodity_Reference);

   Vector : Commodity_Vectors.Vector;
   Map    : Commodity_Maps.Map;

   Food_Ref : Commodity_Reference := Null_Commodity_Reference;
   Water_Ref : Commodity_Reference := Null_Commodity_Reference;

   function Food return Commodity_Handle
   is (Get (Food_Ref));

   function Water return Commodity_Handle
   is (Get (Water_Ref));

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (Stock     : Stock_Interface'Class;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      Stock.Set_Stock (Commodity, Stock.Get_Stock (Commodity) + Quantity);
   end Add_Stock;

   ---------------------
   -- All_Commodities --
   ---------------------

   function All_Commodities return Commodity_Array is
   begin
      return Result : Commodity_Array (1 .. Natural (Vector.Last_Index)) do
         for I in Result'Range loop
            Result (I) := Get (Commodity_Reference (I));
         end loop;
      end return;
   end All_Commodities;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag      : String;
      Tonnage  : Non_Negative_Real;
      Is_Food  : Boolean;
      Is_Water : Boolean)
   is
   begin
      Vector.Append
        (Commodity_Record'
           (Identifier => Next_Identifier,
            Tag        => +Tag,
            Tonnage    => Tonnage,
            Is_Food    => Is_Food,
            Is_Water   => Is_Water));
      Map.Insert (Tag, Vector.Last_Index);
      if Is_Food then
         Food_Ref := Vector.Last_Index;
      elsif Is_Water then
         Water_Ref := Vector.Last_Index;
      end if;
   end Create;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Commodity_Handle is
   begin
      pragma Assert (Map.Contains (Tag), "unknown commodity tag: " & Tag);
      return Get (Map.Element (Tag));
   end Get_By_Tag;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Commodity_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Tag), I);
         if Vector (I).Is_Food then
            Food_Ref := I;
         elsif Vector (I).Is_Water then
            Water_Ref := I;
         end if;
      end loop;
   end Load;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (Stock     : Stock_Interface'Class;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      Stock.Set_Stock (Commodity, Stock.Get_Stock (Commodity) - Quantity);
   end Remove_Stock;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Commodity_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Commodity : Commodity_Handle) return String is
   begin
      return -(Vector (Commodity.Reference).Tag);
   end Tag;

end Athena.Handles.Commodity;
