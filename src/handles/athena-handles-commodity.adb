with WL.String_Maps;

package body Athena.Handles.Commodity is

   type Commodity_Record is
      record
         Identifier  : Object_Identifier;
         Tag         : Ada.Strings.Unbounded.Unbounded_String;
         Class       : Commodity_Class;
         Is_Abstract : Boolean;
         Density     : Non_Negative_Real;
         Generator   : Athena.Resources.Resource_Generator;
      end record;

   package Commodity_Vectors is
     new Ada.Containers.Vectors
       (Real_Commodity_Reference, Commodity_Record);

   package Commodity_Maps is
     new WL.String_Maps (Real_Commodity_Reference);

   Vector : Commodity_Vectors.Vector;
   Map    : Commodity_Maps.Map;

   function Class
     (Handle : Commodity_Handle)
      return Commodity_Class
   is (Vector (Handle.Reference).Class);

   function Is_Abstract
     (Handle : Commodity_Handle)
      return Boolean
   is (Vector (Handle.Reference).Is_Abstract);

   function Density
     (Handle : Commodity_Handle)
      return Non_Negative_Real
   is (if Handle.Is_Abstract
       then 0.0 else Vector (Handle.Reference).Density);

   Food_Ref  : Commodity_Reference := Null_Commodity_Reference;
   Fuel_Ref  : Commodity_Reference := Null_Commodity_Reference;
   Power_Ref : Commodity_Reference := Null_Commodity_Reference;
   Water_Ref : Commodity_Reference := Null_Commodity_Reference;

   function Food return Commodity_Handle
   is (Get (Food_Ref));

   function Fuel return Commodity_Handle
   is (Get (Fuel_Ref));

   function Power return Commodity_Handle
   is (Get (Power_Ref));

   function Water return Commodity_Handle
   is (Get (Water_Ref));

   ----------------------------
   -- Add_Resource_Generator --
   ----------------------------

   procedure Add_Resource_Frequency
     (Handle     : Commodity_Handle;
      Frequency  : Athena.Resources.Resource_Frequency)
   is
   begin
      Athena.Resources.Add_Resource_Frequency
        (Generator => Vector (Handle.Reference).Generator,
         Frequency => Frequency);
   end Add_Resource_Frequency;

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

   -----------------
   -- Clear_Stock --
   -----------------

   procedure Clear_Stock
     (Stock     : Stock_Interface'Class)
   is
   begin
      for Commodity of All_Commodities loop
         Stock.Set_Stock (Commodity, 0.0);
      end loop;
   end Clear_Stock;

   ------------
   -- Create --
   ------------

   function Create
     (Tag         : String;
      Class       : Commodity_Class;
      Is_Abstract : Boolean;
      Density     : Non_Negative_Real)
      return Commodity_Handle
   is
   begin
      Vector.Append
        (Commodity_Record'
           (Identifier  => Next_Identifier,
            Tag         => +Tag,
            Density     => Density,
            Class       => Class,
            Is_Abstract => Is_Abstract,
            Generator   => <>));

      Map.Insert (Tag, Vector.Last_Index);

      case Class is
         when Food =>
            Food_Ref := Vector.Last_Index;
         when Fuel =>
            Fuel_Ref := Vector.Last_Index;
         when Water =>
            Water_Ref := Vector.Last_Index;
         when Power =>
            Power_Ref := Vector.Last_Index;
         when Resource =>
            null;
         when Manufactured =>
            null;
      end case;

      return (True, Vector.Last_Index);
   end Create;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   ---------------
   -- Generator --
   ---------------

   function Generator
     (Handle : Commodity_Handle)
      return Athena.Resources.Resource_Generator
   is
   begin
      return Vector (Handle.Reference).Generator;
   end Generator;

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
         case Vector (I).Class is
            when Food =>
               Food_Ref := I;
            when Fuel =>
               Fuel_Ref := I;
            when Water =>
               Water_Ref := I;
            when Power =>
               Power_Ref := I;
            when Resource =>
               null;
            when Manufactured =>
               null;
         end case;
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

   --------------------------
   -- Resource_Commodities --
   --------------------------

   function Resource_Commodities return Commodity_Array is
      Result : Commodity_Array (1 .. Natural (Vector.Last_Index));
      Count  : Natural := 0;
   begin
      for Ref in 1 .. Vector.Last_Index loop
         if Vector.Element (Ref).Class = Resource then
            Count := Count + 1;
            Result (Count) := Get (Ref);
         end if;
      end loop;
      return Result (1 .. Count);
   end Resource_Commodities;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Commodity_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------------
   -- Set_Stock --
   ---------------

   procedure Set_Stock
     (Stock     : in out Stock_Type;
      Commodity : Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      if Stock.Vector.Is_Empty then
         for Item of All_Commodities loop
            Stock.Vector.Append (0.0);
         end loop;
      end if;
      Stock.Vector (Commodity.Reference) := Quantity;
   end Set_Stock;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Commodity : Commodity_Handle) return String is
   begin
      return -(Vector (Commodity.Reference).Tag);
   end Tag;

   --------------
   -- Transfer --
   --------------

   procedure Transfer
     (From      : Stock_Interface'Class;
      To        : Stock_Interface'Class;
      Commodity : Commodity_Handle)
   is
   begin
      if Commodity.Is_Abstract then
         for Item of All_Commodities loop
            if not Item.Is_Abstract
              and then Item.Class = Commodity.Class
            then
               To.Add_Stock (Item, From.Get_Stock (Item));
               From.Set_Stock (Item, 0.0);
            end if;
         end loop;
      else
         To.Add_Stock (Commodity, From.Get_Stock (Commodity));
         From.Set_Stock (Commodity, 0.0);
      end if;
   end Transfer;

end Athena.Handles.Commodity;
