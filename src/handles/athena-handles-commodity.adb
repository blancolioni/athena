with Ada.Containers.Doubly_Linked_Lists;
with WL.String_Maps;

with Athena.Random;

package body Athena.Handles.Commodity is

   package Resource_Constraint_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Resource_Constraint);

   type Commodity_Record is
      record
         Identifier  : Object_Identifier;
         Tag         : Ada.Strings.Unbounded.Unbounded_String;
         Class       : Commodity_Class;
         Is_Abstract : Boolean;
         Tonnage     : Non_Negative_Real;
         Constraints : Resource_Constraint_Lists.List;
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

   function Deposit_Constraint
     (Handle : Commodity_Handle)
      return Resource_Constraint
   is (Vector (Handle.Reference).Constraints.First_Element);

   Food_Ref  : Commodity_Reference := Null_Commodity_Reference;
   Water_Ref : Commodity_Reference := Null_Commodity_Reference;
   Power_Ref : Commodity_Reference := Null_Commodity_Reference;

   function Food return Commodity_Handle
   is (Get (Food_Ref));

   function Power return Commodity_Handle
   is (Get (Power_Ref));

   function Water return Commodity_Handle
   is (Get (Water_Ref));

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Deposit_Constraint
     (Handle     : Commodity_Handle;
      Constraint : Resource_Constraint)
   is
   begin
      Vector (Handle.Reference).Constraints.Append (Constraint);
   end Add_Deposit_Constraint;

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
      Tonnage     : Non_Negative_Real)
      return Commodity_Handle
   is
   begin
      Vector.Append
        (Commodity_Record'
           (Identifier  => Next_Identifier,
            Tag         => +Tag,
            Tonnage     => Tonnage,
            Class       => Class,
            Is_Abstract => Is_Abstract,
            Constraints => <>));

      Map.Insert (Tag, Vector.Last_Index);

      case Class is
         when Food =>
            Food_Ref := Vector.Last_Index;
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

   -------------
   -- Execute --
   -------------

   function Execute
     (Constraint : Resource_Constraint)
      return Non_Negative_Real
   is
   begin
      return Q : Non_Negative_Real := 1.0 do
         for Item of Constraint.List loop
            case Item.Class is
               when Frequency_Constraint =>
                  Q := Q
                    * (Athena.Random.Normal_Random (Item.Standard_Deviation)
                       + Item.Mean);
            end case;
         end loop;
      end return;
   end Execute;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   --------------------------
   -- Frequency_Constraint --
   --------------------------

   function Frequency_Constraint
     (Mean               : Unit_Real;
      Standard_Deviation : Unit_Real)
      return Resource_Constraint
   is
      List : Constraint_Lists.List;
   begin
      List.Append ((Frequency_Constraint, Mean, Standard_Deviation));
      return Resource_Constraint'
        (List => List);
   end Frequency_Constraint;

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

end Athena.Handles.Commodity;
