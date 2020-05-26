with Ada.Strings.Fixed;

with Athena.Color.Table;

package body Athena.Color is

   function Parse_Hex_Color (Spec : String) return Athena_Color;

   function Filter (Item : String;
                    Valid : String)
                    return String;

   ------------
   -- Filter --
   ------------

   function Filter (Item  : String;
                    Valid : String)
                    return String
   is
      Result : String := Item;
      Index  : Natural := Result'First - 1;
   begin
      for Ch of Item loop
         if (for some X of Valid => Ch = X) then
            Index := Index + 1;
            Result (Index) := Ch;
         end if;
      end loop;
      return Result (Result'First .. Index);
   end Filter;

   -----------------
   -- From_String --
   -----------------

   function From_String
     (Item : String)
      return Athena_Color
   is
   begin
      if Item = "" then
         return Black;
      elsif Item (Item'First) = '#' then
         return Parse_Hex_Color (Item (Item'First + 1 .. Item'Last));
      elsif Athena.Color.Table.Exists (Item) then
         return Athena.Color.Table.Get (Item);
      else
         return White;
      end if;
   end From_String;

   function Parse_Hex_Color (Spec : String) return Athena_Color is
      Valid_Spec : constant String :=
                     Filter (Spec, "0123456789ABCDEFabcdef");

      function To_Digit (Ch : Character) return Natural
      is (if Ch in '0' .. '9'
          then Character'Pos (Ch) - 48
          elsif Ch in 'A' .. 'F'
          then Character'Pos (Ch) - Character'Pos ('A') + 10
          elsif Ch in 'a' .. 'f'
          then Character'Pos (Ch) - Character'Pos ('a') + 10
          else raise Constraint_Error with "bad character after filtering");

      function To_Color_Element
        (Value : Natural;
         Max   : Natural)
         return Unit_Real
      is (Real (Value) / Real (Max))
        with Pre => Value <= Max;

      function To_Natural (Hex : String) return Natural;

      ----------------
      -- To_Natural --
      ----------------

      function To_Natural (Hex : String) return Natural is
         X : Natural := 0;
      begin
         for Ch of Hex loop
            X := X * 16 + To_Digit (Ch);
         end loop;
         return X;
      end To_Natural;

   begin
      if Valid_Spec'Length = 0 then
         return (1.0, 0.0, 1.0, 1.0);
      elsif Valid_Spec'Length = 1 then
         declare
            E : constant Unit_Real :=
                  To_Color_Element (To_Natural (Valid_Spec),
                                    15);
         begin
            return (E, E, E, 1.0);
         end;
      elsif Valid_Spec'Length = 2 then
         declare
            E : constant Unit_Real :=
                  To_Color_Element (To_Natural (Valid_Spec),
                                    255);
         begin
            return (E, E, E, 1.0);
         end;
      else
         declare
            Element_Length : constant Positive := Valid_Spec'Length / 3;
            R1             : constant Positive :=
                               Valid_Spec'First;
            R2             : constant Positive :=
                               R1 + Element_Length - 1;
            G1             : constant Positive := R2 + 1;
            G2             : constant Positive := G1 + Element_Length - 1;
            B1             : constant Positive := G2 + 1;
            B2             : constant Positive := B1 + Element_Length - 1;
            Max            : constant Positive :=
                               16 ** Element_Length - 1;
            R              : constant Natural :=
                               To_Natural (Valid_Spec (R1 .. R2));
            G              : constant Natural :=
                               To_Natural (Valid_Spec (G1 .. G2));
            B              : constant Natural :=
                               To_Natural (Valid_Spec (B1 .. B2));
         begin
            return Athena_Color'
              (Red   => To_Color_Element (R, Max),
               Green => To_Color_Element (G, Max),
               Blue  => To_Color_Element (B, Max),
               Alpha => 1.0);
         end;
      end if;
   end Parse_Hex_Color;

   --------------------
   -- To_Html_String --
   --------------------

   function To_Html_String
     (Color : Athena_Color)
      return String
   is
      R : constant Natural := Natural (Color.Red * 255.0);
      G : constant Natural := Natural (Color.Green * 255.0);
      B : constant Natural := Natural (Color.Blue * 255.0);

      function Trim (X : String) return String
      is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Left));

   begin
      return "rgb(" & Trim (R'Image) & "," & Trim (G'Image)
        & "," & Trim (B'Image) & ")";
   end To_Html_String;

end Athena.Color;
