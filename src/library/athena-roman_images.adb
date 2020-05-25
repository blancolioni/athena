package body Athena.Roman_Images is

   -----------------
   -- Roman_Image --
   -----------------

   function Roman_Image (X : Positive) return String is

      subtype Digit_Range is Natural range 0 .. 9;

      Groups : constant array (Positive range <>) of String (1 .. 3) :=
                 (1 => "IVX",
                  2 => "XLC",
                  3 => "CDM",
                  4 => "M**");

      function Digit_Image (D : Digit_Range;
                            One, Five, Ten : Character)
                            return String;
      function Inner
        (Group_Index : Positive;
         Value       : Natural)
         return String;

      -----------------
      -- Digit_Image --
      -----------------

      function Digit_Image (D : Digit_Range;
                            One, Five, Ten : Character)
                            return String
      is
      begin
         case D is
            when 0 .. 3 =>
               return S : constant String (1 .. D) := (others => One) do
                  null;
               end return;
            when 4 =>
               return (One, Five);
            when 5 =>
               return (1 => Five);
            when 6 .. 8 =>
               return S : constant String (1 .. D - 4) :=
                 (1 => Five, others => One)
               do
                  null;
               end return;
            when 9 =>
               return (One, Ten);
         end case;
      end Digit_Image;

      -----------
      -- Inner --
      -----------

      function Inner
        (Group_Index : Positive;
         Value       : Natural)
         return String
      is
         Ds : constant String := Groups (Group_Index);
         Last : constant String :=
                  Digit_Image (Value mod 10, Ds (1), Ds (2), Ds (3));
      begin
         if Value < 10 then
            return Last;
         elsif Group_Index < Groups'Last then
            return Inner (Group_Index + 1, Value / 10) & Last;
         else
            return Result : constant String (1 .. Value) :=
              (others => Ds (1))
            do
               null;
            end return;
         end if;
      end Inner;

   begin
      return Inner (1, X);
   end Roman_Image;

end Athena.Roman_Images;
