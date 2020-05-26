package Athena.Color is

   type Athena_Color is
      record
         Red, Green, Blue, Alpha : Unit_Real;
      end record;

   Black : constant Athena_Color := (0.0, 0.0, 0.0, 1.0);
   White : constant Athena_Color := (1.0, 1.0, 1.0, 1.0);

   function From_String
     (Item : String)
      return Athena_Color;

   function To_Html_String
     (Color : Athena_Color)
      return String;

   function To_Html_String
     (R, G, B : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return String
   is (To_Html_String ((R, G, B, Alpha)));

end Athena.Color;
