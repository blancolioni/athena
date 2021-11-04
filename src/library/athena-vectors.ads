private with Ada.Numerics.Generic_Real_Arrays;
private with Athena.Elementary_Functions;

package Athena.Vectors is

   type Vector_2 is private;

   function X (Vector : Vector_2) return Real;
   function Y (Vector : Vector_2) return Real;

   function "abs" (Vector : Vector_2) return Non_Negative_Real;

   function "+" (Left, Right : Vector_2) return Vector_2;
   function "-" (Left, Right : Vector_2) return Vector_2;

   function "*" (Left : Real; Right : Vector_2) return Vector_2;
   function "*" (Left : Vector_2; Right : Real) return Vector_2;

   function Zero return Vector_2;
   function Unit_X return Vector_2;
   function Unit_Y return Vector_2;

   subtype Normal_Vector_2 is Vector_2
     with Dynamic_Predicate => abs (abs (Normal_Vector_2) - 1.0) < 0.001;

   function Normalize (Vector : Vector_2) return Normal_Vector_2;

private

   package Real_Arrays is
     new Ada.Numerics.Generic_Real_Arrays (Real);

   type Vector_2 is
      record
         Vector : Real_Arrays.Real_Vector (1 .. 2) := (0.0, 0.0);
      end record;

   function Zero return Vector_2 is (Vector => (0.0, 0.0));

   function Unit_X return Vector_2 is (Vector => (1.0, 0.0));
   function Unit_Y return Vector_2 is (Vector => (0.0, 1.0));

   function Normalize (Vector : Vector_2) return Normal_Vector_2
   is (Vector => Real_Arrays."/" (Vector.Vector,
                                  Real_Arrays."abs" (Vector.Vector)));

   function "+" (Left, Right : Vector_2) return Vector_2
   is (Vector => (Left.Vector (1) + Right.Vector (1),
                  Left.Vector (2) + Right.Vector (2)));

   function "-" (Left, Right : Vector_2) return Vector_2
   is (Vector => (Left.Vector (1) - Right.Vector (1),
                  Left.Vector (2) - Right.Vector (2)));

   function To_Vector (X, Y : Real) return Vector_2
   is (Vector => (X, Y));

   function X (Vector : Vector_2) return Real is (Vector.Vector (1));
   function Y (Vector : Vector_2) return Real is (Vector.Vector (2));

   function "abs" (Vector : Vector_2) return Non_Negative_Real
   is (Athena.Elementary_Functions.Sqrt
       (Vector.Vector (1) ** 2 + Vector.Vector (2) ** 2));

end Athena.Vectors;
