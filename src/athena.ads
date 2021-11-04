with Timon;

package Athena is

   pragma Pure (Athena);

   subtype Real is Timon.Real;

   subtype Unit_Real is Real range 0.0 .. 1.0;
   subtype Signed_Unit_Real is Real range -1.0 .. 1.0;

   subtype Non_Negative_Real is Real range 0.0 .. Real'Last;

   function Clamp (X : Real;
                   Lo, Hi : Real)
                   return Real
   is (Real'Max (Lo, Real'Min (Hi, X)));

   function Unit_Clamp (X : Real) return Unit_Real
   is (Clamp (X, 0.0, 1.0));

   function Signed_Unit_Clamp (X : Real) return Signed_Unit_Real
   is (Clamp (X, -1.0, 1.0));

end Athena;
