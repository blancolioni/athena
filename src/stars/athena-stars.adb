with Athena.Elementary_Functions;

package body Athena.Stars is

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Minerva.Star.Star_Class) return Non_Negative_Real
   is
      use Athena.Elementary_Functions;
   begin
      return Sqrt ((From.X - To.X) ** 2 + (From.Y - To.Y) ** 2);
   end Distance;

end Athena.Stars;
