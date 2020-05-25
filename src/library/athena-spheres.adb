with Ada.Numerics;

with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Random;
with Athena.Real_Images;

package body Athena.Spheres is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

--     function Cross (A, B : Surface_Point) return Surface_Point is
--       (Normalize
--     (A.Y * B.Z - A.Z * B.Y, A.Z * B.X - A.X * B.Z, A.X * B.Y - A.Y * B.X));
--
--     function Dot (A, B : Surface_Point) return Real
--     is (A.X * B.X + A.Y * B.Y + A.Z * B.Z);

--     function Get_Bearing
--       (From, To : Surface_Point)
--        return Athena.Trigonometry.Angle
--     is
--        use Athena.Trigonometry;
--        Lat_1 : constant Angle := Arcsin (From.Z);
--        Lat_2 : constant Angle := Arcsin (To.Z);
--        Long_1 : constant Angle := Arctan (From.Y, From.X);
--        Long_2 : constant Angle := Arctan (To.Y, To.X);
--        D      : constant Angle := Get_Subtended_Angle (From, To);
--     begin
--        if From.Z > 1.0 - 1.0e-6 then
--           return From_Degrees (270.0);
--        elsif From.Z < -1.0 + 1.0e-6 then
--           return From_Degrees (90.0);
--        end if;
--
--        declare
--           A : constant Angle :=
--                 Arcsin ((Sin (Lat_2) - Sin (Lat_1) * Cos (D))
--                         / (Sin (D) * Cos (Lat_1)));
--        begin
--           Athena.Logging.Log
--             ("angle",
--              "",
--              "",
--              Show (Lat_1, Long_1) & " "
--              & Show (Lat_2, Long_2) & " "
--              & ": D = " & Image (To_Degrees (D))
--              & "; angle = " & Image (To_Degrees (A)));
--
--           return A;
--        end;
--     end Get_Bearing;

   function Get_Bearing
     (From, To : Surface_Point)
      return Athena.Trigonometry.Angle
   is
      use Athena.Trigonometry;
      Lat_1   : constant Angle := Arcsin (From.Z);
      Lat_2   : constant Angle := Arcsin (To.Z);
      Long_1  : constant Angle := Arctan (From.Y, From.X);
      Long_2  : constant Angle := Arctan (To.Y, To.X);
      X       : constant Real :=
                  Cos (Lat_2) * Sin (Long_2 - Long_1);
      Y       : constant Real :=
                  Cos (Lat_1) * Sin (Lat_2)
                - Sin (Lat_1) * Cos (Lat_2) * Cos (Long_2 - Long_1);
      A       : constant Angle := Arctan (Y, X);

--        P       : constant Surface_Point :=
--                    To_Surface_Point (Lat_1, Long_2);
--        A       : constant Angle := Get_Subtended_Angle (From, To);
--        B       : constant Angle := Get_Subtended_Angle (From, P);
--        C       : constant Angle := Get_Subtended_Angle (To, P);
--        Angle_2 : constant Angle :=
--                    Arccos ((Cos (C)  - Cos (A) * Cos (B))
--                            / (Sin (A) * Sin (B)));
   begin
      Athena.Logging.Log
        ("angle",
         "",
         "",
         Show (Lat_1, Long_1)
         & " "
         & Show (Lat_2, Long_2)
         & " "
         & ": X = " & Image (X)
         & "; Y = " & Image (Y)
         & "; angle = " & Image (To_Degrees (A)));

      return A;
   end Get_Bearing;

--     function Get_Angle
--       (P1, P2, P3 : Surface_Point)
--        return Real
--     is
--        use Athena.Elementary_Functions;
--        N1 : constant Surface_Point := Cross (P2, P3);
--        N2 : constant Surface_Point := Cross (P1, P2);
--        Angle_1 : constant Real := Arccos (Dot (N1, N2));
--        A       : constant Real := Get_Angle (P1, P2);
--        B       : constant Real := Get_Angle (P2, P3);
--        C       : constant Real := Get_Angle (P3, P1);
--        Angle_2 : constant Real :=
--                    Arccos ((Cos (C)  - Cos (A) * Cos (B))
--                            / (Sin (A) * Sin (B)));
--     begin
--        if False then
--           Athena.Logging.Log
--             ("angle",
--              "",
--              "",
--              "A = " & Show (P1)
--              & "; B = " & Show (P2)
--              & "; C = " & Show (P3)
--              & "; N1 = B x C = " & Show (N1)
--              & "; N2 = A x B = " & Show (N2)
--              & "; N1 . N2 = " & Image (Dot (N1, N2))
--         & "; angle = arccos (N1 . N2) = " & Image (Angle_1 * 180.0 / Pi));
--        end if;
--
--        Athena.Logging.Log
--          ("angle",
--           "",
--           "",
--           Show (P1) & " "
--           & Show (P2) & " "
--           & Show (P3)
--           & ": A = " & Image (A * 180.0 / Pi)
--           & "; B = " & Image (B * 180.0 / Pi)
--           & "; C = " & Image (C * 180.0 / Pi)
--           & "; angle = " & Image (Angle_2 * 180.0 / Pi));
--
--        return Angle_2;
--     end Get_Angle;

   -------------------------
   -- Get_Subtended_Angle --
   -------------------------

   function Get_Subtended_Angle
     (P1, P2 : Surface_Point)
      return Athena.Trigonometry.Angle
   is
      use Athena.Elementary_Functions;
      D  : constant Non_Negative_Real :=
             Sqrt
               ((P1.X - P2.X) ** 2
                + (P1.Y - P2.Y) ** 2
                + (P1.Z - P2.Z) ** 2);
      A  : constant Real := 2.0 * Arcsin (D / 2.0);
   begin
      return Athena.Trigonometry.From_Radians (A);
   end Get_Subtended_Angle;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (X, Y, Z : Real) return Surface_Point is
      use Athena.Elementary_Functions;
      D : constant Non_Negative_Real :=
            Sqrt (X ** 2 + Y ** 2 + Z ** 2);
   begin
      return (X / D, Y / D, Z / D);
   end Normalize;

   --------------------------
   -- Random_Sphere_Points --
   --------------------------

   procedure Random_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural)
   is
      use Athena.Elementary_Functions;

      Pi : constant := Ada.Numerics.Pi;

      Sample_Count : Positive := 1;

      Lambdas : array (1 .. Count) of Real;
      Phis    : array (1 .. Count) of Real;

      Current_Count : Natural := 0;

      procedure Next_Point;

      ---------------
      -- New_Point --
      ---------------

      procedure Next_Point is
         Best_Lambda : Real;
         Best_Phi    : Real;
         Best_Min_D  : Real := 0.0;
      begin
         for I in 1 .. Sample_Count loop
            declare
               Lambda : constant Real :=
                          (Athena.Random.Unit_Random * 2.0 - 1.0) * Pi;
               Phi    : constant Real :=
                          Arccos (2.0 * Athena.Random.Unit_Random - 1.0);
               Min_Distance : Non_Negative_Real := Real'Last;
            begin
               if I = 1 then
                  Best_Lambda := Lambda;
                  Best_Phi    := Phi;
               else
                  for J in 1 .. Current_Count loop
                     declare
                        Sin_Phi_1 : constant Signed_Unit_Real :=
                                      Sin (Phi);
                        Sin_Phi_2 : constant Signed_Unit_Real :=
                                      Sin (Phis (J));
                        Cos_Phi_1 : constant Signed_Unit_Real :=
                                      Cos (Phi);
                        Cos_Phi_2 : constant Signed_Unit_Real :=
                                      Cos (Phis (J));
                        D_Lambda  : constant Non_Negative_Real :=
                                      abs (Lambda - Lambdas (J));
                        Cos_D_Lambda : constant Signed_Unit_Real :=
                                        Cos (D_Lambda);
                        Cos_D     : constant Signed_Unit_Real :=
                                      Sin_Phi_1 * Sin_Phi_2
                                          + Cos_Phi_1 * Cos_Phi_2
                                        * Cos_D_Lambda;
                        D         : constant Non_Negative_Real :=
                                      Arccos (Cos_D);
                     begin
                        if D < Min_Distance then
                           Min_Distance := D;
                        end if;
                     end;
                  end loop;
               end if;

               if Min_Distance > Best_Min_D then
                  Best_Min_D := Min_Distance;
                  Best_Lambda := Lambda;
                  Best_Phi := Phi;
               end if;
            end;
         end loop;

         Current_Count := Current_Count + 1;
         Lambdas (Current_Count) := Best_Lambda;
         Phis (Current_Count) := Best_Phi;
         Sample_Count := Sample_Count + 1;
      end Next_Point;

   begin
      for I in 1 .. Count loop
         Next_Point;
      end loop;

      for I in 1 .. Count loop
         Point_List.Append
           (Surface_Point'
              (X => Cos (Lambdas (I)) * Cos (Phis (I)),
               Y => Sin (Lambdas (I)) * Cos (Phis (I)),
               Z => Sin (Phis (I))));
      end loop;
   end Random_Sphere_Points;

   ----------
   -- Show --
   ----------

   function Show
     (Latitude, Longitude : Athena.Trigonometry.Angle)
      return String
   is
   begin
      return "("
        & Image (Athena.Trigonometry.To_Degrees (Latitude))
        & "N "
        & Image (Athena.Trigonometry.To_Degrees (Longitude))
        & "E)";
   end Show;

   ----------
   -- Show --
   ----------

   function Show (P : Surface_Point) return String is
   begin
      return "(" & Image (P.X) & "," & Image (P.Y) & "," & Image (P.Z) & ")";
   end Show;

   --------------------------
   -- Spiral_Sphere_Points --
   --------------------------

   procedure Spiral_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural)
   is

      --  from http://web.archive.org/web/20120421191837/
      --  http://www.cgafaq.info/wiki/Evenly_distributed_points_on_sphere
      --  dlong := pi*(3-sqrt(5))  /* ~2.39996323 */
      --  dz    := 2.0/N
      --  long := 0
      --  z    := 1 - dz/2
      --  for k := 0 .. N-1
      --      r    := sqrt(1-z*z)
      --      node[k] := (cos(long)*r, sin(long)*r, z)
      --      z    := z - dz
      --      long := long + dlong

      use Athena.Elementary_Functions;
      D_Long : constant Real := Ada.Numerics.Pi
                 * (3.0 - Sqrt (5.0));
      D_Z : constant Real := 2.0 / Real (Count);
      Long : Real := 0.0;
      Z : Real := 1.0 - (D_Z / 2.0);

   begin
      for K in 1 .. Count loop
         declare
            R : constant Real := Sqrt (1.0 - Z ** 2);
         begin
            Point_List.Append ((Cos (Long) * R, Sin (Long) * R, Z));
            Z := Z - D_Z;
            Long := Long + D_Long;
         end;
      end loop;
   end Spiral_Sphere_Points;

   ----------------------
   -- To_Surface_Point --
   ----------------------

   function To_Surface_Point
     (Latitude, Longitude : Athena.Trigonometry.Angle)
      return Surface_Point
   is
      use Athena.Trigonometry;
   begin
      return (Cos (Longitude) * Cos (Latitude),
              Sin (Longitude) * Cos (Latitude),
              Sin (Latitude));
   end To_Surface_Point;

end Athena.Spheres;
