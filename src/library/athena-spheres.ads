with Ada.Containers.Vectors;
with Athena.Trigonometry;

package Athena.Spheres is

   type Surface_Point is
      record
         X, Y, Z : Signed_Unit_Real;
      end record;

   function Normalize (X, Y, Z : Real) return Surface_Point;

   function Get_Subtended_Angle
     (P1, P2 : Surface_Point)
      return Athena.Trigonometry.Angle;

   function Get_Bearing
     (From, To : Surface_Point)
      return Athena.Trigonometry.Angle;

   function To_Surface_Point
     (Latitude, Longitude : Athena.Trigonometry.Angle)
      return Surface_Point;

   function Show (P : Surface_Point) return String;

   function Show
     (Latitude, Longitude : Athena.Trigonometry.Angle)
      return String;

   package Surface_Point_Vectors is
     new Ada.Containers.Vectors (Positive, Surface_Point);

   procedure Random_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural);

   procedure Spiral_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural);

end Athena.Spheres;
