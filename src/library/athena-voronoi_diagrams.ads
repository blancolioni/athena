private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

package Athena.Voronoi_Diagrams is

   type Voronoi_Diagram is tagged limited private;

   procedure Clear (Item : in out Voronoi_Diagram'Class);

   procedure Add_Point
     (To   : in out Voronoi_Diagram'Class;
      X, Y : Real);

   procedure Add_Spherical_Point
     (To        : in out Voronoi_Diagram'Class;
      X, Y, Z   : Signed_Unit_Real)
     with Pre => Z /= 1.0;

   procedure Generate
     (Diagram : in out Voronoi_Diagram'Class);

   function Delauny_Triangle_Count
     (Diagram : Voronoi_Diagram'Class)
      return Natural;

   procedure Get_Delauny_Triangle
     (Diagram : Voronoi_Diagram'Class;
      Index   : Positive;
      A, B, C : out Positive);

   procedure Get_Delauny_Triangle_Vertex
     (Diagram        : Voronoi_Diagram'Class;
      Vertex_Index   : Positive;
      X, Y           : out Real);

   procedure Get_Delauny_Triangle_Vertex
     (Diagram        : Voronoi_Diagram'Class;
      Vertex_Index   : Positive;
      X, Y, Z        : out Signed_Unit_Real);

   function Polygon_Count
     (Item : Voronoi_Diagram'Class)
      return Natural;

   function Vertex_Count
     (Item          : Voronoi_Diagram'Class;
      Polygon_Index : Positive)
      return Natural;

   function Vertex_X
     (Item : Voronoi_Diagram'Class;
      Polygon_Index : Positive;
      Vertex_Index  : Positive)
      return Real;

   function Vertex_Y
     (Item : Voronoi_Diagram'Class;
      Polygon_Index : Positive;
      Vertex_Index  : Positive)
      return Real;

   function Vertex_Count
     (Item : Voronoi_Diagram'Class)
      return Natural;

   procedure Get_Spherical_Vertex
     (Item          : Voronoi_Diagram'Class;
      Vertex_Index  : Positive;
      X, Y, Z       : out Signed_Unit_Real);

   function Polygon_Vertex_Index
     (Item           : Voronoi_Diagram'Class;
      Polygon_Index  : Positive;
      Index          : Positive)
      return Positive;

private

   type Voronoi_Point is
      record
         X, Y : Real;
      end record;

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Voronoi_Point);

   type Voronoi_Site is
      record
         Point      : Voronoi_Point;
         Site_Index : Positive;
      end record;

   package Site_Vectors is
     new Ada.Containers.Vectors (Positive, Voronoi_Site);

   type Point_Index_Array is array (Positive range <>) of Positive;

   type Voronoi_Polygon (Count : Natural) is
      record
         Vertices : Point_Index_Array (1 .. Count);
      end record;

   package Polygon_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Voronoi_Polygon);

   type Voronoi_Triangle is
      record
         A, B, C : Positive;
      end record;

   package Triangle_Vectors is
     new Ada.Containers.Vectors (Positive, Voronoi_Triangle);

   type Voronoi_Diagram is tagged limited
      record
         Sites        : Site_Vectors.Vector;
         Triangles    : Triangle_Vectors.Vector;
         Min_X, Min_Y : Real := Real'Last;
         Max_X, Max_Y : Real := Real'First;
         Diagram_Pts  : Point_Vectors.Vector;
         Diagram      : Polygon_Vectors.Vector;
      end record;

   function Vertex_Count
     (Item : Voronoi_Diagram'Class)
      return Natural
   is (Item.Diagram_Pts.Last_Index);

   function Polygon_Vertex_Index
     (Item           : Voronoi_Diagram'Class;
      Polygon_Index  : Positive;
      Index          : Positive)
      return Positive
   is (Item.Diagram.Element (Polygon_Index).Vertices (Index));

   function Delauny_Triangle_Count
     (Diagram : Voronoi_Diagram'Class)
      return Natural
   is (Diagram.Triangles.Last_Index);

end Athena.Voronoi_Diagrams;
