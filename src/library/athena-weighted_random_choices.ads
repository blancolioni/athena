private with Ada.Containers.Vectors;

generic
   type Element_Type is private;
package Athena.Weighted_Random_Choices is

   type Weighted_Choice_Set is tagged private;

   function Is_Empty (Set : Weighted_Choice_Set'Class) return Boolean;

   procedure Insert
     (Set    : in out Weighted_Choice_Set'Class;
      Item   : Element_Type;
      Score  : Natural)
     with Post => Score = 0 or else not Set.Is_Empty;

   function Choose
     (Set : Weighted_Choice_Set'Class)
      return Element_Type
     with Pre => not Set.Is_Empty;

private

   type Weighted_Element is
      record
         Element : Element_Type;
         Score   : Natural;
      end record;

   package Vectors is new Ada.Containers.Vectors (Positive, Weighted_Element);

   type Weighted_Choice_Set is tagged
      record
         Vector      : Vectors.Vector;
         Total_Score : Natural := 0;
      end record;

   function Is_Empty (Set : Weighted_Choice_Set'Class) return Boolean
   is (Set.Vector.Is_Empty);

end Athena.Weighted_Random_Choices;
