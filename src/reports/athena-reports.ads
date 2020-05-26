private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Athena.Real_Images;

package Athena.Reports is

   type Writer_Interface is interface;

   procedure Put
     (Writer : in out Writer_Interface;
      Text   : String)
   is abstract;

   procedure New_Line
     (Writer : in out Writer_Interface)
   is abstract;

   procedure Put_Line
     (Writer : in out Writer_Interface'Class;
      Text   : String);

   procedure Put_Heading
     (Writer : in out Writer_Interface'Class;
      Text   : String);

   function Standard_Writer return Writer_Interface'Class;

   type Column_Alignment is (Left, Middle, Right);

   type Report_Table is limited private;

   procedure Add_Heading
     (Table     : in out Report_Table;
      Heading   : String;
      Alignment : Column_Alignment := Left);

   procedure Add_Row
     (Table : in out Report_Table);

   procedure Add_Cell
     (Table : in out Report_Table;
      Text  : String);

   procedure Write
     (Table  : Report_Table;
      Writer : in out Writer_Interface'Class);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Row_Vectors is
     new Ada.Containers.Vectors
       (Positive, String_Vectors.Vector, String_Vectors."=");

   package Width_Vectors is
     new Ada.Containers.Vectors (Positive, Natural);

   package Alignment_Vectors is
     new Ada.Containers.Vectors (Positive, Column_Alignment);

   type Report_Table is
      record
         Headings  : String_Vectors.Vector;
         Rows      : Row_Vectors.Vector;
         Widths    : Width_Vectors.Vector;
         Alignment : Alignment_Vectors.Vector;
      end record;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

end Athena.Reports;
