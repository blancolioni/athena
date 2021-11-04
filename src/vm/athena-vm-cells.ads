with Timon.Vectors;

package Athena.VM.Cells is

   type Cell_Tag is (Integer_Tag, Real_Tag, Vector_Tag);

   type Cell_Type (Tag : Cell_Tag := Integer_Tag) is private;

   subtype Integer_Cell_Type is Cell_Type (Integer_Tag);
   subtype Real_Cell_Type is Cell_Type (Real_Tag);
   subtype Vector_Cell_Type is Cell_Type (Vector_Tag);

   function To_Integer (Cell : Integer_Cell_Type) return Integer;
   function To_Real (Cell : Real_Cell_Type) return Real;
   function To_Vector (Cell : Vector_Cell_Type) return Timon.Vectors.Vector_3;

   function To_Cell (Value : Integer) return Integer_Cell_Type;
   function To_Cell (Value : Real) return Real_Cell_Type;
   function To_Cell (Value : Timon.Vectors.Vector_3) return Vector_Cell_Type;

   function "<" (Left, Right : Cell_Type) return Boolean;

   procedure Negate
     (Dst : in out Cell_Type);

   procedure Clear
     (Dst : in out Cell_Type);

   procedure Add
     (Src : Cell_Type;
      Dst : in out Cell_Type);

   procedure Subtract
     (Src : Cell_Type;
      Dst : in out Cell_Type);

   procedure Multiply
     (Src : Cell_Type;
      Dst : in out Cell_Type);

   procedure Divide
     (Src : Cell_Type;
      Dst : in out Cell_Type);

   procedure Absolute_Value
     (Dst : in out Cell_Type);

   procedure Normalize
     (Dst : in out Cell_Type);

   type Store_Interface is interface;

   function Get
     (Store   : Store_Interface;
      Address : Address_Type)
      return Athena.VM.Cells.Cell_Type
      is abstract;

   procedure Set
     (Store   : in out Store_Interface;
      Address : Address_Type;
      Cell    : Athena.VM.Cells.Cell_Type)
   is abstract;

   procedure Update
     (Store   : in out Store_Interface;
      Address : Address_Type;
      Process : not null access
        procedure (Cell : in out Athena.VM.Cells.Cell_Type))
   is abstract;

private

   type Cell_Type (Tag : Cell_Tag := Integer_Tag) is
      record
         case Tag is
            when Integer_Tag =>
               Integer_Value : Integer := 0;
            when Real_Tag =>
               Real_Value    : Real := 0.0;
            when Vector_Tag =>
               Vector_Value  : Timon.Vectors.Vector_3 := Timon.Vectors.Zero;
         end case;
      end record;

   function To_Integer (Cell : Integer_Cell_Type) return Integer
   is (Cell.Integer_Value);

   function To_Real (Cell : Real_Cell_Type) return Real
   is (Cell.Real_Value);

   function To_Vector (Cell : Vector_Cell_Type) return Timon.Vectors.Vector_3
   is (Cell.Vector_Value);

   function To_Cell (Value : Integer) return Integer_Cell_Type
   is (Integer_Tag, Value);

   function To_Cell (Value : Real) return Real_Cell_Type
   is (Real_Tag, Value);

   function To_Cell (Value : Timon.Vectors.Vector_3) return Vector_Cell_Type
   is (Vector_Tag, Value);

end Athena.VM.Cells;
