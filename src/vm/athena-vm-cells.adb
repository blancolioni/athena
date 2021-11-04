package body Athena.VM.Cells is

   use type Timon.Vectors.Vector_3;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Cell_Type) return Boolean is
   begin
      case Left.Tag is
         when Integer_Tag =>
            case Right.Tag is
               when Integer_Tag =>
                  return Left.Integer_Value < Right.Integer_Value;
               when Real_Tag =>
                  return Real (Left.Integer_Value) < Right.Real_Value;
               when Vector_Tag =>
                  return Real (Left.Integer_Value) < abs Right.Vector_Value;
            end case;
         when Real_Tag =>
            case Right.Tag is
               when Integer_Tag =>
                  return Left.Real_Value < Real (Right.Integer_Value);
               when Real_Tag =>
                  return Left.Real_Value < Right.Real_Value;
               when Vector_Tag =>
                  return Left.Real_Value < abs Right.Vector_Value;
            end case;
         when Vector_Tag =>
            case Right.Tag is
               when Integer_Tag =>
                  return abs Left.Vector_Value < Real (Right.Integer_Value);
               when Real_Tag =>
                  return abs Left.Vector_Value < Right.Real_Value;
               when Vector_Tag =>
                  return abs Left.Vector_Value < abs Right.Vector_Value;
            end case;
      end case;
   end "<";

   --------------------
   -- Absolute_Value --
   --------------------

   procedure Absolute_Value (Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            Dst.Integer_Value := abs Dst.Integer_Value;
         when Real_Tag =>
            Dst.Real_Value := abs Dst.Real_Value;
         when Vector_Tag =>
            Dst := (Real_Tag, abs Dst.Vector_Value);
      end case;
   end Absolute_Value;

   ---------
   -- Add --
   ---------

   procedure Add (Src : Cell_Type; Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value + Src.Integer_Value;
               when Real_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value
                    + Integer (Src.Real_Value);
               when Vector_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value
                    + Integer (abs Src.Vector_Value);
            end case;
         when Real_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Real_Value := Dst.Real_Value + Real (Src.Integer_Value);
               when Real_Tag =>
                  Dst.Real_Value := Dst.Real_Value + Src.Real_Value;
               when Vector_Tag =>
                  Dst.Real_Value := Dst.Real_Value + abs Src.Vector_Value;
            end case;
         when Vector_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    + Timon.Vectors.To_Vector (Real (Src.Integer_Value),
                                               0.0, 0.0);
               when Real_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    + Timon.Vectors.To_Vector (Src.Real_Value,
                                               0.0, 0.0);
               when Vector_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value + Src.Vector_Value;
            end case;
      end case;
   end Add;

   -----------
   -- Clear --
   -----------

   procedure Clear (Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            Dst.Integer_Value := 0;
         when Real_Tag =>
            Dst.Real_Value := 0.0;
         when Vector_Tag =>
            Dst.Vector_Value := Timon.Vectors.Zero;
      end case;
   end Clear;

   ------------
   -- Divide --
   ------------

   procedure Divide (Src : Cell_Type; Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value / Src.Integer_Value;
               when Real_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value
                    / Integer (Src.Real_Value);
               when Vector_Tag =>
                  Dst :=
                    (Vector_Tag, Src.Vector_Value / Real (Dst.Integer_Value));
            end case;
         when Real_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Real_Value := Dst.Real_Value / Real (Src.Integer_Value);
               when Real_Tag =>
                  Dst.Real_Value := Dst.Real_Value / Src.Real_Value;
               when Vector_Tag =>
                  Dst :=
                    (Vector_Tag, Src.Vector_Value / Dst.Real_Value);
            end case;
         when Vector_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    / Real (Src.Integer_Value);
               when Real_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    / Src.Real_Value;
               when Vector_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    / abs Src.Vector_Value;
            end case;
      end case;
   end Divide;

   --------------
   -- Multiply --
   --------------

   procedure Multiply (Src : Cell_Type; Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value * Src.Integer_Value;
               when Real_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value
                    * Integer (Src.Real_Value);
               when Vector_Tag =>
                  Dst :=
                    (Vector_Tag, Src.Vector_Value * Real (Dst.Integer_Value));
            end case;
         when Real_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Real_Value := Dst.Real_Value * Real (Src.Integer_Value);
               when Real_Tag =>
                  Dst.Real_Value := Dst.Real_Value * Src.Real_Value;
               when Vector_Tag =>
                  Dst :=
                    (Vector_Tag, Src.Vector_Value * Dst.Real_Value);
            end case;
         when Vector_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    * Real (Src.Integer_Value);
               when Real_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    * Src.Real_Value;
               when Vector_Tag =>
                  Dst := (Real_Tag, Dst.Vector_Value * Src.Vector_Value);
            end case;
      end case;
   end Multiply;

   ------------
   -- Negate --
   ------------

   procedure Negate (Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            Dst.Integer_Value := -Dst.Integer_Value;
         when Real_Tag =>
            Dst.Real_Value := -Dst.Real_Value;
         when Vector_Tag =>
            Dst.Vector_Value := -Dst.Vector_Value;
      end case;
   end Negate;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            Dst.Integer_Value := 1;
         when Real_Tag =>
            Dst.Real_Value := 1.0;
         when Vector_Tag =>
            Dst.Vector_Value := Timon.Vectors.Normalize (Dst.Vector_Value);
      end case;
   end Normalize;

   --------------
   -- Subtract --
   --------------

   procedure Subtract (Src : Cell_Type; Dst : in out Cell_Type) is
   begin
      case Dst.Tag is
         when Integer_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value - Src.Integer_Value;
               when Real_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value
                    - Integer (Src.Real_Value);
               when Vector_Tag =>
                  Dst.Integer_Value := Dst.Integer_Value
                    - Integer (abs Src.Vector_Value);
            end case;
         when Real_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Real_Value := Dst.Real_Value - Real (Src.Integer_Value);
               when Real_Tag =>
                  Dst.Real_Value := Dst.Real_Value - Src.Real_Value;
               when Vector_Tag =>
                  Dst.Real_Value := Dst.Real_Value - abs Src.Vector_Value;
            end case;
         when Vector_Tag =>
            case Src.Tag is
               when Integer_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    - Timon.Vectors.To_Vector (Real (Src.Integer_Value),
                                               0.0, 0.0);
               when Real_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value
                    - Timon.Vectors.To_Vector (Src.Real_Value,
                                               0.0, 0.0);
               when Vector_Tag =>
                  Dst.Vector_Value := Dst.Vector_Value - Src.Vector_Value;
            end case;
      end case;
   end Subtract;

end Athena.VM.Cells;
