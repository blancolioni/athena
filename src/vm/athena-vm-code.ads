private with Ada.Containers.Vectors;

with Timon.Vectors;

with Athena.VM.Cells;

package Athena.VM.Code is

   Break_Error : exception;
   Code_Error  : exception;

   type Instruction is
     (I_MOV, I_ADD, I_SUB, I_MUL, I_DIV, I_CMP, I_BIT,
      I_CLR, I_NEG, I_NOT,
      I_BR, I_BEQ, I_BNE,
      I_JSR,
      I_RTS, I_NOP, I_BRK);

   type Code_Tag is
     (Instruction_Tag, R_Tag, FR_Tag, VR_Tag, Cell_Tag, Address_Tag);

   type Code_Cell (Tag : Code_Tag := Instruction_Tag) is private;

   type Code_Block is private;

   procedure Load
     (Block : in out Code_Block;
      Path  : String);

   type Registers is array (Register_Index) of Address_Type;
   type Float_Registers is array (Register_Index) of Real;
   type Vector_Registers is array (Register_Index) of Timon.Vectors.Vector_3;

   procedure Step
     (Code : Code_Block;
      PC   : in out Address_Type;
      PSW  : in out Status_Word;
      Rs   : in out Registers;
      Fs   : in out Float_Registers;
      Vs   : in out Vector_Registers;
      Mem  : in out Athena.VM.Cells.Store_Interface'Class);

private

   type Code_Cell (Tag : Code_Tag := Instruction_Tag) is
      record
         case Tag is
            when Instruction_Tag =>
               Instr     : Instruction := I_BRK;
            when R_Tag =>
               Reg_Index : Register_Index := 0;
               Indirect  : Boolean;
               Offset    : Address_Type;
            when FR_Tag =>
               Freg_Index : Register_Index := 0;
            when VR_Tag =>
               Vreg_Index : Register_Index := 0;
            when Cell_Tag =>
               Immediate  : Athena.VM.Cells.Cell_Type;
            when Address_Tag =>
               Addr       : Address_Type := 0;
         end case;
      end record;

   package Code_Vectors is
     new Ada.Containers.Vectors (Natural, Code_Cell);

   type Code_Block is
      record
         Vector : Code_Vectors.Vector;
      end record;

end Athena.VM.Code;
