with Ada.Text_IO;

with Athena.Logging;

package body Athena.VM.Code is

   ----------
   -- Load --
   ----------

   procedure Load
     (Block : in out Code_Block;
      Path  : String)
   is
      pragma Unreferenced (Path);
      procedure Add (Instr : Instruction);
      procedure Reg (Index : Register_Index);
      procedure Indirect (Index : Register_Index;
                          Offset : Integer := 0);
      procedure FR (Index : Register_Index);
      procedure VR (Index : Register_Index);
      procedure Imm (Value : Address_Type);
      procedure Imm (Value : Real);
      procedure Addr (A : Address_Type);

      procedure Log (Text : String);

      V : Code_Vectors.Vector renames Block.Vector;

      ---------
      -- Add --
      ---------

      procedure Add (Instr : Instruction) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => Instruction_Tag,
               Instr      => Instr));
      end Add;

      ----------
      -- Addr --
      ----------

      procedure Addr (A : Address_Type) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag  => Address_Tag,
               Addr => A));
      end Addr;

      --------
      -- FR --
      --------

      procedure FR (Index : Register_Index) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => FR_Tag,
               Freg_Index => Index));
      end FR;

      ---------
      -- Imm --
      ---------

      procedure Imm (Value : Address_Type) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => Cell_Tag,
               Immediate  =>
                 Athena.VM.Cells.To_Cell (Natural (Value))));
      end Imm;

      ---------
      -- Imm --
      ---------

      procedure Imm (Value : Real) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => Cell_Tag,
               Immediate  =>
                 Athena.VM.Cells.To_Cell (Value)));
      end Imm;

      --------------
      -- Indirect --
      --------------

      procedure Indirect
        (Index  : Register_Index;
         Offset : Integer := 0) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => R_Tag,
               Reg_Index  => Index,
               Indirect   => True,
               Offset     =>
                 (if Offset < 0
                  then Address_Type (65536 + Offset)
                  else Address_Type (Offset))));
      end Indirect;

      ---------
      -- Log --
      ---------

      procedure Log (Text : String) is
      begin
         Athena.Logging.Log
           (Block.Vector.Length'Image
            & ": " & Text);
      end Log;

      ---------
      -- Reg --
      ---------

      procedure Reg (Index : Register_Index) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => R_Tag,
               Reg_Index  => Index,
               Indirect   => False,
               Offset     => 0));
      end Reg;

      --------
      -- VR --
      --------

      procedure VR (Index : Register_Index) is
      begin
         Block.Vector.Append
           (Code_Cell'
              (Tag        => VR_Tag,
               Vreg_Index => Index));
      end VR;

      L1, L2, L3 : Natural;
   begin
      Log ("clr v0");
      Add (I_CLR);
      VR (0);

      Log ("clr f0");
      Add (I_CLR);
      FR (0);

      Log ("mov #4096, r0");
      Add (I_MOV);
      Imm (4096);
      Reg (0);

      Log ("1:");
      L1 := Block.Vector.Last_Index + 1;

      Log ("bit #0x100, (r0)");
      Add (I_BIT);
      Imm (16#0100#);
      Indirect (0);

      Log ("beq $3");
      Add (I_BEQ);
      Imm (0);
      L3 := Block.Vector.Last_Index;

      Log ("bit #0x13, (r0)");
      Add (I_BIT);
      Imm (16#13#);
      Indirect (0);

      Log ("beq $2");
      Add (I_BEQ);
      Imm (0);
      L2 := Block.Vector.Last_Index;

      Log ("mov 1(r0), v1");
      Add (I_MOV);
      Indirect (0, 1);
      VR (1);

      Log ("add 2(r0), v1");
      Add (I_ADD);
      Indirect (0, 2);
      VR (1);

      Log ("mov 3(r0), f1");
      Add (I_MOV);
      Indirect (0, 3);
      FR (1);

      Log ("mul f1, v1");
      Add (I_MUL);
      FR (1);
      VR (1);

      Log ("add f1, f0");
      Add (I_ADD);
      FR (1);
      FR (0);

      Log ("add v1, v0");
      Add (I_ADD);
      VR (1);
      VR (0);

      V (L2) := (Cell_Tag,
                 Athena.VM.Cells.To_Cell (Block.Vector.Last_Index + 1));

      Log ("add #16, r0");
      Add (I_ADD);
      Imm (16);
      Reg (0);

      Log ("cmp r0, #8192");
      Add (I_CMP);
      Reg (0);
      Imm (8192);

      Log ("bne $1");
      Add (I_BNE);
      Imm (Address_Type (L1));

      Log ("$3:");
      V (L3) := (Cell_Tag,
                 Athena.VM.Cells.To_Cell (Block.Vector.Last_Index + 1));

      Log ("cmp f0, #0.0");
      Add (I_CMP);
      FR (0);
      Imm (0.0);

      Log ("beq 0");
      Add (I_BEQ);
      Imm (0);

      Log ("neg v0");
      Add (I_NEG);
      VR (0);

      Log ("div f0, v0");
      Add (I_DIV);
      FR (0);
      VR (0);

      Log ("mov v0, 0x0100");
      Add (I_MOV);
      VR (0);
      Addr (16#0100#);

      Log ("mov #1, 0x0101");
      Add (I_MOV);
      Imm (1);
      Addr (16#0101#);

      Log ("br 0");
      Add (I_BR);
      Imm (0);

      Log ("brk");
      Add (I_BRK);

   end Load;

   ----------
   -- Step --
   ----------

   procedure Step
     (Code :    Code_Block;
      PC   : in out Address_Type;
      PSW  : in out Status_Word;
      Rs   : in out Registers;
      Fs   : in out Float_Registers;
      Vs   : in out Vector_Registers;
      Mem  : in out Athena.VM.Cells.Store_Interface'Class)
   is

      Src  : Athena.VM.Cells.Cell_Type;
      Current : Code_Cell := Code.Vector.Element (Natural (PC));
      IR   : Instruction;

      procedure Error (Tag : String);

      function To_Integer (Addr : Address_Type) return Integer
      is (if Addr < 32768
          then Integer (Addr)
          else Integer (Addr) - 65536);

      function To_Address (I : Integer) return Address_Type
      is (if I < 0
          then Address_Type (65536 + I)
          else Address_Type (I));

      procedure Next_Code;
      procedure Fetch;
      procedure Store;
      procedure Update
        (Op : not null access
           procedure (Src : Athena.VM.Cells.Cell_Type;
                      Dst : in out Athena.VM.Cells.Cell_Type));

      -----------
      -- Error --
      -----------

      procedure Error (Tag : String) is
      begin
         Athena.Logging.Log
           ("pc =" & PC'Image
            & ": " & Tag);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "pc =" & PC'Image
            & ": " & Tag);
         raise Code_Error with Tag;
      end Error;

      -----------
      -- Fetch --
      -----------

      procedure Fetch is
      begin
         case Current.Tag is
            when Instruction_Tag =>
               Error ("expected value");
            when R_Tag =>
               if Current.Indirect then
                  Src := Mem.Get (Rs (Current.Reg_Index) + Current.Offset);
               else
                  Src := Cells.To_Cell (To_Integer (Rs (Current.Reg_Index)));
               end if;
            when FR_Tag =>
               Src := Cells.To_Cell (Fs (Current.Freg_Index));
            when VR_Tag =>
               Src := Cells.To_Cell (Vs (Current.Vreg_Index));
            when Cell_Tag =>
               Src := Current.Immediate;
            when Address_Tag =>
               Src := Mem.Get (Current.Addr);
         end case;
         Next_Code;
      end Fetch;

      ---------------
      -- Next_Code --
      ---------------

      procedure Next_Code is
      begin
         PC := PC + 1;
         Current := Code.Vector.Element (Natural (PC));
      end Next_Code;

      -----------
      -- Store --
      -----------

      procedure Store is
      begin
         case Current.Tag is
            when Instruction_Tag =>
               Error ("expected value");
            when R_Tag =>
               if Current.Indirect then
                  Mem.Set (Rs (Current.Reg_Index) + Current.Offset, Src);
               else
                  Rs (Current.Reg_Index) :=
                    To_Address (Cells.To_Integer (Src));
               end if;
            when FR_Tag =>
               Fs (Current.Freg_Index) := Cells.To_Real (Src);
            when VR_Tag =>
               Vs (Current.Vreg_Index) := Cells.To_Vector (Src);
            when Cell_Tag =>
               Error ("immediate assignment");
            when Address_Tag =>
               Mem.Set (Current.Addr, Src);
         end case;
         Next_Code;
      end Store;

      ------------
      -- Update --
      ------------

      procedure Update
        (Op : not null access
           procedure (Src : Athena.VM.Cells.Cell_Type;
                      Dst : in out Athena.VM.Cells.Cell_Type))
      is
         Dst  : Athena.VM.Cells.Cell_Type;

         procedure Process (X : in out Athena.VM.Cells.Cell_Type);

         -------------
         -- Process --
         -------------

         procedure Process (X : in out Athena.VM.Cells.Cell_Type) is
         begin
            Op (Src, X);
         end Process;

      begin
         case Current.Tag is
            when Instruction_Tag =>
               Error ("expected value");
            when R_Tag =>
               if Current.Indirect then
                  Mem.Update (Rs (Current.Reg_Index) + Current.Offset,
                              Process'Access);
               else
                  Dst := Cells.To_Cell (Integer (Rs (Current.Reg_Index)));
                  Op (Src, Dst);
                  Rs (Current.Reg_Index) :=
                    Address_Type (Cells.To_Integer (Dst));
               end if;
            when FR_Tag =>
               Dst := Cells.To_Cell (Fs (Current.Freg_Index));
               Op (Src, Dst);
               Fs (Current.Freg_Index) := Cells.To_Real (Dst);

            when VR_Tag =>
               Dst := Cells.To_Cell (Vs (Current.Vreg_Index));
               Op (Src, Dst);
               Vs (Current.Vreg_Index) := Cells.To_Vector (Dst);

            when Cell_Tag =>
               Error ("cannot update immediate");

            when Address_Tag =>
               Mem.Update
                 (Current.Addr, Process'Access);
         end case;
         Next_Code;
      end Update;

   begin

      if Current.Tag /= Instruction_Tag then
         return;
      end if;

      Athena.Logging.Log
        ("pc" & PC'Image & ": "
         & Current.Instr'Image);

      IR := Current.Instr;
      Next_Code;

      case IR is
         when I_MOV =>
            Fetch;
            Store;
         when I_ADD =>
            Fetch;
            Update (Cells.Add'Access);
         when I_SUB =>
            Fetch;
            Update (Cells.Subtract'Access);
         when I_MUL =>
            Fetch;
            Update (Cells.Multiply'Access);
         when I_DIV =>
            Fetch;
            Update (Cells.Divide'Access);
         when I_CMP =>
            Fetch;
            declare
               use type Cells.Cell_Type;
               Src_1 : constant Cells.Cell_Type := Src;
               Z     : Boolean renames PSW (Zero);
               N     : Boolean renames PSW (Negative);
            begin
               Fetch;
               Z := Src_1 = Src;
               N := Src_1 < Src;
            end;
         when I_BIT =>
            Fetch;
            declare
               Src_1 : constant Address_Type :=
                         Address_Type (Cells.To_Integer (Src));
            begin
               Fetch;
               declare
                  Src_2 : constant Address_Type :=
                            Address_Type (Cells.To_Integer (Src));
                  Z     : Boolean renames PSW (Zero);
               begin
                  Z := (Src_1 and Src_2) = 0;
               end;
            end;
         when I_CLR =>
            declare
               procedure Set_Zero
                 (Src : Cells.Cell_Type;
                  Dst : in out Cells.Cell_Type);

               --------------
               -- Set_Zero --
               --------------

               procedure Set_Zero
                 (Src : Cells.Cell_Type;
                  Dst : in out Cells.Cell_Type)
               is
                  pragma Unreferenced (Src);
               begin
                  Cells.Clear (Dst);
               end Set_Zero;

            begin
               Update (Set_Zero'Access);
               PSW (Zero) := True;
               PSW (Negative) := False;
            end;

         when I_NEG =>
            declare
               procedure Negate
                 (Src : Cells.Cell_Type;
                  Dst : in out Cells.Cell_Type);

               --------------
               -- Set_Zero --
               --------------

               procedure Negate
                 (Src : Cells.Cell_Type;
                  Dst : in out Cells.Cell_Type)
               is
                  pragma Unreferenced (Src);
               begin
                  Cells.Negate (Dst);
               end Negate;

            begin
               Update (Negate'Access);
            end;

         when I_NOT =>
            declare
               procedure Process
                 (Src : Cells.Cell_Type;
                  Dst : in out Cells.Cell_Type);

               -------------
               -- Process --
               -------------

               procedure Process
                 (Src : Cells.Cell_Type;
                  Dst : in out Cells.Cell_Type)
               is
                  pragma Unreferenced (Src);
               begin
                  Dst :=
                    Cells.To_Cell
                      (To_Integer
                         (not To_Address
                            (Cells.To_Integer (Dst))));
               end Process;

            begin
               Update (Process'Access);
            end;

         when I_BR =>
            Fetch;
            PC := To_Address (Cells.To_Integer (Src));
            Athena.Logging.Log
              ("pc <-" & PC'Image);

         when I_BEQ =>
            Fetch;
            if PSW (Zero) then
               PC := To_Address (Cells.To_Integer (Src));
            end if;

         when I_BNE =>
            Fetch;
            if not PSW (Zero) then
               PC := To_Address (Cells.To_Integer (Src));
            end if;

         when I_JSR =>
            Error ("unimplemented");

         when I_RTS =>
            Error ("unimplemented");

         when I_NOP =>
            null;

         when I_BRK =>
            raise Break_Error with "break at" & PC'Image;

      end case;

   end Step;

end Athena.VM.Code;
