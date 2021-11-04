private with Ada.Containers.Indefinite_Holders;

with Athena.VM.Cells;

package Athena.VM.Memory is

   type Memory_Type is tagged private;

   procedure Initialize
     (Memory : in out Memory_Type'Class;
      Size   : Natural);

   function Get
     (Memory  : Memory_Type;
      Address : Address_Type)
      return Athena.VM.Cells.Cell_Type;

   procedure Set
     (Memory  : in out Memory_Type;
      Address : Address_Type;
      Cell    : Athena.VM.Cells.Cell_Type);

   procedure Update
     (Memory  : in out Memory_Type;
      Address : Address_Type;
      Process : not null access
        procedure (Cell : in out Athena.VM.Cells.Cell_Type));

private

   type Cell_Array is
     array (Address_Type range <>) of Athena.VM.Cells.Cell_Type;

   package Cell_Array_Holders is
     new Ada.Containers.Indefinite_Holders (Cell_Array);

   type Memory_Type is tagged
      record
         Cells : Cell_Array_Holders.Holder;
      end record;

end Athena.VM.Memory;
