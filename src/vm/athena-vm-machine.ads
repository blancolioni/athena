private with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;

with Timon.Vectors;

with Athena.VM.Cells;
with Athena.VM.Code;
with Athena.VM.Drivers;

package Athena.VM.Machine is

   type Athena_Machine is
     new Ada.Finalization.Controlled
     and Athena.VM.Cells.Store_Interface
   with private;

   procedure Load_Driver
     (Machine : in out Athena_Machine'Class;
      Base    : Address_Type;
      Bound   : Address_Type;
      Driver  : Athena.VM.Drivers.Driver_Interface'Class);

   procedure Update_Driver
     (Machine : in out Athena_Machine'Class;
      Base    : Address_Type;
      Process : not null access
        procedure (Driver : in out Athena.VM.Drivers.Driver_Interface'Class));

   procedure Step
     (Machine : in out Athena_Machine'Class;
      Code    : Athena.VM.Code.Code_Block);

   function Ticks (Machine : Athena_Machine'Class) return Natural;

private

   package Driver_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Natural, Athena.VM.Drivers.Driver_Interface'Class,
        Athena.VM.Drivers."=");

   type Athena_Machine is
     new Ada.Finalization.Controlled
     and Athena.VM.Cells.Store_Interface with
      record
         PC      : Address_Type := 0;
         Rs      : Athena.VM.Code.Registers := (others => 0);
         Fs      : Athena.VM.Code.Float_Registers := (others => 0.0);
         Vs      : Athena.VM.Code.Vector_Registers :=
                     (others => Timon.Vectors.Zero);
         PSW     : Status_Word := (others => False);
         Drivers : Driver_Vectors.Vector;
         Ticks   : Natural := 0;
      end record;

   overriding procedure Initialize (Machine : in out Athena_Machine);

   overriding function Get
     (Machine : Athena_Machine;
      Address : Address_Type)
      return Athena.VM.Cells.Cell_Type;

   overriding procedure Set
     (Machine : in out Athena_Machine;
      Address : Address_Type;
      Cell    : Athena.VM.Cells.Cell_Type);

   overriding procedure Update
     (Machine : in out Athena_Machine;
      Address : Address_Type;
      Process : not null access
        procedure (Cell : in out Athena.VM.Cells.Cell_Type));

   function Ticks (Machine : Athena_Machine'Class) return Natural
   is (Machine.Ticks);

end Athena.VM.Machine;
