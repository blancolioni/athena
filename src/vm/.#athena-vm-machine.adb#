with Athena.Logging;

package body Athena.VM.Machine is

   type Null_Driver is
     new Athena.VM.Drivers.Driver_Interface with null record;

   overriding function Get
     (Driver  : Null_Driver;
      Address : Page_Address_Type)
      return Athena.VM.Cells.Cell_Type
   is (Athena.VM.Cells.To_Cell (0));

   overriding procedure Set
     (Driver  : in out Null_Driver;
      Address : Page_Address_Type;
      Cell    : Athena.VM.Cells.Cell_Type)
   is null;

   overriding procedure Update
     (Driver  : in out Null_Driver;
      Address : Page_Address_Type;
      Process : not null access
        procedure (Cell : in out Athena.VM.Cells.Cell_Type))
   is null;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Machine : Athena_Machine; Address : Address_Type)
      return Athena.VM.Cells.Cell_Type
   is
   begin
      return Machine.Drivers (Natural (Address / 256))
        .Get (Page_Address_Type (Address mod 256));
   end Get;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Machine : in out Athena_Machine) is
      Driver : Null_Driver;
   begin
      for I in 0 .. 255 loop
         Machine.Drivers.Append (Driver);
      end loop;
   end Initialize;

   -----------------
   -- Load_Driver --
   -----------------

   procedure Load_Driver
     (Machine : in out Athena_Machine'Class;
      Base    : Address_Type;
      Bound   : Address_Type;
      Driver  : Athena.VM.Drivers.Driver_Interface'Class)
   is
      pragma Unreferenced (Bound);
   begin
      Machine.Drivers.Replace_Element
        (Natural (Base / 256), Driver);
   end Load_Driver;

   ---------
   -- Set --
   ---------

   overriding procedure Set
     (Machine : in out Athena_Machine; Address : Address_Type;
      Cell    :        Athena.VM.Cells.Cell_Type)
   is
   begin
      Athena.Logging.Log ("set:" & Address'Image);
      Machine.Drivers (Natural (Address / 256))
        .Set (Page_Address_Type (Address mod 256), Cell);
   end Set;

   ----------
   -- Step --
   ----------

   procedure Step
     (Machine : in out Athena_Machine'Class;
      Code    : Athena.VM.Code.Code_Block)
   is
   begin
      Athena.VM.Code.Step
        (Code => Code,
         PSW  => Machine.PSW,
         PC   => Machine.PC,
         Rs   => Machine.Rs,
         Fs   => Machine.Fs,
         Vs   => Machine.Vs,
         Mem  => Machine);
      Machine.Ticks := Machine.Ticks + 1;
   end Step;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Machine : in out Athena_Machine;
      Address : Address_Type;
      Process :        not null access procedure
        (Cell : in out Athena.VM.Cells.Cell_Type))
   is
   begin
      Machine.Drivers (Natural (Address / 256))
        .Update (Page_Address_Type (Address mod 256), Process);
   end Update;

   -------------------
   -- Update_Driver --
   -------------------

   procedure Update_Driver
     (Machine : in out Athena_Machine'Class;
      Base    : Address_Type;
      Process : not null access
        procedure (Driver : in out Athena.VM.Drivers.Driver_Interface'Class))
   is
   begin
      Process (Machine.Drivers (Natural (Base / 256)));
   end Update_Driver;

end Athena.VM.Machine;
