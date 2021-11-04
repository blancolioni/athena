with Athena.VM.Cells;

package Athena.VM.Drivers is

   type Driver_Interface is interface;

   function Get
     (Driver  : Driver_Interface;
      Address : Page_Address_Type)
      return Athena.VM.Cells.Cell_Type
      is abstract;

   procedure Set
     (Driver  : in out Driver_Interface;
      Address : Page_Address_Type;
      Cell    : Athena.VM.Cells.Cell_Type)
   is abstract;

   procedure Update
     (Driver  : in out Driver_Interface;
      Address : Page_Address_Type;
      Process : not null access
        procedure (Cell : in out Athena.VM.Cells.Cell_Type))
   is abstract;

end Athena.VM.Drivers;
