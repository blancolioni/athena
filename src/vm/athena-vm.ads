package Athena.VM is

   type Address_Type is mod 2 ** 16;
   type Page_Address_Type is mod 2 ** 8;

   type Register_Index is mod 16;

   type Status_Flag is (Negative, Zero, Error, Break);

   type Status_Word is array (Status_Flag) of Boolean;

end Athena.VM;
