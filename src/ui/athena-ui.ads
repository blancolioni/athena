package Athena.UI is

   type Athena_User_Interface is interface;

   procedure Start
     (UI     : in out Athena_User_Interface)
   is abstract;

end Athena.UI;
