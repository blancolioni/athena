package Athena.Updates is

   type Update_Interface is interface;

   procedure Activate
     (Update : Update_Interface)
   is abstract;

end Athena.Updates;
