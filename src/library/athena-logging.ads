package Athena.Logging is

   procedure Log
     (Message  : String);

   procedure Start_Logging
     (Log_Name : String);

   procedure Stop_Logging;

   procedure Start_Update;
   procedure Finish_Update;

end Athena.Logging;
