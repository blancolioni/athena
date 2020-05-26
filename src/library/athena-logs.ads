package Athena.Logs is

   type Log_Interface is limited interface;

   function Path (Log : Log_Interface) return String is abstract;

   function Field_Count (Log : Log_Interface) return Natural is abstract;
   function Heading
     (Log   : Log_Interface;
      Index : Positive)
      return String
      is abstract
     with Pre'Class => Index <= Log_Interface'Class (Log).Field_Count;

   function Value
     (Log   : Log_Interface;
      Index : Positive)
      return String
      is abstract
     with Pre'Class => Index <= Log_Interface'Class (Log).Field_Count;

   procedure Log
     (Item : Log_Interface'Class);

   procedure Log_Fields
     (Log_Path  : String;
      Field_1   : String;
      Field_2   : String := "";
      Field_3   : String := "";
      Field_4   : String := "";
      Field_5   : String := "";
      Field_6   : String := "";
      Field_7   : String := "";
      Field_8   : String := "";
      Field_9   : String := "";
      Field_10  : String := "";
      Field_11  : String := "";
      Field_12  : String := "";
      Field_13  : String := "";
      Field_14  : String := "";
      Field_15  : String := "";
      Field_16  : String := "");

   procedure Flush_Logs (Show_Console_Progress : Boolean);

end Athena.Logs;
