with Athena.Signals;

package Athena.Server is

   procedure Initialize;

   procedure Create_Scenario;

   procedure Add_Empire
     (Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : String);

   function Add_Handler
     (Signal      : Athena.Signals.Signal_Type;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class;
      Handler     : Athena.Signals.Signal_Handler_Interface'Class)
      return Athena.Signals.Handler_Id;

   procedure Emit
     (Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal      : Athena.Signals.Signal_Type;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class);

end Athena.Server;
