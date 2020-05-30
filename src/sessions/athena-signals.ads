private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with WL.String_Maps;

package Athena.Signals is

   type Handler_Id is private;

   Null_Handler_Id : constant Handler_Id;

   function Image (Id : Handler_Id) return String;

   type Signal_Type (<>) is private;

   function Signal (Identifier : String) return Signal_Type;

   function Clock_Tick return Signal_Type;

   procedure Create_Signal
     (Identifier : String);

   type Signal_Source_Interface is interface;

   function Any_Source return Signal_Source_Interface'Class;

   type User_Data_Interface is interface;

   function Null_User_Data return User_Data_Interface'Class;

   type Signal_Data_Interface is interface;

   function Null_Signal_Data return Signal_Data_Interface'Class;

   type Signal_Handler_Interface is interface;

   function Handle
     (Handler     : Signal_Handler_Interface;
      Source      : Signal_Source_Interface'Class;
      Signal_Data : Signal_Data_Interface'Class;
      User_Data   : User_Data_Interface'Class)
      return Boolean
      is abstract;

   type Signal_Dispatch_Interface is interface;

   function Add_Handler
     (Container   : in out Signal_Dispatch_Interface;
      Signal      : Signal_Type;
      Source      : Signal_Source_Interface'Class;
      User_Data   : User_Data_Interface'Class;
      Handler     : Signal_Handler_Interface'Class)
      return Handler_Id
      is abstract;

   procedure Emit
     (Container   : Signal_Dispatch_Interface;
      Source      : Signal_Source_Interface'Class;
      Signal      : Signal_Type;
      Signal_Data : Signal_Data_Interface'Class)
   is abstract;

   type Signal_Handler_Container is
     new Signal_Dispatch_Interface with private;

   overriding function Add_Handler
     (Container   : in out Signal_Handler_Container;
      Signal      : Signal_Type;
      Source      : Signal_Source_Interface'Class;
      User_Data   : User_Data_Interface'Class;
      Handler     : Signal_Handler_Interface'Class)
      return Handler_Id;

   overriding procedure Emit
     (Container   : Signal_Handler_Container;
      Source      : Signal_Source_Interface'Class;
      Signal      : Signal_Type;
      Signal_Data : Signal_Data_Interface'Class);

private

   type Handler_Id is new Natural;

   Null_Handler_Id : constant Handler_Id := 0;

   function Image (Id : Handler_Id) return String
   is (Id'Image);

   type Signal_Type (Name_Length : Natural) is
      record
         Name : String (1 .. Name_Length);
      end record;

   package Signal_Maps is
     new WL.String_Maps (Signal_Type);

   package Signal_Handler_Holders is
     new Ada.Containers.Indefinite_Holders (Signal_Handler_Interface'Class);

   package Signal_Source_Holders is
     new Ada.Containers.Indefinite_Holders (Signal_Source_Interface'Class);

   package User_Data_Holders is
     new Ada.Containers.Indefinite_Holders (User_Data_Interface'Class);

   type Signal_Handler_Record is
      record
         Id        : Handler_Id;
         Source    : Signal_Source_Holders.Holder;
         User_Data : User_Data_Holders.Holder;
         Handler   : Signal_Handler_Holders.Holder;
      end record;

   package Signal_Handler_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Signal_Handler_Record);

   package Signal_Handler_Maps is
     new WL.String_Maps (Signal_Handler_Lists.List, Signal_Handler_Lists."=");

   type Signal_Handler_Container is
     new Signal_Dispatch_Interface with
      record
         Next_Id : Handler_Id := 1;
         Map     : Signal_Handler_Maps.Map;
      end record;

end Athena.Signals;
