private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with WL.String_Maps;

package Athena.Signals is

   type Signal_Type is new String;

   type Signal_Data_Interface is interface;

   type Signaler is limited interface;

   procedure Send_Signal
     (Object : in out Signaler;
      Signal : Signal_Type)
   is abstract;

   type Handler_Id is private;

   Null_Handler_Id : constant Handler_Id;

   type Handler_Type is access
     procedure (Object : Signaler'Class;
                Data   : Signal_Data_Interface'Class);

   function Add_Handler
     (Object  : in out Signaler;
      Signal  : Signal_Type;
      Handler : Handler_Type;
      Data    : Signal_Data_Interface'Class)
      return Handler_Id
   is abstract;

   procedure Remove_Handler
     (Object  : in out Signaler;
      Signal  : Signal_Type;
      Id      : Handler_Id)
   is abstract;

   type Signal_Dispatcher is tagged private;

   procedure Call_Handlers
     (Dispatcher : Signal_Dispatcher;
      Object     : Signaler'Class;
      Signal     : Signal_Type);

   function Add_Handler
     (Dispatcher : in out Signal_Dispatcher;
      Signal     : Signal_Type;
      Handler    : Handler_Type;
      Data       : Signal_Data_Interface'Class)
      return Handler_Id;

   procedure Remove_Handler
     (Dispatcher : in out Signal_Dispatcher;
      Signal     : Signal_Type;
      Id         : Handler_Id);

private

   type Handler_Id is new Natural;

   Null_Handler_Id : constant Handler_Id := 0;

   package Data_Holder is
     new Ada.Containers.Indefinite_Holders
       (Signal_Data_Interface'Class);

   type Handler_Record is
      record
         Id      : Handler_Id;
         Handler : Handler_Type;
         Data    : Data_Holder.Holder;
      end record;

   package Handler_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Handler_Record);

   package Signal_Maps is
     new WL.String_Maps (Handler_Lists.List, Handler_Lists."=");

   type Signal_Dispatcher is tagged
      record
         Next_Id : Handler_Id := 1;
         Map     : Signal_Maps.Map;
      end record;

end Athena.Signals;
