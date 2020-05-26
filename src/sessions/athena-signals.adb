with Ada.Exceptions;
with Ada.Text_IO;

package body Athena.Signals is

   -----------------
   -- Add_Handler --
   -----------------

   function Add_Handler
     (Dispatcher : in out Signal_Dispatcher;
      Signal     : Signal_Type;
      Handler    : Handler_Type;
      Data       : Signal_Data_Interface'Class)
      return Athena.Signals.Handler_Id
   is
      S : constant String := String (Signal);
   begin
      if not Dispatcher.Map.Contains (S) then
         Dispatcher.Map.Insert (S, Handler_Lists.Empty_List);
      end if;
      return Id : constant Handler_Id := Dispatcher.Next_Id do
         Dispatcher.Next_Id := Dispatcher.Next_Id + 1;
         Dispatcher.Map (S).Append
           (Handler_Record'
              (Id      => Id,
               Handler => Handler,
               Data    => Data_Holder.To_Holder (Data)));
      end return;
   end Add_Handler;

   -------------------
   -- Call_Handlers --
   -------------------

   procedure Call_Handlers
     (Dispatcher : Signal_Dispatcher;
      Object     : Signaler'Class;
      Signal     : Signal_Type)
   is
      S        : constant String := String (Signal);
   begin
      if Dispatcher.Map.Contains (S) then
         declare
            List : constant Handler_Lists.List :=
                     Dispatcher.Map.Element (S);
         begin
            for Handler of List loop
               begin
                  Handler.Handler (Object, Handler.Data.Element);
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "exception while calling handler"
                        & Handler.Id'Image
                        & " for "
                        & String (Signal)
                        & ": "
                        & Ada.Exceptions.Exception_Message (E));
               end;
            end loop;
         end;
      end if;
   end Call_Handlers;

   --------------------
   -- Remove_Handler --
   --------------------

   procedure Remove_Handler
     (Dispatcher : in out Signal_Dispatcher;
      Signal     : Signal_Type;
      Id         : Handler_Id)
   is
      S : constant String := String (Signal);
      List : Handler_Lists.List renames Dispatcher.Map (S);
      Pos  : Handler_Lists.Cursor := Handler_Lists.No_Element;
   begin
      for Position in List.Iterate loop
         if Handler_Lists.Element (Position).Id = Id then
            Pos := Position;
            exit;
         end if;
      end loop;

      if Handler_Lists.Has_Element (Pos) then
         List.Delete (Pos);
      else
         raise Constraint_Error with
           "no such handler" & Id'Image & " for signal " & S;
      end if;
   end Remove_Handler;

end Athena.Signals;
