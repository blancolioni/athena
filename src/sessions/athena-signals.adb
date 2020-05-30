with Athena.Signal_Store;

package body Athena.Signals is

   type Null_Signal_Data_Type is
     new Signal_Data_Interface with null record;

   type Null_User_Data_Type is
     new User_Data_Interface with null record;

   type Any_Signal_Source is
     new Signal_Source_Interface with null record;

   Signal_Clock_Tick  : constant String := "signal-clock-tick";
   Clock_Tick_Created : Boolean := False;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Container : in out Signal_Handler_Container;
      Signal    : Signal_Type;
      Source    : Signal_Source_Interface'Class;
      User_Data : User_Data_Interface'Class;
      Handler   : Signal_Handler_Interface'Class)
      return Handler_Id
   is
   begin
      if not Container.Map.Contains (Signal.Name) then
         Container.Map.Insert (Signal.Name, Signal_Handler_Lists.Empty_List);
      end if;

      declare
         List : Signal_Handler_Lists.List renames Container.Map (Signal.Name);
      begin
         List.Append
           (Signal_Handler_Record'
              (Id        => Container.Next_Id,
               Source    => Signal_Source_Holders.To_Holder (Source),
               User_Data => User_Data_Holders.To_Holder (User_Data),
               Handler   => Signal_Handler_Holders.To_Holder (Handler)));

         return Id         : constant Handler_Id := Container.Next_Id do
            Container.Next_Id := Container.Next_Id + 1;
         end return;
      end;
   end Add_Handler;

   ----------------
   -- Any_Source --
   ----------------

   function Any_Source return Signal_Source_Interface'Class is
   begin
      return Source : Any_Signal_Source;
   end Any_Source;

   ----------------
   -- Clock_Tick --
   ----------------

   function Clock_Tick return Signal_Type is
   begin
      if not Clock_Tick_Created then
         Create_Signal (Signal_Clock_Tick);
         Clock_Tick_Created := True;
      end if;
      return Signal (Signal_Clock_Tick);
   end Clock_Tick;

   -------------------
   -- Create_Signal --
   -------------------

   procedure Create_Signal (Identifier : String) is
      Signal : constant Signal_Type :=
                 Signal_Type'
                   (Name_Length => Identifier'Length,
                    Name        => Identifier);
   begin
      if Athena.Signal_Store.Exists (Identifier) then
         raise Constraint_Error with
           "signal already exists: " & Identifier;
      end if;

      Athena.Signal_Store.Add (Identifier, Signal);
   end Create_Signal;

   ----------
   -- Emit --
   ----------

   overriding procedure Emit
     (Container   : Signal_Handler_Container;
      Source      : Signal_Source_Interface'Class;
      Signal      : Signal_Type;
      Signal_Data : Signal_Data_Interface'Class)
   is
   begin
      if Container.Map.Contains (Signal.Name) then
         declare
            List : constant Signal_Handler_Lists.List :=
                     Container.Map (Signal.Name);
         begin
            for Item of List loop
               if Item.Handler.Element.Handle
                 (Source, Signal_Data, Item.User_Data.Element)
               then
                  exit;
               end if;
            end loop;
         end;
      end if;
   end Emit;

   ----------------------
   -- Null_Signal_Data --
   ----------------------

   function Null_Signal_Data return Signal_Data_Interface'Class is
   begin
      return Null_Signal_Data_Type'(null record);
   end Null_Signal_Data;

   --------------------
   -- Null_User_Data --
   --------------------

   function Null_User_Data return User_Data_Interface'Class is
   begin
      return Null_User_Data_Type'(null record);
   end Null_User_Data;

   ------------
   -- Signal --
   ------------

   function Signal (Identifier : String) return Signal_Type is
   begin
      return Athena.Signal_Store.Get (Identifier);
   end Signal;

end Athena.Signals;
