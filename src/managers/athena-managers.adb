with WL.String_Maps;

with Athena.Logging;

with Minerva.Manager;

with Athena.Managers.Attack;
with Athena.Managers.Colonization;
with Athena.Managers.Defend;
with Athena.Managers.Development;
with Athena.Managers.Exploration;
with Athena.Managers.Research;
with Athena.Managers.Transportation;
with Athena.Managers.Upgrade;

package body Athena.Managers is

   package Manager_Maps is
     new WL.String_Maps (Athena_Manager_Script'Class);

   Manager_Map : Manager_Maps.Map;

   package Message_Maps is
     new WL.String_Maps (Message_Lists.List, Message_Lists."=");

   Message_Map : Message_Maps.Map;

   ------------
   -- Exists --
   ------------

   function Exists
     (Class  : String;
      Name   : String)
      return Boolean
   is
   begin
      return Manager_Map.Contains (Class & "--" & Name);
   end Exists;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (Class  : String;
      Name   : String;
      Empire : Minerva.Empire.Empire_Class)
      return Athena_Manager_Script'Class
   is
      Key : constant String := Class & "--" & Name;
      Message_Key : constant String :=
                      Class & "--" & Empire.Identifier;
      Manager : constant Minerva.Empire_Manager.Empire_Manager_Class :=
                  Minerva.Empire_Manager.Get_By_Empire_Manager
                    (Empire  => Empire,
                     Manager => Minerva.Manager.Get_By_Tag (Name));
   begin
      return Script : Athena_Manager_Script'Class :=
        Manager_Map.Element (Key)
      do
         Script.Initialize (Manager, Name, Empire);
         if Message_Map.Contains (Message_Key) then
            declare
               Msgs : Message_Lists.List renames Message_Map (Message_Key);
            begin
               for Msg of Msgs loop
                  Script.Process_Message (Msg);
               end loop;
            end;
            Message_Map.Delete (Message_Key);
         end if;
      end return;
   end Get_Manager;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Script  : in out Athena_Manager_Script;
      Manager : Minerva.Empire_Manager.Empire_Manager_Class;
      Name    : String;
      Empire  : Minerva.Empire.Empire_Class)
   is
   begin
      Script.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Script.Manager := Manager.To_Empire_Manager_Handle;
      Script.Empire := Empire.To_Empire_Handle;
   end Initialize;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
   begin

      --  Manager_Map.Insert
      --    (Minerva.Colonization_Manager'Image,
      --     Colonization.Default_Colonization_Manager);

      Manager_Map.Insert
        ("attack--attack",
         Attack.Default_Attack_Manager);

      Manager_Map.Insert
        ("develop--develop",
         Development.Default_Development_Manager);

      Manager_Map.Insert
        ("colonize--colonize",
         Colonization.Default_Colonization_Manager);

      Manager_Map.Insert
        ("defend--defend",
         Defend.Default_Defend_Manager);

      Manager_Map.Insert
        ("explore--explore",
         Exploration.Default_Exploration_Manager);

      Manager_Map.Insert
        ("transport--transport",
         Transportation.Default_Transportation_Manager);

      Manager_Map.Insert
        ("research--research",
         Research.Default_Research_Manager);

      Manager_Map.Insert
        ("upgrade--upgrade",
         Upgrade.Default_Upgrade_Manager);

      --     Manager_Orders.Insert
   --       ("attack", Athena.Managers.Attack.Create_Orders'Access);
   --     Manager_Orders.Insert
   --       ("colonize", Athena.Managers.Colonization.Create_Orders'Access);
   --     Manager_Orders.Insert
   --       ("defend", Athena.Managers.Defend.Create_Orders'Access);
   --     Manager_Orders.Insert
   --       ("develop", Athena.Managers.Development.Create_Orders'Access);
   --     Manager_Orders.Insert
   --       ("explore", Athena.Managers.Exploration.Create_Orders'Access);
   --     Manager_Orders.Insert
   --       ("research", Athena.Managers.Research.Create_Orders'Access);
   --     Manager_Orders.Insert
   --     ("transport", Athena.Managers.Transportation.Create_Orders'Access);
   --     Manager_Orders.Insert
   --       ("upgrade", Athena.Managers.Upgrade.Create_Orders'Access);
   end Load_Managers;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Athena_Manager_Script'Class;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Manager.Empire.Name
         & "/"
         & Manager.Identifier
         & ": "
         & Message);
   end Log;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Destination : String;
      Empire      : Minerva.Empire.Empire_Class;
      Message     : Message_Type'Class)
   is
      Key : constant String :=
              Destination & "--" & Empire.Identifier;
   begin
      if not Message_Map.Contains (Key) then
         Message_Map.Insert (Key, Message_Lists.Empty_List);
      end if;
      Message_Map (Key).Append (Message);
   end Send_Message;

end Athena.Managers;
