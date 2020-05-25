with WL.String_Maps;

with Athena.Logging;

with Athena.Handles.Empire;

--  with Athena.Handles.Manager;
--
--  with Athena.Managers.Attack;
with Athena.Managers.Colonization;
--  with Athena.Managers.Defend;
with Athena.Managers.Development;
with Athena.Managers.Exploration;
--  with Athena.Managers.Research;
with Athena.Managers.Transportation;
--  with Athena.Managers.Upgrade;
--
package body Athena.Managers is

   package Manager_Maps is
     new WL.String_Maps (Root_Manager_Type'Class);

   Manager_Map : Manager_Maps.Map;

   ------------
   -- Exists --
   ------------

   function Exists
     (Name   : String)
      return Boolean
   is
   begin
      return Manager_Map.Contains (Name);
   end Exists;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (Name   : String;
      Empire : Athena.Handles.Empire_Reference)
      return Root_Manager_Type'Class
   is
   begin
      return Manager : Root_Manager_Type'Class :=
        Manager_Map.Element (Name)
      do
         Manager.Initialize (Name, Empire);
      end return;
   end Get_Manager;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Manager : in out Root_Manager_Type;
      Name    : String;
      Empire  : Athena.Handles.Empire_Reference)
   is
   begin
      Manager.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Manager.Empire := Empire;
   end Initialize;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
   begin

      Manager_Map.Insert
        (Athena.Handles.Colonization_Manager'Image,
         Colonization.Default_Colonization_Manager);

      Manager_Map.Insert
        (Athena.Handles.Development_Manager'Image,
         Development.Default_Development_Manager);

      Manager_Map.Insert
        (Athena.Handles.Exploration_Manager'Image,
         Exploration.Default_Exploration_Manager);

      Manager_Map.Insert
        (Athena.Handles.Transport_Manager'Image,
         Transportation.Default_Transportation_Manager);

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
     (Manager : Root_Manager_Type'Class;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Athena.Handles.Empire.Get (Manager.Empire).Name
         & "/"
         & Manager.Identifier
         & ": "
         & Message);
   end Log;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (To      : in out Root_Manager_Type'Class;
      Message : Message_Type'Class)
   is
   begin
      To.Messages.Append (Message);
   end Send_Message;

end Athena.Managers;
