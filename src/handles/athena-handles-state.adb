with Ada.Directories;
with Ada.Streams.Stream_IO;

with Athena.Options;

with Athena.Calendar;

with Athena.Handles.Colony;
with Athena.Handles.Component;
with Athena.Handles.Design;
with Athena.Handles.Design_Module;
with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.Hull;
with Athena.Handles.Hull_Armor;
with Athena.Handles.Knowledge;
with Athena.Handles.Module;
with Athena.Handles.Order;
with Athena.Handles.Relationship;
with Athena.Handles.Ship;
with Athena.Handles.Star;
with Athena.Handles.Technology;
with Athena.Handles.War;

package body Athena.Handles.State is

   function State_Path return String;

   ----------------
   -- Load_State --
   ----------------

   procedure Load_State is
      use Ada.Streams.Stream_IO;
      File   : File_Type;
   begin
      Open (File, In_File, State_Path);
      declare
         S : constant Stream_Access := Stream (File);
      begin
         Athena_Turn_Number'Read (S, Turn_Number);
         Object_Identifier'Read (S, Current_Identifier);

         declare
            Clock : Athena.Calendar.Time;
         begin
            Athena.Calendar.Time'Read (S, Clock);
            Athena.Calendar.Set_Clock (Clock);
         end;

         Athena.Handles.Colony.Load (S);
         Athena.Handles.Component.Load (S);
         Athena.Handles.Design.Load (S);
         Athena.Handles.Design_Module.Load (S);
         Athena.Handles.Empire.Load (S);
         Athena.Handles.Fleet.Load (S);
         Athena.Handles.Hull.Load (S);
         Athena.Handles.Hull_Armor.Load (S);
         Athena.Handles.Knowledge.Load (S);
         Athena.Handles.Module.Load (S);
         Athena.Handles.Order.Load (S);
         Athena.Handles.Relationship.Load (S);
         Athena.Handles.Ship.Load (S);
         Athena.Handles.Star.Load (S);
         Athena.Handles.Technology.Load (S);
         Athena.Handles.War.Load (S);
      end;
      Close (File);
   end Load_State;

   ---------------
   -- New_State --
   ---------------

   procedure New_State is
   begin
      Current_Identifier := "0AA00AA0";
   end New_State;

   ----------------
   -- Save_State --
   ----------------

   procedure Save_State is
      use Ada.Streams.Stream_IO;
      File : File_Type;
   begin
      Create (File, Out_File, State_Path);
      declare
         S : constant Stream_Access := Stream (File);
      begin
         Athena_Turn_Number'Write (S, Turn_Number);
         Object_Identifier'Write (S, Current_Identifier);
         Athena.Calendar.Time'Write (S, Athena.Calendar.Clock);
         Athena.Handles.Colony.Save (S);
         Athena.Handles.Component.Save (S);
         Athena.Handles.Design.Save (S);
         Athena.Handles.Design_Module.Save (S);
         Athena.Handles.Empire.Save (S);
         Athena.Handles.Fleet.Save (S);
         Athena.Handles.Hull.Save (S);
         Athena.Handles.Hull_Armor.Save (S);
         Athena.Handles.Knowledge.Save (S);
         Athena.Handles.Module.Save (S);
         Athena.Handles.Order.Save (S);
         Athena.Handles.Relationship.Save (S);
         Athena.Handles.Ship.Save (S);
         Athena.Handles.Star.Save (S);
         Athena.Handles.Technology.Save (S);
         Athena.Handles.War.Save (S);
      end;
      Close (File);
   end Save_State;

   ----------------
   -- State_Path --
   ----------------

   function State_Path return String is
      Games_Path   : constant String :=
                       Athena.Options.Game_Folder;
      Game_Id_Path : constant String :=
                       Games_Path & "/" & Athena.Options.Game_Id;
      File_Name    : constant String := "state.athena";
   begin
      if not Ada.Directories.Exists (Games_Path) then
         Ada.Directories.Create_Directory (Games_Path);
      end if;
      if not Ada.Directories.Exists (Game_Id_Path) then
         Ada.Directories.Create_Directory (Game_Id_Path);
      end if;
      return Game_Id_Path & "/" & File_Name;
   end State_Path;

end Athena.Handles.State;
