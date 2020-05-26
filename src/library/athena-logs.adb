with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Ada.Text_IO;

with WL.Processes;
with WL.String_Maps;

with Athena.Options;

package body Athena.Logs is

   Sep : constant Character := Character'Val (9);
   Ext : constant String := "csv";

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Log_File_Maps is
     new WL.String_Maps (String_Lists.List, String_Lists."=");

   Log_Files : Log_File_Maps.Map;

   procedure Ensure_Path
     (Base_Path  : String;
      Local_Path : String);

   procedure Append_Line
     (Log_Path    : String;
      Field_Count : Natural;
      Field       : not null access
        function (Index : Positive) return String);

   -----------------
   -- Append_Line --
   -----------------

   procedure Append_Line
     (Log_Path    : String;
      Field_Count : Natural;
      Field       : not null access
        function (Index : Positive) return String)
   is
      use Ada.Strings.Unbounded;
      Line : Unbounded_String := Null_Unbounded_String;
   begin
      for I in 1 .. Field_Count loop
         Line := Line & Sep & Field (I);
      end loop;
      Log_Files (Log_Path).Append
        ("log"
         & To_String (Line));
   end Append_Line;

   -----------------
   -- Ensure_Path --
   -----------------

   procedure Ensure_Path
     (Base_Path  : String;
      Local_Path : String)
   is
      use Ada.Strings.Fixed;
      Slash : constant Natural := Index (Local_Path, "/");
   begin
      if Slash > 0 then
         declare
            Folder_Name : constant String :=
                            Local_Path (Local_Path'First .. Slash - 1);
            Folder_Path : constant String :=
                            Ada.Directories.Compose
                              (Base_Path, Folder_Name);
         begin
            if not Ada.Directories.Exists (Folder_Path) then
               Ada.Directories.Create_Directory (Folder_Path);
            end if;
            Ensure_Path (Folder_Path,
                         Local_Path (Slash + 1 .. Local_Path'Last));
         end;
      end if;
   end Ensure_Path;

   ----------------
   -- Flush_Logs --
   ----------------

   procedure Flush_Logs (Show_Console_Progress : Boolean) is
      Base_Path : constant String := Athena.Options.Log_Folder;
      Process : WL.Processes.Process_Type;
   begin
      if Log_Files.Is_Empty then
         return;
      end if;

      if Show_Console_Progress then
         Process.Start_Percentage
           ("Writing log files", Positive (Log_Files.Length));
      end if;

      for Position in Log_Files.Iterate loop
         declare
            use Ada.Text_IO;
            Local_Path : constant String :=
                           Log_File_Maps.Key (Position);
            Full_Path  : constant String :=
                           Base_Path & "/" & Local_Path & "." & Ext;
            File       : File_Type;
         begin
            Ensure_Path (Base_Path, Local_Path);
            Create (File, Out_File, Full_Path);
            for Line of Log_File_Maps.Element (Position) loop
               Put_Line (File, Line);
            end loop;
            Close (File);
            if Show_Console_Progress then
               Process.Tick;
            end if;
         end;
      end loop;
      if Show_Console_Progress then
         Process.Finish;
      end if;
      Log_Files.Clear;

   end Flush_Logs;

   procedure Log
     (Item : Log_Interface'Class)
   is
      Log_Path : constant String := Item.Path;
   begin
      if not Log_Files.Contains (Log_Path) then
         Log_Files.Insert (Log_Path, String_Lists.Empty_List);

         declare
            function Heading (Index : Positive) return String
            is (Item.Heading (Index));
         begin
            Append_Line (Log_Path, Item.Field_Count, Heading'Access);
         end;
      end if;

      declare
         function Field (Index : Positive) return String
         is (Item.Value (Index));
      begin
         Append_Line (Log_Path, Item.Field_Count, Field'Access);
      end;

   end Log;

   ----------------
   -- Log_Fields --
   ----------------

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
      Field_16  : String := "")
   is
   begin
      if not Log_Files.Contains (Log_Path) then
         Log_Files.Insert (Log_Path, String_Lists.Empty_List);
      end if;

      declare
         function Field (Field : String) return String
         is (if Field = "" then "" else Sep & Field);

         Line : constant String :=
                  Field_1
                  & Field (Field_2) & Field (Field_3)
                  & Field (Field_4) & Field (Field_5)
                  & Field (Field_6) & Field (Field_7)
                  & Field (Field_8) & Field (Field_9)
                  & Field (Field_10) & Field (Field_11)
                  & Field (Field_12) & Field (Field_13)
                  & Field (Field_14) & Field (Field_15)
                  & Field (Field_16);
      begin
         Log_Files (Log_Path).Append
           (Line);
      end;
   end Log_Fields;

end Athena.Logs;
