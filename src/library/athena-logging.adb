with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Text_IO;

with Athena.Options;

package body Athena.Logging is

   Logging_Enabled : Boolean := False;
   Log_File        : Ada.Text_IO.File_Type;

   -------------------
   -- Finish_Update --
   -------------------

   procedure Finish_Update is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Finish_Update;

   ---------
   -- Log --
   ---------

   procedure Log
     (Message  : String)
   is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Put_Line
           (Log_File,
            Ada.Calendar.Formatting.Image
              (Ada.Calendar.Clock, True)
            & Character'Val (9)
            & Message);
      end if;
   end Log;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging
     (Log_Name : String)
   is
      Log_Directory : constant String :=
                        Athena.Options.Log_Folder;
      Log_File_Name : constant String :=
                        "athena"
                        & (if Log_Name = "" then ""
                           else "-" & Log_Name)
                        & ".log";
   begin
      if not Ada.Directories.Exists (Log_Directory) then
         Ada.Directories.Create_Directory (Log_Directory);
      end if;
      Ada.Text_IO.Create (Log_File, Ada.Text_IO.Out_File,
                          Ada.Directories.Compose
                            (Log_Directory, Log_File_Name));
      Logging_Enabled := True;
   end Start_Logging;

   ------------------
   -- Start_Update --
   ------------------

   procedure Start_Update is
   begin
      null;
   end Start_Update;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Close (Log_File);
         Logging_Enabled := False;
      end if;
   end Stop_Logging;

end Athena.Logging;
