with WL.Localisation;

with Athena.Logging;

package body Athena.Handles is

   ------------------------
   -- Current_Turn_Image --
   ------------------------

   function Current_Turn_Image return String is
      Image : constant String := Turn_Number'Image;
   begin
      return Image (2 .. Image'Last);
   end Current_Turn_Image;

   --------------------------
   -- File_Name_Turn_Image --
   --------------------------

   function File_Name_Turn_Image
     return String is
      Image : String := "0000";
      Raw   : constant String := Turn_Number'Image;
      Index : Natural := Image'Last;
   begin
      for Ch of reverse Raw loop
         exit when Ch not in '0' .. '9';
         Image (Index) := Ch;
         Index := Index - 1;
      end loop;
      return Image;
   end File_Name_Turn_Image;

   ----------------
   -- Local_Text --
   ----------------

   function Local_Text
     (Localised : Localised_Interface'Class)
      return String
   is
   begin
      return WL.Localisation.Local_Text (Localised.Tag);
   end Local_Text;

   ---------
   -- Log --
   ---------

   procedure Log
     (Handle  : Root_Athena_Handle'Class;
      Message : String)
   is
   begin
      Athena.Logging.Log (Handle.Short_Name & ": " & Message);
   end Log;

   ---------------------
   -- Next_Identifier --
   ---------------------

   function Next_Identifier return Object_Identifier is
   begin
      return Result : constant Object_Identifier :=
        Current_Identifier
      do
         for Ch of reverse Current_Identifier loop
            if Ch = 'Z' then
               Ch := 'A';
            elsif Ch = '9' then
               Ch := '0';
            else
               Ch := Character'Succ (Ch);
               exit;
            end if;
         end loop;
      end return;
   end Next_Identifier;

   ---------------
   -- Next_Turn --
   ---------------

   procedure Next_Turn is
   begin
      Turn_Number := Turn_Number + 1;
   end Next_Turn;

end Athena.Handles;
