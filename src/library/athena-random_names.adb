with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

with Athena.Paths;

package body Athena.Random_Names is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   Last_Names : String_Vectors.Vector;
   Female_Names : String_Vectors.Vector;
   Male_Names   : String_Vectors.Vector;

   package Name_Random is
     new Ada.Numerics.Discrete_Random (Natural);

   Gen : Name_Random.Generator;

   function R (Max : Positive) return Positive
   is (Name_Random.Random (Gen) mod Max + 1);

   function Random_Element (V : String_Vectors.Vector) return String
   is (V.Element (R (V.Last_Index)));

   ----------------
   -- Load_Names --
   ----------------

   procedure Load_Names is

      procedure Load
        (V    : in out String_Vectors.Vector;
         Name : String);

      function Normalize (S : String) return String;

      ----------
      -- Load --
      ----------

      procedure Load
        (V    : in out String_Vectors.Vector;
         Name : String)
      is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File,
               Athena.Paths.Config_File ("names/" & Name & ".txt"));
         while not End_Of_File (File) loop
            declare
               S : constant String := Get_Line (File);
               T : constant String := Normalize (S);
            begin
               if T /= "" then
                  V.Append (T);
               end if;
            end;
         end loop;
         Close (File);
      end Load;

      ---------------
      -- Normalize --
      ---------------

      function Normalize (S : String) return String is
         use Ada.Characters.Handling;
         Cap    : Boolean  := True;
         Result : String := S;
         Last   : Positive := Result'First;
      begin
         while Last <= Result'Last loop
            declare
               Ch : constant Character := Result (Last);
            begin
               if Is_Letter (Ch) then
                  if Cap and then Is_Lower (Ch) then
                     Result (Last) := To_Upper (Ch);
                  elsif not Cap and then Is_Upper (Ch) then
                     Result (Last) := To_Lower (Ch);
                  end if;
               elsif Is_Space (Ch) then
                  return Result (Result'First .. Last);
               end if;

               Cap := False;
               Last := Last + 1;
            end;
         end loop;

         return Result;
      end Normalize;

   begin
      Name_Random.Reset (Gen, 0);
      Load (Last_Names, "last-names");
      Load (Female_Names, "female-first-names");
      Load (Male_Names, "male-first-names");
   end Load_Names;

   ------------------------
   -- Random_Female_Name --
   ------------------------

   function Random_Female_Name return String is
   begin
      return Random_Element (Female_Names);
   end Random_Female_Name;

   ----------------------
   -- Random_Last_Name --
   ----------------------

   function Random_Last_Name return String is
   begin
      return Random_Element (Last_Names);
   end Random_Last_Name;

   ----------------------
   -- Random_Male_Name --
   ----------------------

   function Random_Male_Name return String is
   begin
      return Random_Element (Male_Names);
   end Random_Male_Name;

end Athena.Random_Names;
