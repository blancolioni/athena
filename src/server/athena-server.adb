with Ada.Text_IO;
with Ada.Directories;

with WL.Command_Line;
with WL.Localisation;
with WL.Random.Names;

with Tropos.Reader;

with Athena.Color;
with Athena.Configure;
with Athena.Options;
with Athena.Paths;
with Athena.Random_Names;
with Athena.Real_Images;
with Athena.Ships.Designs;

with Athena.Empires.Create;

with Athena.Configure.Ships;

with Minerva.Ship_Design;
with Minerva.Star;
with Minerva.Star_Distance;

with Minerva.Db.Database;

package body Athena.Server is

   Name_Generator    : WL.Random.Names.Name_Generator;

   ----------------
   -- Add_Empire --
   ----------------

   procedure Add_Empire
     (Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : String)
   is

      function Check_Star
        (Handle : Minerva.Star.Star_Class)
         return Boolean;

      ----------------
      -- Check_Star --
      ----------------

      function Check_Star
        (Handle : Minerva.Star.Star_Class)
         return Boolean
      is
      begin
         if Handle.Owner.Has_Element
           or else Handle.Space not in 1000 .. 2500
           or else Handle.Resource not in 0.5 .. 1.0
           or else Handle.Habitability not in 0.5 .. 1.0
         then
            return False;
         end if;

         for Neighbour of
           Minerva.Star_Distance.Select_Closest_Stars_Bounded_By_Distance
             (Handle, 0.0, 25.0)
         loop
            if Neighbour.To.Owner.Has_Element then
               return False;
            end if;
         end loop;

         return True;

      end Check_Star;

      Home : Minerva.Star.Star_Handle;

   begin
      for Star of Minerva.Star.Scan_By_Core_Distance loop
         if Check_Star (Star) then
            Home := Star.To_Star_Handle;
            exit;
         end if;
      end loop;

      if not Home.Has_Element then
         Ada.Text_IO.Put_Line
           (Name & ": unable to find home world");
         return;
      end if;

      declare
         Template : constant Tropos.Configuration :=
                      Tropos.Reader.Read_Config
                        (Athena.Paths.Config_File
                           ("templates/empires/default-empire.template"));
      begin

         Athena.Empires.Create.New_Empire
           (Star      => Home,
            Name      => Name,
            Plural    => (if Plural = "" then Name else Plural),
            Adjective => (if Adjective = "" then Name else Adjective),
            Capital   => (if Capital = "" then Name else Capital),
            Color     => Athena.Color.From_String (Color),
            Template  => Template);

         Ada.Text_IO.Put_Line
           (Name & " founded at " & Home.Name);
      end;

   end Add_Empire;

   ---------------------
   -- Create_Scenario --
   ---------------------

   procedure Create_Scenario is
      Database_Open : Boolean := False;
      Radius_X      : constant Natural :=
                        Natural'Max
                          (Athena.Options.Galaxy_Radius_X,
                           Athena.Options.Galaxy_Radius);
      Radius_Y      : constant Natural :=
                        Natural'Max (Athena.Options.Galaxy_Radius_Y,
                                     Athena.Options.Galaxy_Radius);

   begin
      Minerva.Db.Database.Create;
      Database_Open := True;

      Athena.Configure.Initialize_Database;

      Athena.Configure.Ships.Configure_Designs
        (Designs_Config =>
           Tropos.Reader.Read_Config
             (Athena.Paths.Config_File ("ships/designs"), "design"));

      for Design of Minerva.Ship_Design.Scan_By_Name loop
         Ada.Text_IO.Put_Line
           (Design.Name & " dry mass "
            & Athena.Real_Images.Approximate_Image
              (Athena.Ships.Designs.Dry_Mass (Design))
            & "t; max speed "
            & Athena.Real_Images.Approximate_Image
              (Athena.Ships.Designs.Maximum_Speed (Design, 1.0)));
      end loop;

      Athena.Configure.Create_Galaxy
        (Radius_X       => Non_Negative_Real (Radius_X),
         Radius_Y       => Non_Negative_Real (Radius_Y),
         Star_Count     => Athena.Options.Star_Count,
         Name_Generator => Name_Generator);

      Minerva.Db.Database.Close;
      Database_Open := False;

   exception

      when others =>
         if Database_Open then
            Minerva.Db.Database.Close;
         end if;
         raise;

   end Create_Scenario;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      if not Ada.Directories.Exists (".athena-options") then
         Ada.Directories.Copy_File
           (Source_Name => Athena.Paths.Config_File ("default-options.txt"),
            Target_Name => ".athena-options");
      end if;

      WL.Command_Line.Load_Defaults (".athena-options");

      WL.Localisation.Read_Localisation
        (Athena.Paths.Config_File
           ("localisation/" & Athena.Options.Language & ".txt"));

      WL.Random.Names.Load_Lexicon
        (Name_Generator,
         Athena.Paths.Config_File ("totro-vowels.txt"),
         Athena.Paths.Config_File ("totro-consonants.txt"));

      if Athena.Options.Randomise then
         WL.Random.Randomise;
      end if;

      Athena.Random_Names.Load_Names;

   end Initialize;

   ----------
   -- Load --
   ----------

   --  procedure Load is
   --  begin
   --     Athena.Stars.Load_Stars;
   --     Athena.Colonies.Load_Colonies;
   --     Athena.Ships.Load_Ships;
   --     Athena.Managers.Load_Managers;
   --     Athena.Ships.Scripts.Load_Standard_Scripts;
   --  end Load;
   --
   --  procedure Save is
   --  begin
   --     null;
   --  end Save;
   --
end Athena.Server;
