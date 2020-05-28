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

with Athena.Managers;

--  with Athena.Colonies;
with Athena.Empires.Create;
--  with Athena.Ships.Scripts;
--  with Athena.Stars;

--  with Athena.Db.Database;

with Athena.Handles.Star;
--  with Athena.Db.Star;

--  with Athena.Handles.Empire;
--  with Athena.Handles.Star_Distance.Selections;

with Athena.Handles.State;

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
        (Handle : Athena.Handles.Star.Star_Handle)
         return Boolean;

      function Check_Star
        (Handle : Athena.Handles.Star.Star_Handle)
         return Boolean
      is
      begin
         if Handle.Has_Owner
           or else Handle.Space < 2500
           or else Handle.Resource not in 0.5 .. 1.0
           or else Handle.Habitability not in 0.5 .. 1.0
         then
            return False;
         end if;

         --  for (Neighbour : Athena.Handles.Star.Star_Handle) of
         --    Athena.Handles.Star.Iterate_Nearest_Stars
         --      (Handle, 20.0)
         --  loop
         --     if Neighbour.Owner.Has_Element then
         --        return False;
         --     end if;
         --  end loop;

         declare
            Found_Empire : Boolean := False;

            function Check_Neighbour
              (Neighbour : Athena.Handles.Star.Star_Handle)
               return Boolean;

            ---------------------
            -- Check_Neighbour --
            ---------------------

            function Check_Neighbour
              (Neighbour : Athena.Handles.Star.Star_Handle)
               return Boolean
            is
            begin
               if Neighbour.Has_Owner then
                  Found_Empire := True;
                  return False;
               end if;
               return True;
            end Check_Neighbour;

         begin
            Handle.Iterate_Nearest_Stars
              (Max_Distance => 25.0,
               Process      => Check_Neighbour'Access);

            if Found_Empire then
               return False;
            end if;
         end;

         return True;

      end Check_Star;

      Home : constant Athena.Handles.Star.Star_Handle :=
               Athena.Handles.Star.Find_Star (Check_Star'Access);
      Template : constant Tropos.Configuration :=
                   Tropos.Reader.Read_Config
                     (Athena.Paths.Config_File
                        ("templates/empires/default-empire.template"));
   begin

      if not Home.Has_Element then
         Ada.Text_IO.Put_Line
           (Name & ": unable to find home world");
         return;
      end if;

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
      Athena.Handles.State.New_State;
      Database_Open := True;

      Athena.Configure.Initialize_Database;

      Athena.Configure.Create_Galaxy
        (Radius_X       => Non_Negative_Real (Radius_X),
         Radius_Y       => Non_Negative_Real (Radius_Y),
         Star_Count     => Athena.Options.Star_Count,
         Name_Generator => Name_Generator);

      Athena.Handles.State.Save_State;
      Database_Open := False;

   exception

      when others =>
         if Database_Open then
            Athena.Handles.State.Save_State;
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

      Athena.Managers.Load_Managers;

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
