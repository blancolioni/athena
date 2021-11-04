with Ada.Exceptions;
with Ada.Text_IO;

with Athena.Logging;
with Athena.Managers;
with Athena.Options;
with Athena.Server;
with Athena.Turns;
with Athena.Updates;

with Athena.UI.Launch;

with Minerva.Db.Database;

with Minerva.Empire;
with Minerva.Encounter;
with Minerva.Star;
with Minerva.Turn;

procedure Athena.Driver is
begin
   Athena.Server.Initialize;

   if Athena.Options.Create then
      Athena.Logging.Start_Logging ("create");
      Athena.Server.Create_Scenario;
      Athena.Logging.Stop_Logging;
      return;
   end if;

   if Athena.Options.Add_Empire then
      Minerva.Db.Database.Open;
      Athena.Server.Add_Empire
        (Name      => Athena.Options.Name,
         Plural    => Athena.Options.Plural,
         Adjective => Athena.Options.Adjective,
         Capital   => Athena.Options.Capital,
         Color     => Athena.Options.Color);
      Minerva.Db.Database.Close;
      return;
   end if;

   if Athena.Options.Update then
      Minerva.Db.Database.Open;

      Athena.Managers.Load_Managers;

      for I in 1 .. Athena.Options.Update_Count loop
         Ada.Text_IO.Put_Line
           ("starting update " & Athena.Turns.Current_Turn_Image);
         Athena.Updates.Run_Update;
         Ada.Text_IO.Put_Line ("update finished");
      end loop;
      Minerva.Db.Database.Close;
      return;
   end if;

   if Athena.Options.View_Encounter then

      if Athena.Options.Star_Name = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "missing option: --star-name");
         return;
      end if;

      if Athena.Options.Turn = 0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "missing option: --turn");
         return;
      end if;

      Minerva.Db.Database.Open;

      declare
         Star : constant Minerva.Star.Star_Class :=
                  Minerva.Star.First_By_Name
                    (Athena.Options.Star_Name);
         Turn : constant Minerva.Turn.Turn_Class :=
                  Athena.Turns.Get_Turn (Athena.Options.Turn);
         Encounter : constant Minerva.Encounter.Encounter_Class :=
                       Minerva.Encounter.Get_By_Encounter (Star, Turn);
      begin
         if not Star.Has_Element then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot find star: " & Athena.Options.Star_Name);
            return;
         elsif not Turn.Has_Element then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "no such turn: " & Athena.Options.Turn'Image);
            return;
         elsif not Encounter.Has_Element then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot find encounter at "
               & Star.Name
               & " on turn" & Athena.Options.Turn'Image);
            return;
         end if;

         declare
            UI : Athena.UI.Athena_User_Interface'Class :=
                   Athena.UI.Launch.Get_Encounter_UI
                     (Encounter);
         begin
            UI.Start;
         end;
      end;

      Minerva.Db.Database.Close;
      return;

   end if;

   Minerva.Db.Database.Open;
   declare
      Client_UI : Athena.UI.Athena_User_Interface'Class :=
                    Athena.UI.Launch.Get_UI
                      (Minerva.Empire.Get_By_Name
                         (Athena.Options.Empire_Name));
   begin
      Client_UI.Start;
   end;
   Minerva.Db.Database.Close;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("exception: " & Ada.Exceptions.Exception_Message (E));
      Minerva.Db.Database.Close;
      raise;
end Athena.Driver;
