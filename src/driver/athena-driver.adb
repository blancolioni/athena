with Ada.Calendar;
with Ada.Text_IO;

with WL.Processes;

with Athena.Options;

with Athena.Calendar;
with Athena.Logging;
with Athena.Logs;
with Athena.Real_Images;

with Athena.Server;

--  with Athena.Encounters;
with Athena.Updates.Control;

--  with Athena.Handles.Empire;
--  with Athena.Handles.Encounter;

with Athena.UI.Launch;

with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.State;

with Athena.Reports.Empires;

procedure Athena.Driver is

   --  Database_Open : Boolean := False;
   --
begin

   Athena.Server.Initialize;

   if Athena.Options.Create then
      Athena.Logging.Start_Logging ("create");
      Athena.Server.Create_Scenario;
      Athena.Logging.Stop_Logging;
      return;
   end if;

   if Athena.Options.Report_Designs then
      Athena.Handles.State.Load_State;

      declare
         procedure Report_Design
           (Handle : Athena.Handles.Design.Design_Handle);

         -------------------
         -- Report_Design --
         -------------------

         procedure Report_Design
           (Handle : Athena.Handles.Design.Design_Handle)
         is
         begin
            Ada.Text_IO.Put_Line
              (Handle.Name
               & ": tonnage"
               & Natural'Image (Natural (Handle.Tonnage))
               & "; cargo space"
               & Natural'Image (Natural (Handle.Cargo_Space)));
         end Report_Design;

      begin
         Athena.Handles.Design.Iterate (Report_Design'Access);
      end;
      return;
   end if;

   if Athena.Options.Report_Empire then

      if Athena.Options.Empire_Name = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "missing required argument --empire-name");
         return;
      end if;

      Athena.Handles.State.Load_State;

      declare
         Empire : constant Athena.Handles.Empire.Empire_Handle :=
           Athena.Handles.Empire.Get_By_Name
             (Athena.Options.Empire_Name);
         Writer : Athena.Reports.Writer_Interface'Class :=
           Athena.Reports.Standard_Writer;
      begin

         if not Empire.Has_Element then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "cannot find an empire called " & Athena.Options.Empire_Name);
            return;
         end if;

         Athena.Reports.Empires.Report (Writer, Empire);
         return;
      end;
   end if;

   if Athena.Options.Add_Empire then
      declare
         Name : constant String := Athena.Options.Name;
      begin
         if Name = "" then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "missing argument: name");
            return;
         end if;

         Athena.Logging.Start_Logging ("add-empire");

         Athena.Handles.State.Load_State;

         Athena.Server.Add_Empire
           (Name      => Name,
            Plural    => Athena.Options.Plural,
            Adjective => Athena.Options.Adjective,
            Capital   => Athena.Options.Capital,
            Color     => Athena.Options.Color);

         Athena.Handles.State.Save_State;

      end;
      return;
   end if;

   if Athena.Options.Update then

      Athena.Logging.Start_Logging ("update");
      Athena.Handles.State.Load_State;

      declare
         Process     : WL.Processes.Process_Type;
         Update_Days : constant Natural := Athena.Options.Update_Count;
         Start       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         if Update_Days > 0 then
            Process.Start_Bar ("Updating", Update_Days * 24, True);

            for Day_Index in 1 .. Update_Days loop
               for Hour_Index in 1 .. 24 loop
                  for Minute_Index in 1 .. 60 loop
                     Athena.Calendar.Advance (60.0);
                     Athena.Updates.Control.Execute_Pending_Updates;
                  end loop;
                  Process.Tick;
               end loop;
               Process.Tick;
            end loop;
            Process.Finish;

            declare
               use Ada.Calendar;
            begin
               Ada.Text_IO.Put_Line
                 ("advanced" & Update_Days'Image & " day"
                  & (if Update_Days = 1 then "" else "s")
                  & " in "
                  & Athena.Real_Images.Approximate_Image
                    (Real (Clock - Start))
                  & "s");
            end;

         end if;

      end;

      Athena.Handles.State.Save_State;
      Athena.Logging.Stop_Logging;

      return;
   end if;

   --     if Athena.Options.View_Encounter then
   --
   --        if Athena.Options.Star_Name = "" then
   --           Ada.Text_IO.Put_Line
   --             (Ada.Text_IO.Standard_Error,
   --              "missing option: --star-name");
   --           return;
   --        end if;
   --
   --        if Athena.Options.Turn = 0 then
   --           Ada.Text_IO.Put_Line
   --             (Ada.Text_IO.Standard_Error,
   --              "missing option: --turn");
   --           return;
   --        end if;
   --
   --        Athena.Handles.State.Load_State;
   --        Database_Open := True;
   --
   --  --        declare
   --  --           Star : constant Athena.Handles.Star.Star_Handle :=
   --  --                    Athena.Stars.Find_Star (Athena.Options.Star_Name);
   --  --           Turn : constant Athena.Handles.Turn.Turn_Class :=
   --  --                    Athena.Turns.Get_Turn (Athena.Options.Turn);
   --  --
   --  --        begin
   --  --           if not Star.Has_Element then
   --  --              Ada.Text_IO.Put_Line
   --  --                (Ada.Text_IO.Standard_Error,
   --  --                 "cannot find star: " & Athena.Options.Star_Name);
   --  --           elsif not Turn.Has_Element then
   --  --              Ada.Text_IO.Put_Line
   --  --                (Ada.Text_IO.Standard_Error,
   --  --                 "no such turn: " & Athena.Options.Turn'Image);
   --  --           else
   --
   --        declare
   --        Encounter : constant Athena.Handles.Encounter.Encounter_Handle :=
   --                         Athena.Encounters.Find
   --                           (Athena.Options.Star_Name,
   --                            Athena.Options.Turn);
   --        begin
   --           if Encounter.Has_Element then
   --              declare
   --                 UI : Athena.UI.Athena_User_Interface'Class :=
   --                        Athena.UI.Launch.Get_Encounter_UI
   --                          (Encounter);
   --              begin
   --                 UI.Start;
   --              end;
   --           end if;
   --        end;
   --
   --        Athena.Handles.State.Save_State;
   --        Database_Open := False;
   --
   --        return;
   --     end if;

   Athena.Logging.Start_Logging ("gui");
   Athena.Handles.State.Load_State;

   declare
      UI : Athena.UI.Athena_User_Interface'Class :=
        Athena.UI.Launch.Get_UI
          (Athena.Handles.Empire.Get_By_Name
             (Athena.Options.Empire_Name));
   begin
      UI.Start;
   end;

   Athena.Handles.State.Save_State;

   Athena.Logging.Stop_Logging;

exception

   when others =>
      --  if Database_Open then
      --     Athena.Handles.State.Save_State;
      --  end if;
      --
      Athena.Logs.Flush_Logs (True);
      Athena.Logging.Stop_Logging;
      raise;

end Athena.Driver;
