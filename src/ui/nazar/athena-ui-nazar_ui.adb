with Ada.Containers.Doubly_Linked_Lists;

with Nazar.Builder.Gtk_Creator;
with Nazar.Controllers.Draw;

with Nazar.Models.Draw;
with Nazar.Models.Text;

with Nazar.Views.Button;
with Nazar.Views.Draw;

with Nazar.Main;
with Nazar.Signals;

--  with Athena.UI.Models.Encounters;

with Athena.UI.Models.Galaxy;

with Athena.Updates;

with Athena.Options;
with Athena.Paths;

package body Athena.UI.Nazar_UI is

   package Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Nazar.Models.Nazar_Model, Nazar.Models."=");

   Current_Tick : Natural := 0;

   type Athena_Encounter_UI is
     new Athena_User_Interface
     and Nazar.Signals.User_Data_Interface with
      record
         Top               : Nazar.Views.Nazar_View;
         Models            : Model_Lists.List;
         Encounter_Model   : Nazar.Models.Draw.Nazar_Draw_Model;
         Encounter_View    : Nazar.Views.Draw.Nazar_Draw_View;
         Turn_Model        : Nazar.Models.Text.Nazar_Text_Model;
         Encounter_Control : Nazar.Controllers.Draw.
           Nazar_Draw_Controller_Record;
      end record;

   overriding procedure Start
     (UI : in out Athena_Encounter_UI);

   type Athena_Nazar_UI is
     new Athena_User_Interface
     and Nazar.Signals.User_Data_Interface with
      record
         Top            : Nazar.Views.Nazar_View;
         Models         : Model_Lists.List;
         Galaxy_Model   : Nazar.Models.Draw.Nazar_Draw_Model;
         Galaxy_View    : Nazar.Views.Draw.Nazar_Draw_View;
         Galaxy_Control : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
      end record;

   overriding procedure Start
     (UI : in out Athena_Nazar_UI);

   procedure On_Update_Clicked
     (User_Data : Nazar.Signals.User_Data_Interface'Class);

   procedure Next_Encounter_Tick
     (User_Data : Nazar.Signals.User_Data_Interface'Class)
     with Unreferenced;

   task Update_Task is
      entry Start (UI : Athena_Nazar_UI;
                   Interval : Duration);
      entry Run_Update;
      entry Stop;
   end Update_Task;

   ----------------------
   -- Get_Encounter_UI --
   ----------------------

   function Get_Encounter_UI
     (Encounter : Athena.Handles.Encounter.Encounter_Handle)
      return Athena_User_Interface'Class
   is
      pragma Unreferenced (Encounter);
      Builder : constant Nazar.Builder.Nazar_Builder :=
                  Nazar.Builder.Nazar_Builder_New
                    (Creator     => Nazar.Builder.Gtk_Creator.Get_Gtk_Creator,
                     Config_Path =>
                       Athena.Paths.Config_File ("ui/encounter.nazar"));
   begin
      Nazar.Main.Init;
      Current_Tick := 0;
      return Result : Athena_Encounter_UI do
         Result.Top := Builder.Get_View ("Athena");

         Result.Turn_Model :=
           Nazar.Models.Text.Nazar_Text_Model_New ("turn");
         Builder.Get_View ("turn-label").Set_Model (Result.Turn_Model);
         Result.Models.Append (Nazar.Models.Nazar_Model (Result.Turn_Model));

         --  Result.Encounter_Model :=
         --    Athena.UI.Models.Encounters.Encounter_Model (Encounter);
         Result.Models.Append
           (Nazar.Models.Nazar_Model (Result.Encounter_Model));
         Result.Encounter_View :=
           Nazar.Views.Draw.Nazar_Draw_View
             (Builder.Get_View ("encounter"));
         Result.Encounter_Control.Start_Draw
           (Model => Result.Encounter_Model,
            View  => Result.Encounter_View);
      end return;
   end Get_Encounter_UI;

   ------------
   -- Get_UI --
   ------------

   function Get_UI
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena_User_Interface'Class
   is
      Builder : constant Nazar.Builder.Nazar_Builder :=
        Nazar.Builder.Nazar_Builder_New
          (Creator     => Nazar.Builder.Gtk_Creator.Get_Gtk_Creator,
           Config_Path => Athena.Paths.Config_File ("ui/athena.nazar"));
   begin
      Nazar.Main.Init;
      return Result : Athena_Nazar_UI do
         Result.Top := Builder.Get_View ("Athena");
         Result.Galaxy_Model := Athena.UI.Models.Galaxy.Galaxy_Model (Empire);
         Result.Galaxy_View :=
           Nazar.Views.Draw.Nazar_Draw_View
             (Builder.Get_View ("galaxy"));

         Result.Models.Append (Nazar.Models.Nazar_Model (Result.Galaxy_Model));

         Result.Galaxy_Control.Start_Draw
           (Model => Result.Galaxy_Model,
            View  => Result.Galaxy_View);

         Builder.Get_View ("empire-label").Set_Property ("text", Empire.Name);

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Turn_Model;
         begin
            Builder.Get_View ("turn-label").Set_Model (Model);
            Result.Models.Append (Nazar.Models.Nazar_Model (Model));
         end;

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Cash_Model (Empire);
         begin
            Builder.Get_View ("cash-label").Set_Model (Model);
            Result.Models.Append (Nazar.Models.Nazar_Model (Model));
         end;

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Debt_Model (Empire);
         begin
            Builder.Get_View ("debt-label").Set_Model (Model);
            Result.Models.Append (Nazar.Models.Nazar_Model (Model));
         end;

         Nazar.Views.Button.Nazar_Button_View
           (Builder.Get_View ("update"))
             .On_Activate (On_Update_Clicked'Access, Result);

      end return;
   end Get_UI;

   -------------------------
   -- Next_Encounter_Tick --
   -------------------------

   procedure Next_Encounter_Tick
     (User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      UI : Athena_Encounter_UI'Class renames
             Athena_Encounter_UI'Class (User_Data);
   begin
      Current_Tick := Current_Tick + 1;
      UI.Turn_Model.Set_Text ("Turn" & Current_Tick'Image);
      for Model of UI.Models loop
         Model.Reload;
      end loop;
      UI.Encounter_View.Set_Viewport
        (UI.Encounter_Model.Bounding_Box);
   end Next_Encounter_Tick;

   -----------------------
   -- On_Update_Clicked --
   -----------------------

   procedure On_Update_Clicked
     (User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      UI : Athena_Nazar_UI'Class renames
             Athena_Nazar_UI'Class (User_Data);

      procedure Reload_Models;

      -------------------
      -- Reload_Models --
      -------------------

      procedure Reload_Models is
      begin
         for Model of UI.Models loop
            Model.Reload;
         end loop;
         UI.Galaxy_View.Set_Viewport
           (UI.Galaxy_Model.Bounding_Box);
      end Reload_Models;

   begin
      if Athena.Options.Auto_Update then
         Update_Task.Run_Update;
      else
         Athena.Updates.Run_Update;

         Nazar.Main.With_Render_Lock
           (Reload_Models'Access);
      end if;
   end On_Update_Clicked;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Athena_Nazar_UI)
   is
   begin
      if Athena.Options.Auto_Update then
         Update_Task.Start
           (UI, Duration (Athena.Options.Update_Interval) / 1000.0);
      end if;

      UI.Top.Show;

      if Athena.Options.Auto_Update then
         Update_Task.Stop;
      end if;

   end Start;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Athena_Encounter_UI)
   is
   begin
      UI.Top.Show;
      UI.Encounter_Model.Unload;
   end Start;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
      Update_UI       : Athena_Nazar_UI;
      Update_Interval : Duration;

      procedure Reload_Models;

      -------------------
      -- Reload_Models --
      -------------------

      procedure Reload_Models is
      begin
         for Model of Update_UI.Models loop
            Model.Reload;
         end loop;
         Update_UI.Galaxy_View.Set_Viewport
           (Update_UI.Galaxy_Model.Bounding_Box);
      end Reload_Models;

   begin
      select
         accept Start (UI : in Athena_Nazar_UI; Interval : in Duration) do
            Update_UI := UI;
            Update_Interval := Interval;
         end Start;
      or
         terminate;
      end select;

      loop
         select
            accept Stop;
            exit;
         or
            accept Run_Update;
            Athena.Updates.Run_Update;

            Nazar.Main.With_Render_Lock
              (Reload_Models'Access);

         or
            delay Update_Interval;
            Athena.Updates.Run_Update;

            Nazar.Main.With_Render_Lock
              (Reload_Models'Access);

         end select;
      end loop;
   end Update_Task;

end Athena.UI.Nazar_UI;
