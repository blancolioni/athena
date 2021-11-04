with Ada.Containers.Doubly_Linked_Lists;

with Nazar.Builder.Gtk_Creator;
with Nazar.Controllers.Draw;

with Nazar.Models.Draw;
with Nazar.Models.Text;

with Nazar.Views.Draw;

with Nazar.Gtk_Main;
with Nazar.Main;
with Nazar.Signals;

with Athena.UI.Models.Encounters;
with Athena.UI.Models.Galaxy;

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
         Top              : Nazar.Views.Nazar_View;
         Models           : Model_Lists.List;
         Galaxy_View      : Nazar.Views.Draw.Nazar_Draw_View;
         Home_System_View : Nazar.Views.Draw.Nazar_Draw_View;
      end record;

   overriding procedure Start
     (UI : in out Athena_Nazar_UI);

   procedure Next_Encounter_Tick
     (User_Data : Nazar.Signals.User_Data_Interface'Class)
     with Unreferenced;

   procedure Image
     (Category : String;
      Name     : String)
     with Unreferenced;

   ----------------------
   -- Get_Encounter_UI --
   ----------------------

   function Get_Encounter_UI
     (Encounter : Minerva.Encounter.Encounter_Class)
      return Athena_User_Interface'Class
   is
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

         Result.Encounter_Model :=
           Athena.UI.Models.Encounters.Encounter_Model (Encounter);

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
     (Empire : Minerva.Empire.Empire_Class)
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

         declare
            Galaxy_Layers : constant Athena.UI.Models.Draw_Model_Layers :=
                              Athena.UI.Models.Galaxy.Galaxy_Model (Empire);
         begin
            Result.Galaxy_View :=
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("galaxy"));

            for Model of Galaxy_Layers loop
               Result.Galaxy_View.Append (Model);
               Result.Models.Append (Nazar.Models.Nazar_Model (Model));
            end loop;
            Result.Galaxy_View.Show;
         end;

         Builder.Get_View ("empire-label").Set_Property ("text", Empire.Name);

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Date_Model;
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

      end return;
   end Get_UI;

   -----------
   -- Image --
   -----------

   procedure Image
     (Category : String;
      Name     : String)
   is
   begin
      Nazar.Gtk_Main.Add_Image
        (Resource_Name => Name,
         File_Path     =>
           Athena.Paths.Config_File
             ("ui/images/" & Category & "/" & Name & ".png"));
   end Image;

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

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Athena_Nazar_UI)
   is
   begin
      UI.Top.Show;
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

end Athena.UI.Nazar_UI;
