with Athena.Money;

package body Athena.UI.Models is

   type Current_Turn_Model_Record is
     new Dynamic_Text_Model with null record;

   overriding function Current_Text
     (Model : Current_Turn_Model_Record) return String
   is ("Turn " & Athena.Handles.Current_Turn_Image);

   type Current_Cash_Model_Record is
     new Dynamic_Text_Model with
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
      end record;

   overriding function Current_Text
     (Model : Current_Cash_Model_Record) return String
   is ("Cash "
       & Athena.Money.Show (Model.Empire.Cash));

   type Current_Debt_Model_Record is
     new Dynamic_Text_Model with
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
      end record;

   overriding function Current_Text
     (Model : Current_Debt_Model_Record) return String
   is ("Debt "
       & Athena.Money.Show (Model.Empire.Debt));

   ------------------------
   -- Current_Cash_Model --
   ------------------------

   function Current_Cash_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Nazar.Models.Text.Nazar_Text_Model
   is
   begin
      return Model : constant Nazar.Models.Text.Nazar_Text_Model :=
        new Current_Cash_Model_Record'
          (Nazar.Models.Text.Nazar_Text_Model_Record with
             Empire => Empire)
      do
         Model.Set_Text ("");
         Model.Reload;
      end return;
   end Current_Cash_Model;

   ------------------------
   -- Current_Debt_Model --
   ------------------------

   function Current_Debt_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Nazar.Models.Text.Nazar_Text_Model
   is
   begin
      return Model : constant Nazar.Models.Text.Nazar_Text_Model :=
        new Current_Debt_Model_Record'
          (Nazar.Models.Text.Nazar_Text_Model_Record with
             Empire => Empire)
      do
         Model.Set_Text ("");
         Model.Reload;
      end return;
   end Current_Debt_Model;

   ------------------------
   -- Current_Turn_Model --
   ------------------------

   function Current_Turn_Model return Nazar.Models.Text.Nazar_Text_Model is
   begin
      return Model : constant Nazar.Models.Text.Nazar_Text_Model :=
        new Current_Turn_Model_Record
      do
         Model.Set_Text ("");
         Model.Reload;
      end return;
   end Current_Turn_Model;

   ------------
   -- Reload --
   ------------

   overriding procedure Reload (Model : in out Dynamic_Text_Model) is
   begin
      Model.Set_Text (Dynamic_Text_Model'Class (Model).Current_Text);
   end Reload;

end Athena.UI.Models;
