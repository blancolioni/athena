with Nazar.Models.Draw;
with Nazar.Models.Text;

with Athena.Handles.Empire;

package Athena.UI.Models is

   function Current_Date_Model return Nazar.Models.Text.Nazar_Text_Model;

   function Current_Cash_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
     return Nazar.Models.Text.Nazar_Text_Model;

   function Current_Debt_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Nazar.Models.Text.Nazar_Text_Model;

   type Draw_Model_Layers is
     array (Positive range <>) of Nazar.Models.Draw.Nazar_Draw_Model;

private

   type Dynamic_Text_Model is
     abstract new Nazar.Models.Text.Nazar_Text_Model_Record with null record;

   function Current_Text
     (Model : Dynamic_Text_Model) return String
      is abstract;

   overriding procedure Reload (Model : in out Dynamic_Text_Model);

end Athena.UI.Models;
