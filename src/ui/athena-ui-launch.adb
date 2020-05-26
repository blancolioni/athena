with Athena.UI.Nazar_UI;

package body Athena.UI.Launch is

   ----------------------
   -- Get_Encounter_UI --
   ----------------------

   function Get_Encounter_UI
     (Encounter : Athena.Handles.Encounter.Encounter_Handle)
      return Athena_User_Interface'Class
   is
   begin
      return Athena.UI.Nazar_UI.Get_Encounter_UI (Encounter);
   end Get_Encounter_UI;

   ------------
   -- Get_UI --
   ------------

   function Get_UI
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena_User_Interface'Class
   is
   begin
      return Athena.UI.Nazar_UI.Get_UI (Empire);
   end Get_UI;

end Athena.UI.Launch;
