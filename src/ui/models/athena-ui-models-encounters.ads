with Nazar.Models.Draw;

with Athena.Handles.Encounter;

package Athena.UI.Models.Encounters is

   function Encounter_Model
     (Encounter : Athena.Handles.Encounter.Encounter_Handle)
      return Nazar.Models.Draw.Nazar_Draw_Model;

end Athena.UI.Models.Encounters;
