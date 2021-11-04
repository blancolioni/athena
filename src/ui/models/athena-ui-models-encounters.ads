with Nazar.Models.Draw;

with Minerva.Encounter;

package Athena.UI.Models.Encounters is

   function Encounter_Model
     (Encounter : Minerva.Encounter.Encounter_Class)
      return Nazar.Models.Draw.Nazar_Draw_Model;

end Athena.UI.Models.Encounters;
