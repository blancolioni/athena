with Nazar.Models.Draw;

with Athena.Handles.Empire;

package Athena.UI.Models.Galaxy is

   function Galaxy_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Nazar.Models.Draw.Nazar_Draw_Model;

end Athena.UI.Models.Galaxy;
