with Athena.Handles.Empire;
with Athena.Handles.Star;

package Athena.UI.Models.Stars is

   function Star_System_Model
     (Empire : Athena.Handles.Empire.Empire_Handle;
      Star   : Athena.Handles.Star.Star_Handle)
      return Draw_Model_Layers;

end Athena.UI.Models.Stars;
