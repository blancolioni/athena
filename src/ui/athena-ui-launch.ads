with Athena.Handles.Empire;
with Athena.Handles.Encounter;

package Athena.UI.Launch is

   function Get_UI
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena_User_Interface'Class;

   function Get_Encounter_UI
     (Encounter : Athena.Handles.Encounter.Encounter_Handle)
      return Athena_User_Interface'Class;

end Athena.UI.Launch;
