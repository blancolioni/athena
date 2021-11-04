with Minerva.Empire;
with Minerva.Encounter;

private package Athena.UI.Nazar_UI is

   function Get_UI
     (Empire : Minerva.Empire.Empire_Class)
      return Athena_User_Interface'Class;

   function Get_Encounter_UI
     (Encounter : Minerva.Encounter.Encounter_Class)
      return Athena_User_Interface'Class;

end Athena.UI.Nazar_UI;
