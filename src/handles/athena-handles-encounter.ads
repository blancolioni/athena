package Athena.Handles.Encounter is

   type Encounter_Handle is
     new Root_Athena_Handle with private;

   --  function Find_Encounter
   --    (Star_Name : String;
   --     Turn      : Athena_Turn_Number)
   --     return Encounter_Handle;

private

   type Encounter_Handle is
     new Root_Athena_Handle with
      record
         Reference : Encounter_Reference;
      end record;

   overriding function Short_Name
     (Encounter : Encounter_Handle)
      return String
   is ("encounter");

end Athena.Handles.Encounter;
