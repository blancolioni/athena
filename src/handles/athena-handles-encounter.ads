with Ada.Streams.Stream_IO;

with Athena.Handles.Empire;
with Athena.Handles.Ship;
with Athena.Handles.Star;

package Athena.Handles.Encounter is

   type Encounter_Handle is
     new Root_Athena_Handle with private;

   function Empty_Handle return Encounter_Handle;

   function Reference
     (Handle : Encounter_Handle)
      return Encounter_Reference;

   function Get
     (Reference : Encounter_Reference)
      return Encounter_Handle;

   function Star
     (Handle : Encounter_Handle)
      return Athena.Handles.Star.Star_Handle;

   procedure Add_Actor
     (Handle : Encounter_Handle;
      Actor  : Athena.Handles.Ship.Ship_Handle'Class);

   function Create
     (Star : Athena.Handles.Star.Star_Handle)
      return Encounter_Handle;

   function Find_Active_Encounter
     (Star      : Athena.Handles.Star.Star_Handle;
      Involving : Athena.Handles.Empire.Empire_Handle)
      return Encounter_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Encounter_Handle is
     new Root_Athena_Handle with
      record
         Reference : Encounter_Reference;
      end record;

   overriding function Short_Name
     (Encounter : Encounter_Handle)
      return String
   is ("encounter at " & Encounter.Star.Name);

   function Empty_Handle return Encounter_Handle
   is (False, 0);

   function Reference
     (Handle : Encounter_Handle)
      return Encounter_Reference
   is (Handle.Reference);

   function Get
     (Reference : Encounter_Reference)
      return Encounter_Handle
   is (Reference /= 0, Reference);

end Athena.Handles.Encounter;
