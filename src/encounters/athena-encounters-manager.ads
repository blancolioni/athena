with Ada.Containers.Doubly_Linked_Lists;

with Athena.Encounters.Frames;

with Minerva.Encounter;
with Minerva.Ship;
with Minerva.Star;
with Minerva.War;

package Athena.Encounters.Manager is

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Ship.Ship_Handle, Minerva.Ship."=");

   procedure Resolve_Encounter
     (Star  : Minerva.Star.Star_Class;
      War   : Minerva.War.War_Class;
      Ships : Ship_Lists.List;
      Size  : Positive);

   procedure Load_Encounter
     (Encounter : Minerva.Encounter.Encounter_Class;
      Sequence  : out Athena.Encounters.Frames.Frame_Sequence_Type);

end Athena.Encounters.Manager;
