private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Athena.Handles;

package Athena.Resources is

   type Resource_Generator is private;

   type Resource_Frequency is private;

   procedure Add_Resource_Frequency
     (Generator : in out Resource_Generator;
      Frequency : Resource_Frequency);

   function Create_Resource_Frequency
     (Mean               : Unit_Real;
      Standard_Deviation : Unit_Real)
      return Resource_Frequency;

   procedure Age_Constraint
     (Frequency     : in out Resource_Frequency;
      Minimum_Years : Non_Negative_Real);

   procedure Composition_Constraint
     (Frequency     : in out Resource_Frequency;
      Required      : Athena.Handles.World_Composition);

   procedure Hydrosphere_Constraint
     (Frequency     : in out Resource_Frequency;
      Minimum       : Unit_Real := 0.0;
      Maximum   : Unit_Real := 1.0);

   procedure Life_Constraint
     (Frequency     : in out Resource_Frequency;
      Minimum       : Athena.Handles.Life_Complexity_Type);

   procedure Zone_Constraint
     (Frequency     : in out Resource_Frequency;
      Zone          : Athena.Handles.Stellar_Orbit_Zone);

private

   type Resource_Constraint_Class is
     (Age_Constraint, Composition_Constraint, Hydrosphere_Constraint,
      Life_Constraint, Zone_Constraint);

   type Resource_Constraint (Class : Resource_Constraint_Class) is
      record
         case Class is
            when Age_Constraint =>
               Minimum_Age : Non_Negative_Real;
            when Composition_Constraint =>
               Composition : Athena.Handles.World_Composition;
            when Hydrosphere_Constraint =>
               Minimum_Hydrosphere : Unit_Real;
               Maximum_Hydrosphere : Unit_Real;
            when Life_Constraint =>
               Minimum_Complexity  : Athena.Handles.Life_Complexity_Type;
            when Zone_Constraint =>
               Zone                : Athena.Handles.Stellar_Orbit_Zone;
         end case;
      end record;

   package Resource_Constraint_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Resource_Constraint);

   type Resource_Frequency is
      record
         Constraints : Resource_Constraint_Lists.List;
         Mean        : Non_Negative_Real;
         Std_Dev     : Non_Negative_Real;
         Unlimited   : Boolean;
      end record;

   package Resource_Frequency_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Resource_Frequency);

   type Resource_Generator is
      record
         List : Resource_Frequency_Lists.List;
      end record;

end Athena.Resources;
