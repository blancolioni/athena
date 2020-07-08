package body Athena.Resources is

   ----------------------------
   -- Add_Resource_Frequency --
   ----------------------------

   procedure Add_Resource_Frequency
     (Generator : in out Resource_Generator;
      Frequency : Resource_Frequency)
   is
   begin
      Generator.List.Append (Frequency);
   end Add_Resource_Frequency;

   --------------------
   -- Age_Constraint --
   --------------------

   procedure Age_Constraint
     (Frequency     : in out Resource_Frequency;
      Minimum_Years : Non_Negative_Real)
   is
   begin
      Frequency.Constraints.Append
        (Resource_Constraint'
           (Class               => Age_Constraint,
            Minimum_Age         => Minimum_Years));
   end Age_Constraint;

   ----------------------------
   -- Composition_Constraint --
   ----------------------------

   procedure Composition_Constraint
     (Frequency : in out Resource_Frequency;
      Required  :        Athena.Handles.World_Composition)
   is
   begin
      Frequency.Constraints.Append
        (Resource_Constraint'
           (Class               => Composition_Constraint,
            Composition         => Required));
   end Composition_Constraint;

   -------------------------------
   -- Create_Resource_Frequency --
   -------------------------------

   function Create_Resource_Frequency
     (Mean               : Unit_Real;
      Standard_Deviation : Unit_Real)
      return Resource_Frequency
   is
   begin
      return Resource_Frequency'
        (Constraints => <>,
         Mean        => Mean,
         Std_Dev     => Standard_Deviation,
         Unlimited   => Mean = 0.0 and then Standard_Deviation = 0.0);
   end Create_Resource_Frequency;

   ----------------------------
   -- Hydrosphere_Constraint --
   ----------------------------

   procedure Hydrosphere_Constraint
     (Frequency : in out Resource_Frequency;
      Minimum   : Unit_Real := 0.0;
      Maximum   :        Unit_Real := 1.0)
   is
   begin
      Frequency.Constraints.Append
        (Resource_Constraint'
           (Class               => Hydrosphere_Constraint,
            Minimum_Hydrosphere => Minimum,
            Maximum_Hydrosphere => Maximum));
   end Hydrosphere_Constraint;

   ---------------------
   -- Life_Constraint --
   ---------------------

   procedure Life_Constraint
     (Frequency : in out Resource_Frequency;
      Minimum   :        Athena.Handles.Life_Complexity_Type)
   is
   begin
      Frequency.Constraints.Append
        (Resource_Constraint'
           (Class               => Life_Constraint,
            Minimum_Complexity  => Minimum));
   end Life_Constraint;

   ---------------------
   -- Zone_Constraint --
   ---------------------

   procedure Zone_Constraint
     (Frequency : in out Resource_Frequency;
      Zone      :        Athena.Handles.Stellar_Orbit_Zone)
   is
   begin
      Frequency.Constraints.Append
        (Resource_Constraint'
           (Class => Zone_Constraint,
            Zone  => Zone));
   end Zone_Constraint;

end Athena.Resources;
