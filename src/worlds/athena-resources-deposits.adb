with Athena.Random;

package body Athena.Resources.Deposits is

   --------------------
   -- Score_Resource --
   --------------------

   function Score_Resource
     (Generator : Resource_Generator;
      World     : Athena.Handles.World.World_Handle)
      return Natural
   is

      Score : Non_Negative_Real := 0.0;

      function Check (Frequency : Resource_Frequency) return Boolean;

      -----------
      -- Check --
      -----------

      function Check
        (Frequency : Resource_Frequency)
         return Boolean
      is
         use type Athena.Handles.Stellar_Orbit_Zone;
         use type Athena.Handles.Life_Complexity_Type;
         use type Athena.Handles.World_Composition;
      begin
         for Constraint of Frequency.Constraints loop
            case Constraint.Class is
               when Zone_Constraint =>
                  if Constraint.Zone /= World.Orbit_Zone then
                     return False;
                  end if;

               when Life_Constraint =>
                  if World.Life < Constraint.Minimum_Complexity then
                     return False;
                  end if;

               when Age_Constraint =>
                  if World.Age < Constraint.Minimum_Age then
                     return False;
                  end if;

               when Hydrosphere_Constraint =>
                  if World.Hydrosphere < Constraint.Minimum_Hydrosphere
                    or else World.Hydrosphere > Constraint.Maximum_Hydrosphere
                  then
                     return False;
                  end if;

               when Composition_Constraint =>
                  if World.Composition /= Constraint.Composition then
                     return False;
                  end if;
            end case;
         end loop;
         return True;
      end Check;

   begin
      for Frequency of Generator.List loop
         if Check (Frequency) then
            if Frequency.Unlimited then
               return 0;
            else
               Score := Score +
                 Real'Max (0.0,
                           Athena.Random.Normal_Random (Frequency.Std_Dev)
                           + Frequency.Mean);
            end if;
         end if;
      end loop;
      return Natural (Score * 1000.0);
   end Score_Resource;

end Athena.Resources.Deposits;
