with Ada.Containers.Doubly_Linked_Lists;

with Athena.Logging;

with Athena.Colonies;
with Athena.Empires;
with Athena.Stars;

with Athena.Knowledge.Stars;
with Athena.Managers.Transportation;

with Minerva.Colony;
with Minerva.Empire;
with Minerva.Star;

with Minerva.Db;

package body Athena.Managers.Colonization is

   type Colonization_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Colonization_Manager)
      return String
   is ("colonization");

   overriding procedure Create_Orders
     (Manager : Colonization_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Colonization_Manager)
   is

      Empire      : constant Minerva.Empire.Empire_Handle := Manager.Empire;
      Knowledge   : Athena.Knowledge.Stars.Star_Knowledge;

      type Target_Record is
         record
            Star  : Minerva.Star.Star_Handle;
            Score : Non_Negative_Real;
         end record;

      function Better (Left, Right : Target_Record) return Boolean
      is (Left.Score > Right.Score);

      package Target_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Target_Record);

      package Target_Sorting is
        new Target_Lists.Generic_Sorting (Better);

      Targets : Target_Lists.List;

      procedure Check_Colonization_Target
        (Star : Minerva.Star.Star_Class;
         Stop : out Boolean);

      -------------------------------
      -- Check_Colonization_Target --
      -------------------------------

      procedure Check_Colonization_Target
        (Star : Minerva.Star.Star_Class;
         Stop : out Boolean)
      is
      begin

         Stop := False;

         if Star.Owner.Has_Element then
            return;
         end if;

         if not Knowledge.Visited (Star)
           or else Knowledge.Colonizing (Star)
         then
            return;
         end if;

         declare
            Distance : constant Non_Negative_Real :=
                         Athena.Stars.Distance
                           (Star, Athena.Empires.Capital (Empire));
            Rec : constant Target_Record :=
                    Target_Record'
                      (Star  => Star.To_Star_Handle,
                       Score =>
                         (Star.Resource + Star.Habitability)
                       / Distance);
         begin
            Manager.Log
              ("colonization target " & Star.Name
               & ": resource " & Image (Star.Resource * 100.0) & "%"
               & "; habitability " & Image (Star.Habitability * 100.0) & "%"
               & "; space" & Star.Space'Image
               & "; distance " & Image (Distance)
               & "; colonization score " & Image (Rec.Score));
            Targets.Append (Rec);
         end;

      end Check_Colonization_Target;

   begin
      Knowledge.Load (Manager.Empire);

      Knowledge.Iterate_Uncolonized
        (Check_Colonization_Target'Access);

      if not Targets.Is_Empty then
         Target_Sorting.Sort (Targets);
         Athena.Logging.Log
           ("colonizing: " & Targets.First_Element.Star.Name
            & " score " & Image (Targets.First_Element.Score));

         declare
            function Pop (Of_Colony : Minerva.Colony.Colony_Class)
                          return Real
            is (Of_Colony.Population);

            From : constant Minerva.Colony.Colony_Class :=
                     Athena.Colonies.Best_Colony
                       (Empire, Pop'Access);
         begin
            Manager.Log ("colonizing from " & From.Star.Name);

            Send_Message
              (Destination => "transport",
               Empire      => Empire,
               Message     =>
                 Athena.Managers.Transportation.Transport_Message
                   (Empire   => Empire,
                    From     => From.Star,
                    To       => Targets.First_Element.Star,
                    Cargo    => Minerva.Db.Colonists,
                    Quantity => 10.0,
                    Priority => Manager.Priority));
            Send_Message
              (Destination => "transport",
               Empire      => Empire,
               Message     =>
                 Athena.Managers.Transportation.Transport_Message
                   (Empire   => Empire,
                    From     => From.Star,
                    To       => Targets.First_Element.Star,
                    Cargo    => Minerva.Db.Material,
                    Quantity => 10.0,
                    Priority => Manager.Priority));
         end;

         Knowledge.Set_Colonizing
           (Targets.First_Element.Star, True);
      end if;

   end Create_Orders;

   ----------------------------------
   -- Default_Colonization_Manager --
   ----------------------------------

   function Default_Colonization_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Manager : constant Colonization_Manager :=
        Colonization_Manager'
          (Name     => +"explore",
           Empire   => <>,
           Priority => 1030,
           Manager => <>);
   end Default_Colonization_Manager;

end Athena.Managers.Colonization;
