with Ada.Containers.Doubly_Linked_Lists;

with Athena.Logging;

with Athena.Colonies;
with Athena.Empires;
with Athena.Stars;

with Athena.Managers.Transportation;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Knowledge;
with Athena.Handles.Star;

package body Athena.Managers.Colonization is

   type Colonization_Manager is
     new Root_Manager_Type with null record;

   overriding function Identifier
     (Manager : Colonization_Manager)
      return String
   is ("colonization");

   overriding procedure Create_Orders
     (Manager : in out Colonization_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : in out Colonization_Manager)
   is

      Empire      : constant Athena.Handles.Empire.Empire_Handle :=
                      Athena.Handles.Empire.Get (Manager.Empire);
      Knowledge   : constant Athena.Handles.Knowledge.Knowledge_Handle :=
                      Empire.Knowledge;

      type Target_Record is
         record
            Star  : Athena.Handles.Star.Star_Handle;
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
        (Star : Athena.Handles.Star.Star_Handle;
         Stop : out Boolean);

      -------------------------------
      -- Check_Colonization_Target --
      -------------------------------

      procedure Check_Colonization_Target
        (Star : Athena.Handles.Star.Star_Handle;
         Stop : out Boolean)
      is
      begin

         Stop := False;

         if Star.Has_Owner then
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
                      (Star  => Star,
                       Score =>
                         Real'Max (Star.Resource, Star.Habitability)
                       / Distance / Distance);
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
      Knowledge.Iterate_Uncolonized
        (Check_Colonization_Target'Access);

      if not Targets.Is_Empty then
         Target_Sorting.Sort (Targets);
         Athena.Logging.Log
           ("colonizing: " & Targets.First_Element.Star.Name
            & " score " & Image (Targets.First_Element.Score));

         declare
            function Pop (Of_Colony : Athena.Handles.Colony.Colony_Handle)
                          return Real
            is (Of_Colony.Population);

            From : constant Athena.Handles.Colony.Colony_Handle :=
                     Athena.Colonies.Best_Colony
                       (Empire, Pop'Access);
         begin
            Manager.Log ("colonizing from " & From.Star.Name);

            Empire.Send_Message
              (Athena.Handles.Transport_Manager,
               Athena.Managers.Transportation.Transport_Message
                 (Empire   => Empire.Reference,
                  From     => From.Star.Reference,
                  To       => Targets.First_Element.Star.Reference,
                  Cargo    => Athena.Handles.Colonists,
                  Quantity => 10.0,
                  Priority => Manager.Priority));

         end;

         Knowledge.Set_Colonizing
           (Targets.First_Element.Star, True);
      end if;

      Manager.Set_Next_Update_Delay (Athena.Calendar.Days (5.0));
   end Create_Orders;

   ----------------------------------
   -- Default_Colonization_Manager --
   ----------------------------------

   function Default_Colonization_Manager
     return Root_Manager_Type'Class
   is
   begin
      return Manager : constant Colonization_Manager :=
        Colonization_Manager'
          (Name     => +"explore",
           Empire   => <>,
           Priority => 1030,
           Next_Update => Athena.Calendar.Clock,
           Messages => <>);
   end Default_Colonization_Manager;

end Athena.Managers.Colonization;
