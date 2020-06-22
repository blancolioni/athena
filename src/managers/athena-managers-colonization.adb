with Ada.Containers.Doubly_Linked_Lists;

with Athena.Cargo.Colonists;
with Athena.Cargo.Commodities;
with Athena.Colonies;
with Athena.Empires;
with Athena.Stars;

with Athena.Managers.Transportation;

with Athena.Handles.Colony;
with Athena.Handles.Commodity;
with Athena.Handles.Empire;
with Athena.Handles.Knowledge;
with Athena.Handles.Star;
with Athena.Handles.World;

package body Athena.Managers.Colonization is

   type Colonization_Manager is
     new Root_Manager_Type with null record;

   overriding function Identifier
     (Manager : Colonization_Manager)
      return String
   is ("colonization");

   overriding procedure Dispatch_Create_Orders
     (Manager : in out Colonization_Manager);

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
           Has_Next_Update => True,
           Messages => <>);
   end Default_Colonization_Manager;

   ----------------------------
   -- Dispatch_Create_Orders --
   ----------------------------

   overriding procedure Dispatch_Create_Orders
     (Manager : in out Colonization_Manager)
   is

      Empire      : constant Athena.Handles.Empire.Empire_Handle :=
                      Athena.Handles.Empire.Get (Manager.Empire);
      Knowledge   : constant Athena.Handles.Knowledge.Knowledge_Handle :=
                      Empire.Knowledge;

      type Target_Record is
         record
            World : Athena.Handles.World.World_Handle;
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
         Best_World : Athena.Handles.World.World_Handle :=
                        Athena.Handles.World.Empty_Handle;
         Best_Score : Non_Negative_Real := 0.0;

         procedure Check_World
           (Reference : Athena.Handles.World_Reference);

         -----------------
         -- Check_World --
         -----------------

         procedure Check_World
           (Reference : Athena.Handles.World_Reference)
         is
            World : constant Athena.Handles.World.World_Handle :=
                      Athena.Handles.World.Get (Reference);
            Score : constant Non_Negative_Real :=
                      1000.0 * World.Resource * World.Habitability ** 2;
         begin
            if Score > Best_Score then
               Best_Score := Score;
               Best_World := World;
            end if;
         end Check_World;

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

         Star.Iterate_Worlds (Check_World'Access);

         if Best_World.Has_Element then
            declare
               Distance : constant Non_Negative_Real :=
                            Athena.Stars.Distance
                              (Star, Athena.Empires.Capital (Empire));
               Rec      : constant Target_Record :=
                            Target_Record'
                              (World => Best_World,
                               Score => Best_Score / Distance ** 2);
            begin
               Manager.Log
                 ("colonization target " & Best_World.Name
                  & ": resource " & Image (Best_World.Resource * 100.0) & "%"
                  & "; habitability "
                  & Image (Best_World.Habitability * 100.0) & "%"
                  & "; space" & Best_World.Space'Image
                  & "; distance " & Image (Distance)
                  & "; colonization score " & Image (Rec.Score));
               Targets.Append (Rec);
            end;
         end if;

      end Check_Colonization_Target;

   begin
      Knowledge.Iterate_Uncolonized
        (Check_Colonization_Target'Access);

      if not Targets.Is_Empty then
         Target_Sorting.Sort (Targets);
         Manager.Log
           ("colonizing: " & Targets.First_Element.World.Name
            & " score " & Image (Targets.First_Element.Score));

         declare
            function Pop (Of_Colony : Athena.Handles.Colony.Colony_Handle)
                          return Real
            is (Of_Colony.Population);

            From : constant Athena.Handles.Colony.Colony_Handle :=
                     Athena.Colonies.Best_Colony
                       (Empire, Pop'Access);
            Cargo : Athena.Cargo.Cargo_Container;
         begin
            Manager.Log ("colonizing from " & From.World.Name);

            Cargo.Add_Cargo
              (Item     => Athena.Cargo.Colonists.Colonist_Cargo (Empire),
               Quantity => 100.0);

            Cargo.Add_Cargo
              (Item     =>
                 Athena.Cargo.Commodities.Commodity_Cargo
                   (Athena.Handles.Commodity.Food),
               Quantity => 100.0);

            Empire.Send_Message
              (Athena.Handles.Transport_Manager,
               Athena.Managers.Transportation.Transport_Message
                 (Empire   => Empire.Reference,
                  From     => From.World.Reference,
                  To       => Targets.First_Element.World.Reference,
                  Cargo    => Cargo,
                  Priority => Manager.Priority));

         end;

         --  Knowledge.Set_Colonizing
         --    (Targets.First_Element.Star, True);
      end if;

      Manager.Set_Next_Update_Delay (Athena.Calendar.Days (5.0));
   end Dispatch_Create_Orders;

end Athena.Managers.Colonization;
