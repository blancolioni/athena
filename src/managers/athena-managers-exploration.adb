with Ada.Containers.Doubly_Linked_Lists;

with Athena.Ships.Lists;
with Athena.Stars;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Knowledge;
with Athena.Handles.Ship;
with Athena.Handles.Star;

with Athena.Handles.Ship.Actions;

package body Athena.Managers.Exploration is

   type Exploration_Manager is
     new Root_Manager_Type with null record;

   overriding function Identifier
     (Manager : Exploration_Manager)
      return String
   is ("exploration");

   overriding procedure Create_Orders
     (Manager : in out Exploration_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : in out Exploration_Manager)
   is
      Max_Range : constant Non_Negative_Real := 20.0;

      type Star_Score_Record is
         record
            Star  : Athena.Handles.Star.Star_Handle;
            Score : Non_Negative_Real;
         end record;

      package Star_Score_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Star_Score_Record);

      function Better (Left, Right : Star_Score_Record) return Boolean
      is (Left.Score > Right.Score);

      package Star_Score_Sorting is
        new Star_Score_Lists.Generic_Sorting (Better);

      Scout_Ships : Athena.Ships.Lists.List;
      Neighbours  : Star_Score_Lists.List;

      Empire : constant Athena.Handles.Empire.Empire_Handle :=
                 Athena.Handles.Empire.Get (Manager.Empire);
      Knowledge   : constant Athena.Handles.Knowledge.Knowledge_Handle :=
                      Empire.Knowledge;

      procedure Check_Scout_Target
        (Star    : Athena.Handles.Star.Star_Handle;
         Nearest : Athena.Handles.Colony.Colony_Handle;
         Stop    : out Boolean)
        with Unreferenced;

--        function Check_Upgrade
--          (Scout : Athena.Handles.Ship.Ship_Handle)
--           return Boolean;

      procedure Score_Star
        (Star    : Athena.Handles.Star.Star_Handle;
         Nearest : Athena.Handles.Colony_Reference;
         Stop    : out Boolean);

      procedure Assign_Ship
        (Available : in out Athena.Ships.Lists.List;
         To_Star   : Athena.Handles.Star.Star_Handle);

      -----------------
      -- Assign_Ship --
      -----------------

      procedure Assign_Ship
        (Available : in out Athena.Ships.Lists.List;
         To_Star   : Athena.Handles.Star.Star_Handle)
      is
         Assigned : Athena.Ships.Lists.Cursor :=
                      Athena.Ships.Lists.No_Element;
         Closest  : Non_Negative_Real := Non_Negative_Real'Last;
      begin
         for Position in Available.Iterate loop
            declare
               D : constant Non_Negative_Real :=
                     Athena.Stars.Distance
                       (Athena.Ships.Lists.Element (Position).Star_Location,
                        To_Star);
            begin
               if D < Closest then
                  Closest := D;
                  Assigned := Position;
               end if;
            end;
         end loop;

         pragma Assert (Athena.Ships.Lists.Has_Element (Assigned),
                        "expected a non-empty available list");

         declare
            Ship : constant Athena.Handles.Ship.Ship_Handle :=
                     Athena.Ships.Lists.Element (Assigned);
         begin
            Manager.Log ("move " & Ship.Name & " to " & To_Star.Name);
            Ship.Add_Action
              (Athena.Handles.Ship.Actions.Move_To (To_Star));
         end;

         Available.Delete (Assigned);
      end Assign_Ship;

      ------------------------
      -- Check_Scout_Target --
      ------------------------

      procedure Check_Scout_Target
        (Star    : Athena.Handles.Star.Star_Handle;
         Nearest : Athena.Handles.Colony.Colony_Handle;
         Stop    : out Boolean)
      is
         use type Athena.Handles.Star.Star_Handle;
         Closest : Athena.Handles.Ship.Ship_Handle :=
                     Athena.Handles.Ship.Empty_Handle;
         Min_D   : Non_Negative_Real := Non_Negative_Real'Last;
      begin
         Stop := False;
         if Knowledge.Visited (Star) then
            return;
         end if;

         for Ship of Scout_Ships loop
            if Ship.Destination = Star then
               return;
            elsif Ship.Idle then
               if Ship.Star_Location = Nearest.Star then
                  Closest := Ship;
                  exit;
               else
                  declare
                     D : constant Non_Negative_Real :=
                           Athena.Stars.Distance (Ship.Star_Location, Star);
                  begin
                     if D < Min_D then
                        Min_D := D;
                        Closest := Ship;

                     end if;
                  end;
               end if;
            end if;
         end loop;

         if Closest.Has_Element then
            Closest.Add_Action
              (Athena.Handles.Ship.Actions.Move_To (Star));
         else
            Stop := True;
         end if;

      end Check_Scout_Target;

      -------------------
      -- Check_Upgrade --
      -------------------

--        function Check_Upgrade
--          (Scout : Athena.Handles.Ship.Ship_Handle)
--           return Boolean
--        is
--
--           function Needs_Upgrade
--             (Ship : Athena.Handles.Ship.Ship_Handle)
--              return Boolean;
--
--           -------------------
--           -- Needs_Upgrade --
--           -------------------
--
--           function Needs_Upgrade
--             (Ship : Athena.Handles.Ship.Ship_Handle)
--              return Boolean
--           is
--              Result : Boolean := False;
--
--              procedure Check
--                (Module : Athena.Handles.Ship_Module.Ship_Module_Handle);
--
--              -----------
--              -- Check --
--              -----------
--
--              procedure Check
--                (Module : Athena.Handles.Ship_Module.Ship_Module_Handle)
--              is
--              begin
--                 if Module.Tec_Level + 2.0 <
--                   Athena.Empires.Current_Tec_Level
--                     (Ship.Empire,
--                      Module.Ship_Design_Module.Component.Technology)
--                 then
--                    Result := True;
--                 end if;
--              end Check;
--
--           begin
--              Athena.Ships.Iterate_Components (Ship, Check'Access);
--              return Result;
--           end Needs_Upgrade;

--     begin
--
--           if Scout.First_Order = 0
--             and then not Scout.Destination.Has_Element
--             and then Needs_Upgrade (Scout)
--           then
--              if not Scout.Star.Owner.Has_Element
--                or else Scout.Star.Owner.Identifier /= For_Empire.Identifier
--              then
--                 declare
--                    Colony : constant Athena.Handles.Colony.Colony_Handle :=
--                               Athena.Colonies.Nearest_Colony
--                                 (Scout.Empire, Scout.Star);
--                 begin
--                    Athena.Logging.Log
--                      (For_Empire.Name
--                       & "/exploration: sending "
--                       & Scout.Name
--                       & " to "
--                       & Colony.Star.Name
--                       & " for an upgrade");
--                    Athena.Orders.Set_Destination
--                      (Ship        => Scout,
--                       Destination => Colony.Star,
--                       Priority    => Manager.Priority);
--                 end;
--              end if;
--
--              return True;
--
--           else
--              return False;
--           end if;
--
--        end Check_Upgrade;

      ----------------
      -- Score_Star --
      ----------------

      procedure Score_Star
        (Star    : Athena.Handles.Star.Star_Handle;
         Nearest : Athena.Handles.Colony_Reference;
         Stop    : out Boolean)
      is
         use type Athena.Handles.Star.Star_Handle;
         Colony : constant Athena.Handles.Colony.Colony_Handle :=
                    Athena.Handles.Colony.Get (Nearest);
         Score     : constant Non_Negative_Real :=
                       (Colony.Population + Colony.Industry)
                       / Athena.Stars.Distance (Colony.Star, Star);
         Is_Target : Boolean := False;
      begin
         if not Knowledge.Visited (Star) then
            for Ship of Scout_Ships loop
               if Ship.Has_Destination
                 and then Ship.Destination = Star
               then
                  Is_Target := True;
                  exit;
               end if;

               if Ship.Has_Star_Location
                 and then Ship.Star_Location = Star
               then
                  Is_Target := True;
                  exit;
               end if;
            end loop;

            if not Is_Target then
               Neighbours.Append ((Star, Score));
            end if;
         end if;
         Stop := False;
      end Score_Star;

   begin

      declare
         procedure Add_Ship (Reference : Athena.Handles.Ship_Reference);

         --------------
         -- Add_Ship --
         --------------

         procedure Add_Ship (Reference : Athena.Handles.Ship_Reference) is
            Ship : constant Athena.Handles.Ship.Ship_Handle :=
                     Athena.Handles.Ship.Get (Reference);
         begin
            Scout_Ships.Append (Ship);
         end Add_Ship;

      begin
         Empire.Iterate_Managed_Ships
           (Athena.Handles.Exploration_Manager, Add_Ship'Access);
      end;

      Knowledge.Iterate_Neighbours
        (Max_Range, Score_Star'Access);

      Star_Score_Sorting.Sort (Neighbours);

      Manager.Log
        ("neighbour stars:" & Neighbours.Length'Image
         & "; scout ships:" & Scout_Ships.Length'Image);

      declare
         Previous_Score : Non_Negative_Real := 0.0;
         Busy_Count     : Natural := 0;
         Available_Ships : Athena.Ships.Lists.List;
      begin
         for Ship of Scout_Ships loop
            if Ship.Idle then
               Available_Ships.Append (Ship);
            else
               Busy_Count := Busy_Count + 1;
            end if;
         end loop;

         while not Available_Ships.Is_Empty
           and then not Neighbours.Is_Empty
         loop
            Manager.Log
              ("scouting "
               & Neighbours.First_Element.Star.Name & ": score "
               & Image (Neighbours.First_Element.Score));
            Assign_Ship (Available_Ships, Neighbours.First_Element.Star);
            Previous_Score := Neighbours.First_Element.Score;
            Neighbours.Delete_First;
         end loop;

         declare
            Required : Natural := 0;
         begin
            if Available_Ships.Is_Empty
              and then not Neighbours.Is_Empty
              and then Busy_Count <= 2
            then
               while not Neighbours.Is_Empty
                 and then Neighbours.First_Element.Score > Previous_Score * 0.9
                 and then Neighbours.First_Element.Score > 300.0
               loop
                  Manager.Log
                    ("ordering scout so we can explore "
                     & Neighbours.First_Element.Star.Name & ": score "
                     & Image (Neighbours.First_Element.Score));
                  Required := Required + 1;
                  Previous_Score := Neighbours.First_Element.Score;
                  Neighbours.Delete_First;
               end loop;

               if Required > 0 then
                  Manager.Log ("required scouts =" & Required'Image);

                  --  Athena.Orders.Build_Ships
                  --    (Empire   => For_Empire,
                  --     Design   => Athena.Empires.Scout_Design (For_Empire),
                  --     Fleet    => Athena.Handles.Fleet.Empty_Handle,
                  --     Manager  => Manager,
                  --     Send_To  => Athena.Handles.Star.Empty_Handle,
                  --     Count    => Required,
                  --     Priority => Manager.Priority);
               end if;
            end if;
         end;
      end;

      Manager.Set_Next_Update_Delay (Athena.Calendar.Days (10));
   end Create_Orders;

   ---------------------------------
   -- Default_Exploration_Manager --
   ---------------------------------

   function Default_Exploration_Manager
     return Root_Manager_Type'Class
   is
   begin
      return Manager : constant Exploration_Manager :=
        Exploration_Manager'
          (Name     => +"explore",
           Empire   => <>,
           Priority => 1050,
           Next_Update => Athena.Calendar.Clock,
           Messages => <>);
   end Default_Exploration_Manager;

end Athena.Managers.Exploration;
