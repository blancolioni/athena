with Ada.Containers.Doubly_Linked_Lists;

with Athena.Empires;
with Athena.Knowledge.Stars;
with Athena.Ships.Orders;
with Athena.Stars;
with Athena.Turns;

with Minerva.Colony;
with Minerva.Fleet;
with Minerva.Ship;
with Minerva.Ship_Build_Order;
with Minerva.Star;

package body Athena.Managers.Exploration is

   type Exploration_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Exploration_Manager)
      return String
   is ("exploration");

   overriding procedure Create_Orders
     (Manager : Exploration_Manager);

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Ship.Ship_Handle, Minerva.Ship."=");

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Exploration_Manager)
   is
      Max_Range : constant Non_Negative_Real := 20.0;

      type Star_Score_Record is
         record
            Star  : Minerva.Star.Star_Handle;
            Score : Non_Negative_Real;
         end record;

      package Star_Score_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Star_Score_Record);

      function Better (Left, Right : Star_Score_Record) return Boolean
      is (Left.Score > Right.Score);

      package Star_Score_Sorting is
        new Star_Score_Lists.Generic_Sorting (Better);

      Scout_Ships : Ship_Lists.List;
      Neighbours  : Star_Score_Lists.List;

      procedure Check_Scout_Target
        (Star    : Minerva.Star.Star_Handle;
         Nearest : Minerva.Colony.Colony_Handle;
         Stop    : out Boolean)
        with Unreferenced;

--        function Check_Upgrade
--          (Scout : Minerva.Ship.Ship_Handle)
--           return Boolean;

      procedure Score_Star
        (Star    : Minerva.Star.Star_Class;
         Nearest : Minerva.Colony.Colony_Class;
         Stop    : out Boolean);

      procedure Assign_Ship
        (Available : in out Ship_Lists.List;
         To_Star   : Minerva.Star.Star_Class);

      Knowledge : Athena.Knowledge.Stars.Star_Knowledge;

      -----------------
      -- Assign_Ship --
      -----------------

      procedure Assign_Ship
        (Available : in out Ship_Lists.List;
         To_Star   : Minerva.Star.Star_Class)
      is
         Assigned : Ship_Lists.Cursor :=
                      Ship_Lists.No_Element;
         Closest  : Non_Negative_Real := Non_Negative_Real'Last;
      begin
         for Position in Available.Iterate loop
            declare
               D : constant Non_Negative_Real :=
                     Athena.Stars.Distance
                       (Ship_Lists.Element (Position).Star,
                        To_Star);
            begin
               if D < Closest then
                  Closest := D;
                  Assigned := Position;
               end if;
            end;
         end loop;

         pragma Assert (Ship_Lists.Has_Element (Assigned),
                        "expected a non-empty available list");

         declare
            Ship : constant Minerva.Ship.Ship_Handle :=
                     Ship_Lists.Element (Assigned);
         begin
            Manager.Log ("move " & Ship.Name & " to " & To_Star.Name);
            Athena.Ships.Orders.Move_To (Ship, To_Star);
         end;

         Available.Delete (Assigned);
      end Assign_Ship;

      ------------------------
      -- Check_Scout_Target --
      ------------------------

      procedure Check_Scout_Target
        (Star    : Minerva.Star.Star_Handle;
         Nearest : Minerva.Colony.Colony_Handle;
         Stop    : out Boolean)
      is
         Closest : Minerva.Ship.Ship_Handle :=
                     Minerva.Ship.Empty_Handle;
         Min_D   : Non_Negative_Real := Non_Negative_Real'Last;
      begin
         Stop := False;
         if Knowledge.Visited (Star) then
            return;
         end if;

         for Ship of Scout_Ships loop
            if Ship.Destination.Identifier = Star.Identifier then
               return;
            elsif Athena.Ships.Is_Idle (Ship) then
               if Ship.Star.Identifier = Nearest.Star.Identifier then
                  Closest := Ship;
                  exit;
               else
                  declare
                     D : constant Non_Negative_Real :=
                           Athena.Stars.Distance (Ship.Star, Star);
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
            Athena.Ships.Orders.Move_To (Closest, Star);
         else
            Stop := True;
         end if;

      end Check_Scout_Target;

      -------------------
      -- Check_Upgrade --
      -------------------

--        function Check_Upgrade
--          (Scout : Minerva.Ship.Ship_Handle)
--           return Boolean
--        is
--
--           function Needs_Upgrade
--             (Ship : Minerva.Ship.Ship_Handle)
--              return Boolean;
--
--           -------------------
--           -- Needs_Upgrade --
--           -------------------
--
--           function Needs_Upgrade
--             (Ship : Minerva.Ship.Ship_Handle)
--              return Boolean
--           is
--              Result : Boolean := False;
--
--              procedure Check
--                (Module : Minerva.Ship_Module.Ship_Module_Handle);
--
--              -----------
--              -- Check --
--              -----------
--
--              procedure Check
--                (Module : Minerva.Ship_Module.Ship_Module_Handle)
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
--                    Colony : constant Minerva.Colony.Colony_Handle :=
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
        (Star    : Minerva.Star.Star_Class;
         Nearest : Minerva.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         Colony : constant Minerva.Colony.Colony_Handle :=
                    Nearest.To_Colony_Handle;
         Score     : constant Non_Negative_Real :=
                       (Colony.Population + Colony.Industry)
                       / Athena.Stars.Distance (Colony.Star, Star);
         Is_Target : Boolean := False;
      begin
         if not Knowledge.Visited (Star) then
            for Ship of Scout_Ships loop
               if Athena.Ships.Has_Destination (Ship, Star) then
                  Is_Target := True;
                  exit;
               end if;

               if Athena.Ships.Has_Star_Location (Ship, Star) then
                  Is_Target := True;
                  exit;
               end if;
            end loop;

            if not Is_Target then
               Neighbours.Append
                 (Star_Score_Record'(Star.To_Star_Handle, Score));
            end if;
         end if;
         Stop := False;
      end Score_Star;

   begin

      Knowledge.Load (Manager.Empire);

      for Ship of
        Minerva.Ship.Select_By_Empire_Manager
          (Manager.Manager)
      loop
         Scout_Ships.Append (Ship.To_Ship_Handle);
      end loop;

      Knowledge.Iterate_Neighbours
        (Max_Range, Score_Star'Access);

      Star_Score_Sorting.Sort (Neighbours);

      Manager.Log
        ("neighbour stars:" & Neighbours.Length'Image
         & "; scout ships:" & Scout_Ships.Length'Image);

      declare
         Previous_Score : Non_Negative_Real := 0.0;
         Busy_Count     : Natural := 0;
         Available_Ships : Ship_Lists.List;
      begin
         for Ship of Scout_Ships loop
            if Athena.Ships.Is_Idle (Ship) then
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
               & Neighbours.First_Element.Star.Name
               & ": score "
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

                  Minerva.Ship_Build_Order.Create
                    (Turn        => Athena.Turns.Current_Turn,
                     Empire      => Manager.Empire,
                     Priority    => Manager.Priority,
                     Ship_Design =>
                       Athena.Empires.Standard_Scout_Design (Manager.Empire),
                     Manager     => Manager.Manager,
                     Fleet       => Minerva.Fleet.Empty_Handle,
                     Send_To     => Athena.Empires.Capital (Manager.Empire),
                     Count       => Required);

                  --  Athena.Orders.Build_Ships
                  --    (Empire   => For_Empire,
                  --     Design   => Athena.Empires.Scout_Design (For_Empire),
                  --     Fleet    => Minerva.Fleet.Empty_Handle,
                  --     Manager  => Manager,
                  --     Send_To  => Minerva.Star.Empty_Handle,
                  --     Count    => Required,
                  --     Priority => Manager.Priority);
               end if;
            end if;
         end;
      end;

   end Create_Orders;

   ---------------------------------
   -- Default_Exploration_Manager --
   ---------------------------------

   function Default_Exploration_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Manager : constant Exploration_Manager :=
        Exploration_Manager'
          (Name     => +"explore",
           Empire   => <>,
           Priority => 1050,
           Manager => <>);
   end Default_Exploration_Manager;

end Athena.Managers.Exploration;
