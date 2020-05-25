with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;
with WL.String_Sets;

with Athena.Logging;
with Athena.Money;

--  with Athena.Managers;
with Athena.Ships.Lists;
with Athena.Treaties;

--  with Athena.Orders.Colonies;
--  with Athena.Orders.Empires;
--  with Athena.Orders.Ships;

with Athena.Colonies;
with Athena.Empires;
with Athena.Ships.Updates;

--  with Athena.Encounters.Manager;
--  with Athena.Treaties;

--  with Athena.Db.Colony_Order;
--  with Athena.Db.Research_Order;
--  with Athena.Db.Ship_Build_Order;
--  with Athena.Db.Upgrade_Order;

--  with Athena.Handles.Colony.Selections;
with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Order;
--  with Athena.Handles.Fleet_Order.Selections;
--  with Athena.Handles.Research_Order;
with Athena.Handles.Ship;
--  with Athena.Handles.Ship_Build_Order;
with Athena.Handles.Star;
--  with Athena.Handles.Upgrade_Order;
with Athena.Handles.War;

package body Athena.Updates is

   procedure Debt_Service
     (Empire : Athena.Handles.Empire.Empire_Handle);

   procedure Fleet_Cost
     (Fleet_Owner : Athena.Handles.Empire.Empire_Handle);

   --  procedure Execute_Colony_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle);

   --  procedure Execute_Upgrade_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle);
   --
   --  procedure Execute_Build_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle);
   --
   --  procedure Execute_Research_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle);

   procedure Before_Production
     (Colony : Athena.Handles.Colony.Colony_Handle);

   procedure After_Production
     (Colony : Athena.Handles.Colony.Colony_Handle);

   procedure Check_Colony_Owner
     (Colony : Athena.Handles.Colony.Colony_Handle);

   procedure Run_Encounters with Unreferenced;

   ----------------------
   -- After_Production --
   ----------------------

   procedure After_Production
     (Colony : Athena.Handles.Colony.Colony_Handle)
   is
   begin
      Athena.Empires.Earn
        (Colony.Owner, Athena.Money.To_Money (Colony.Construct),
         "unused production on " & Colony.Star.Name);
      Colony.Set_Construct (0.0);
   end After_Production;

   -----------------------
   -- Before_Production --
   -----------------------

   procedure Before_Production
     (Colony : Athena.Handles.Colony.Colony_Handle)
   is
      Construct : constant Non_Negative_Real :=
                    Real'Min (Colony.Industry, Colony.Population)
                    + (Real'Max (Colony.Industry, Colony.Population)
                       - Colony.Industry)
                    / 4.0;
      New_Pop   : constant Non_Negative_Real :=
                    Colony.Population
                      + 0.1 * Colony.Population * Colony.Star.Habitability;
      Max_Pop   : constant Non_Negative_Real :=
                    Non_Negative_Real (Colony.Star.Space);
   begin
      Colony.Set_Construct (Construct);
      Colony.Set_Population (Real'Min (New_Pop, Max_Pop));
   end Before_Production;

   ------------------------
   -- Check_Colony_Owner --
   ------------------------

   procedure Check_Colony_Owner
     (Colony : Athena.Handles.Colony.Colony_Handle)
   is
      Current_Owner : constant Athena.Handles.Empire.Empire_Handle :=
                        Colony.Owner;
      New_Owner     : Athena.Handles.Empire.Empire_Handle :=
                        Athena.Handles.Empire.Empty_Handle;
      Star          : constant Athena.Handles.Star.Star_Handle :=
                        Colony.Star;
      Present       : WL.String_Sets.Set;
      Count         : Natural := 0;

      procedure Record_Empire
        (Reference : Athena.Handles.Ship_Reference);

      -------------------
      -- Record_Empire --
      -------------------

      procedure Record_Empire
        (Reference : Athena.Handles.Ship_Reference)
      is
         use type Athena.Handles.Empire.Empire_Handle;
         Ship : constant Athena.Handles.Ship.Ship_Handle :=
                  Athena.Handles.Ship.Get (Reference);
      begin
         if Ship.Alive
           and then Athena.Ships.Is_Armed (Ship)
           and then not Present.Contains (Ship.Owner.Identifier)
           and then (Ship.Owner = Colony.Owner
                     or else Athena.Treaties.At_War
                       (Colony.Owner, Ship.Owner))
         then
            Count := Count + 1;
            Present.Include (Ship.Owner.Identifier);
            if Ship.Owner /= Current_Owner then
               New_Owner := Ship.Owner;
            end if;
         end if;
      end Record_Empire;

   begin
      Athena.Handles.Star.Iterate_Orbiting_Ships
        (Star, Record_Empire'Access);
      if not Present.Contains (Current_Owner.Identifier)
        and then Count = 1
      then
         Athena.Colonies.Capture_Colony (Colony, New_Owner);
         Athena.Logging.Log
           (New_Owner.Name & " captures colony on "
            & Star.Name & " from " & Current_Owner.Name);
      end if;
   end Check_Colony_Owner;

   ------------------
   -- Debt_Service --
   ------------------

   procedure Debt_Service
     (Empire : Athena.Handles.Empire.Empire_Handle)
   is
      use Athena.Money;
      Repay : constant Money_Type :=
                Min (Empire.Cash, Empire.Debt);
      New_Cash : constant Money_Type :=
                   Empire.Cash - Repay;
      New_Debt : constant Money_Type :=
                   Empire.Debt - Repay;
      Interest : constant Money_Type :=
                   Adjust (New_Debt, 0.05);
   begin
      if Repay > Zero then
         Athena.Logging.Log
           (Empire.Name & ": repaid " & Athena.Money.Show (Repay)
            & " debt"
            & ": remaining cash " & Athena.Money.Show (New_Cash)
            & "; remaining debt " & Athena.Money.Show (New_Debt));
      end if;
      if New_Debt > Zero then
         Athena.Logging.Log
           (Empire.Name & ": debt " & Athena.Money.Show (New_Debt)
            & "; interest " & Athena.Money.Show (Interest)
            & "; new debt " & Athena.Money.Show (New_Debt + Interest));
      end if;

      Empire.Set_Cash (New_Cash);
      Empire.Set_Debt (New_Debt);

   end Debt_Service;

   --------------------------
   -- Execute_Build_Orders --
   --------------------------

   --  procedure Execute_Build_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle)
   --  is
   --  begin
   --     for Order of
   --    Athena.Db.Ship_Build_Order.Select_Priority_Order_Bounded_By_Priority
   --         (Athena.Turns.Current_Turn.Reference_Turn,
   --          For_Empire.Reference_Empire,
   --          1, Natural'Last)
   --     loop
   --        Athena.Orders.Empires.Apply_Ship_Build_Order
   --          (Athena.Handles.Ship_Build_Order.Get
   --             (Order.Get_Ship_Build_Order_Reference));
   --     end loop;
   --  end Execute_Build_Orders;

   ---------------------------
   -- Execute_Colony_Orders --
   ---------------------------

   --  procedure Execute_Colony_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle)
   --  is
   --  begin
   --     for Order of
   --       Athena.Db.Colony_Order.Select_Priority_Order_Bounded_By_Priority
   --         (Athena.Turns.Current_Turn.Reference_Turn,
   --          For_Empire.Reference_Empire,
   --          1, Natural'Last)
   --     loop
   --        Athena.Logging.Log
   --          (For_Empire.Name & ": processing "
   --           & Order.Category'Image);
   --        Athena.Orders.Colonies.Apply_Colony_Order
   --          (Athena.Handles.Colony_Order.Get
   --             (Order.Get_Colony_Order_Reference));
   --     end loop;
   --  end Execute_Colony_Orders;

   -----------------------------
   -- Execute_Research_Orders --
   -----------------------------

   --  procedure Execute_Research_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle)
   --  is
   --  begin
   --     for Order of
   --       Athena.Db.Research_Order.Select_Priority_Order_Bounded_By_Priority
   --         (Athena.Turns.Current_Turn.Reference_Turn,
   --          For_Empire.Reference_Empire,
   --          1, Natural'Last)
   --     loop
   --        Athena.Orders.Empires.Apply_Research_Order
   --          (Athena.Handles.Research_Order.Get
   --             (Order.Get_Research_Order_Reference));
   --     end loop;
   --  end Execute_Research_Orders;

   ----------------------------
   -- Execute_Upgrade_Orders --
   ----------------------------

   --  procedure Execute_Upgrade_Orders
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle)
   --  is null;
--     begin
--        for Order of
--          Athena.Db.Upgrade_Order.Select_Priority_Order_Bounded_By_Priority
--            (Athena.Turns.Current_Turn.Reference_Turn,
--             For_Empire.Reference_Empire,
--             1, Natural'Last)
--        loop
--           Athena.Orders.Ships.Apply_Upgrade_Order
--             (Athena.Handles.Upgrade_Order.Get
--                (Order.Get_Upgrade_Order_Reference));
--        end loop;
--     end Execute_Upgrade_Orders;

   ----------------
   -- Fleet_Cost --
   ----------------

   procedure Fleet_Cost
     (Fleet_Owner : Athena.Handles.Empire.Empire_Handle)
   is
      Total_Ships : Natural := 0;
      Total_Mass  : Non_Negative_Real := 0.0;

      procedure Bill_Ship (Reference : Athena.Handles.Ship_Reference);

      ---------------
      -- Bill_Ship --
      ---------------

      procedure Bill_Ship (Reference : Athena.Handles.Ship_Reference) is
         Ship : constant Athena.Handles.Ship.Ship_Handle :=
                  Athena.Handles.Ship.Get (Reference);
      begin
         if Ship.Alive then
            Total_Ships := Total_Ships + 1;
            Total_Mass := Total_Mass + Athena.Ships.Tonnage (Ship);
         end if;
      end Bill_Ship;

   begin
      Fleet_Owner.Iterate_Ships (Bill_Ship'Access);

      declare
         Total : constant Athena.Money.Money_Type :=
                   Athena.Money.To_Money
                     (Total_Mass / 2.0 + 5.0 * Real (Total_Ships));
      begin
         Athena.Empires.Pay (Fleet_Owner, Total, "fleet maintenance");
      end;
   end Fleet_Cost;

   --------------------
   -- Run_Encounters --
   --------------------

   procedure Run_Encounters is

      type Force_Record is
         record
            Empire : Athena.Handles.Empire.Empire_Handle;
            Size   : Natural;
         end record;

      package Force_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Force_Record);

      type Star_Record is
         record
            Star    : Athena.Handles.Star.Star_Handle;
            War     : Athena.Handles.War.War_Handle;
            Ships   : Athena.Ships.Lists.List;
            Forces  : Force_Lists.List;
            Hostile : Boolean;
         end record;

      function Encounter_Size (Rec : Star_Record) return Positive
        with Pre => Rec.Hostile, Unreferenced;

      package Star_Record_Maps is
        new WL.String_Maps (Star_Record);

      Star_Map : Star_Record_Maps.Map;

      procedure Record_Ship (Ship : Athena.Handles.Ship.Ship_Handle);

      --------------------
      -- Encounter_Size --
      --------------------

      function Encounter_Size (Rec : Star_Record) return Positive is

         use Force_Lists;

         function More (Left, Right : Force_Record) return Boolean
         is (Left.Size > Right.Size);

         package Force_Sorting is new Force_Lists.Generic_Sorting (More);

         Sorted_List : Force_Lists.List := Rec.Forces;
      begin
         Force_Sorting.Sort (Sorted_List);
         return Element (Next (Sorted_List.First)).Size;
      end Encounter_Size;

      -----------------
      -- Record_Ship --
      -----------------

      procedure Record_Ship (Ship : Athena.Handles.Ship.Ship_Handle) is
      begin

         if not Ship.Alive
           or else not Ship.Has_Star_Location
         then
            return;
         end if;

         if not Star_Map.Contains (Ship.Star_Location.Identifier) then
            Star_Map.Insert
              (Ship.Star_Location.Identifier,
               Star_Record'
                 (Star    => Ship.Star_Location,
                  War     => Athena.Handles.War.Empty_Handle,
                  Ships   => <>,
                  Forces  => <>,
                  Hostile => False));
         end if;

         declare
            use type Athena.Handles.Empire.Empire_Handle;
            Rec : Star_Record renames Star_Map (Ship.Star_Location.Identifier);
         begin
            if not Rec.Hostile then
               declare
                  Armed : constant Boolean := Athena.Ships.Is_Armed (Ship);
               begin
                  for Other_Ship of Rec.Ships loop
                     if (Armed or else Athena.Ships.Is_Armed (Other_Ship))
                       and then Other_Ship.Owner /= Ship.Owner
                     then
                        Athena.Logging.Log
                          ("encounter: checking treaty between "
                           & Other_Ship.Owner.Name
                           & " and " & Ship.Owner.Name);
                     end if;

                     if Athena.Treaties.At_War
                       (Other_Ship.Owner, Ship.Owner)
                     then
                        Rec.War :=
                          Athena.Treaties.Get_War
                            (Other_Ship.Owner, Ship.Owner);
                        Rec.Hostile := True;
                     end if;
                  end loop;
               end;
            end if;

            declare
               Found_Empire : Boolean := False;
            begin
               Rec.Ships.Append (Ship);

               for Force of Rec.Forces loop
                  if Force.Empire = Ship.Owner then
                     Force.Size := Force.Size + 1;
                     Found_Empire := True;
                     exit;
                  end if;
               end loop;

               if not Found_Empire then
                  Rec.Forces.Append ((Ship.Owner, 1));
               end if;
            end;
         end;
      end Record_Ship;

   begin
      Athena.Handles.Ship.Iterate_All (Record_Ship'Access);

      for Element of Star_Map loop
         if Element.Hostile then
            --  Athena.Encounters.Manager.Resolve_Encounter
            --    (Star  => Element.Star,
            --     War   => Element.War,
            --     Ships => Element.Ships,
            --     Size  => Encounter_Size (Element));
            null;
         end if;
      end loop;

   end Run_Encounters;

   ----------------
   -- Run_Update --
   ----------------

   procedure Run_Update is

      Managers : constant array (Positive range <>)
        of Athena.Handles.Manager_Class :=
          (1 => Athena.Handles.Development_Manager,
           2 => Athena.Handles.Exploration_Manager,
           3 => Athena.Handles.Colonization_Manager,
           4 => Athena.Handles.Transport_Manager);

   begin
      Athena.Logging.Start_Logging
        ("update-"
         & Athena.Handles.File_Name_Turn_Image);

      Athena.Handles.Empire.Iterate_All (Fleet_Cost'Access);
      --  Athena.Handles.Ship.Iterate_All (Athena.Ships.Updates.Repair'Access);

      declare
         procedure Reload_Knowledge
           (Empire : Athena.Handles.Empire.Empire_Handle);

         ----------------------
         -- Reload_Knowledge --
         ----------------------

         procedure Reload_Knowledge
           (Empire : Athena.Handles.Empire.Empire_Handle)
         is
         begin
            Empire.Knowledge.Load;
         end Reload_Knowledge;

      begin
         Athena.Handles.Empire.Iterate_All (Reload_Knowledge'Access);
      end;

      for Manager of Managers loop
         declare
            procedure Run_Manager
              (Empire : Athena.Handles.Empire.Empire_Handle);

            -----------------
            -- Run_Manager --
            -----------------

            procedure Run_Manager
              (Empire : Athena.Handles.Empire.Empire_Handle)
            is
            begin
               if Empire.Has_Manager (Manager) then
                  Empire.Run_Manager (Manager);
               end if;
            end Run_Manager;

         begin
            Athena.Handles.Empire.Iterate_All
              (Run_Manager'Access);
         end;
      end loop;

      Athena.Handles.Colony.Iterate_All (Before_Production'Access);

      Athena.Handles.Order.Execute_Orders;

      --  For_All_Empires (Execute_Upgrade_Orders'Access);

      --  For_All_Empires (Execute_Colony_Orders'Access);

      --  For_All_Empires (Execute_Build_Orders'Access);

      --  For_All_Empires (Execute_Research_Orders'Access);

      --  declare
      --     use Athena.Handles.Fleet_Order.Selections;
      --  begin
      --     for Order of
      --       Select_Where (Turn = Athena.Turns.Current_Turn)
      --     loop
      --        Order.Fleet.Update_Fleet
      --          .Set_Destination (Order.Destination.Reference_Star)
      --          .Set_Progress (0.0)
      --          .Done;
      --     end loop;
      --  end;

      --  Athena.Ships.For_All_Fleets (Athena.Ships.Updates.Update'Access);

      Athena.Handles.Ship.Iterate_All
        (Athena.Ships.Updates.Execute_Actions'Access);

      Athena.Handles.Colony.Iterate_All (After_Production'Access);

      --  Athena.Handles.Ship.Iterate_All (Athena.Ships.Updates.Visit'Access);

      --  Run_Encounters;

      Athena.Handles.Colony.Iterate_All (Check_Colony_Owner'Access);

      Athena.Handles.Empire.Iterate_All (Debt_Service'Access);

      Athena.Handles.Next_Turn;

      Athena.Logging.Stop_Logging;
   end Run_Update;

end Athena.Updates;
