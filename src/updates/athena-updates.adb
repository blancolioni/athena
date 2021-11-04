with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;
with WL.String_Sets;

with Athena.Logging;
with Athena.Money;
with Athena.Turns;

--  with Athena.Ships.Lists;
--  with Athena.Treaties;

--  with Athena.Orders.Colonies;
--  with Athena.Orders.Empires;
--  with Athena.Orders.Ships;

with Athena.Colonies.Updates;
with Athena.Empires.Orders;
with Athena.Ships;

with Athena.Empires.Managers;

with Athena.Ships.Orders;
with Athena.Ships.Updates;

with Athena.Encounters.Manager;
with Athena.Treaties;

--  with Minerva.Db.Colony_Order;
--  with Minerva.Db.Research_Order;
--  with Minerva.Db.Ship_Build_Order;
--  with Minerva.Db.Upgrade_Order;

--  with Minerva.Colony.Selections;
with Minerva.Colony;
with Minerva.Colony_Order;
with Minerva.Empire;
with Minerva.Empire_Manager;
with Minerva.Manager;
--  with Minerva.Order;
--  with Minerva.Fleet_Order.Selections;
with Minerva.Research_Order;
with Minerva.Ship;
with Minerva.Ship_Build_Order;
with Minerva.Ship_Knowledge;
with Minerva.Star;
with Minerva.Star_Knowledge;
with Minerva.Upgrade_Order;
with Minerva.War;

package body Athena.Updates is

   procedure Debt_Service
     (Empire : Minerva.Empire.Empire_Class);

   procedure Fleet_Cost
     (Fleet_Owner : Minerva.Empire.Empire_Class);

   procedure Execute_Colony_Orders
     (For_Empire : Minerva.Empire.Empire_Class);

   procedure Execute_Upgrade_Orders
     (For_Empire : Minerva.Empire.Empire_Class);
   --
   procedure Execute_Build_Orders
     (For_Empire : Minerva.Empire.Empire_Class);

   procedure Execute_Research_Orders
     (For_Empire : Minerva.Empire.Empire_Class);

   procedure Before_Production
     (Colony : Minerva.Colony.Colony_Class);

   procedure After_Production
     (Colony : Minerva.Colony.Colony_Class);

   procedure Check_Colony_Owner
     (Colony : Minerva.Colony.Colony_Class);

   procedure Update_Knowledge
     (Empire : Minerva.Empire.Empire_Class);

   procedure Run_Encounters;

   procedure For_All_Empires
     (Update : not null access
        procedure (Empire : Minerva.Empire.Empire_Class));

   procedure For_All_Colonies
     (Update : not null access
        procedure (Colony : Minerva.Colony.Colony_Class));

   ----------------------
   -- After_Production --
   ----------------------

   procedure After_Production
     (Colony : Minerva.Colony.Colony_Class)
   is
   begin
      Athena.Empires.Earn
        (Colony.Empire, Athena.Money.To_Money (Colony.Construct),
         "unused production on " & Colony.Star.Name);
      Colony.Update_Colony
        .Set_Construct (0.0)
        .Done;
   end After_Production;

   -----------------------
   -- Before_Production --
   -----------------------

   procedure Before_Production
     (Colony : Minerva.Colony.Colony_Class)
   is
      From_Industry : constant Non_Negative_Real :=
                        Real'Min (Colony.Industry, Colony.Population);
      From_Pop      : constant Non_Negative_Real :=
                        (Colony.Population - From_Industry) / 4.0;
      Construct     : constant Non_Negative_Real :=
                        From_Industry + From_Pop;
      New_Pop   : constant Non_Negative_Real :=
                    Colony.Population
                      + 0.1 * Colony.Population * Colony.Star.Habitability;
      Max_Pop   : constant Non_Negative_Real :=
                    Non_Negative_Real (Colony.Star.Space);
   begin
      Colony.Update_Colony
        .Set_Construct (Construct)
        .Set_Population (Real'Min (New_Pop, Max_Pop))
          .Done;
   end Before_Production;

   ------------------------
   -- Check_Colony_Owner --
   ------------------------

   procedure Check_Colony_Owner
     (Colony : Minerva.Colony.Colony_Class)
   is
      Current_Owner : constant Minerva.Empire.Empire_Class :=
                        Colony.Empire;
      New_Owner     : Minerva.Empire.Empire_Handle;
      Star          : constant Minerva.Star.Star_Class :=
                        Colony.Star;
      Present       : WL.String_Sets.Set;
      Count         : Natural := 0;

      procedure Record_Empire
        (Ship : Minerva.Ship.Ship_Class);

      -------------------
      -- Record_Empire --
      -------------------

      procedure Record_Empire
        (Ship : Minerva.Ship.Ship_Class)
      is
      begin
         if Ship.Alive
           and then Athena.Ships.Is_Armed (Ship)
           and then not Present.Contains (Ship.Empire.Identifier)
           and then (Ship.Empire.Identifier = Colony.Empire.Identifier
                     or else Athena.Treaties.At_War
                       (Colony.Empire, Ship.Empire))
         then
            Count := Count + 1;
            Present.Include (Ship.Empire.Identifier);
            if Ship.Empire.Identifier /= Current_Owner.Identifier then
               New_Owner := Ship.Empire.To_Empire_Handle;
            end if;
         end if;
      end Record_Empire;

   begin
      for Ship of Minerva.Ship.Select_By_Star (Star) loop
         Record_Empire (Ship);
      end loop;

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
     (Empire : Minerva.Empire.Empire_Class)
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

      Empire.Update_Empire
        .Set_Cash (New_Cash)
        .Set_Debt (New_Debt)
        .Done;

   end Debt_Service;

   --------------------------
   -- Execute_Build_Orders --
   --------------------------

   procedure Execute_Build_Orders
     (For_Empire : Minerva.Empire.Empire_Class)
   is
   begin
      for Order of
        Minerva.Ship_Build_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn,
           For_Empire,
           1, Natural'Last)
      loop
         Athena.Empires.Orders.Apply_Ship_Build_Order (Order);
      end loop;
   end Execute_Build_Orders;

   ---------------------------
   -- Execute_Colony_Orders --
   ---------------------------

   procedure Execute_Colony_Orders
     (For_Empire : Minerva.Empire.Empire_Class)
   is
   begin
      for Order of
        Minerva.Colony_Order.Select_Priority_Order_Bounded_By_Priority
          (Turn            => Athena.Turns.Current_Turn,
           Empire          => For_Empire,
           Start_Priority  => 0,
           Finish_Priority => Natural'Last)
      loop
         Athena.Colonies.Log_Colony
           (Order.Colony, "processing " & Order.Category'Image);

         Athena.Colonies.Updates.Execute_Colony_Order (Order);

         --  Athena.Orders.Colonies.Apply_Colony_Order
         --    (Minerva.Colony_Order.Get
         --       (Order.Get_Colony_Order_Reference));
      end loop;
   end Execute_Colony_Orders;

   -----------------------------
   -- Execute_Research_Orders --
   -----------------------------

   procedure Execute_Research_Orders
     (For_Empire : Minerva.Empire.Empire_Class)
   is
   begin
      for Order of
        Minerva.Research_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn,
           For_Empire,
           1, Natural'Last)
      loop
         Athena.Empires.Orders.Apply_Research_Order (Order);
      end loop;
   end Execute_Research_Orders;

   ----------------------------
   -- Execute_Upgrade_Orders --
   ----------------------------

   procedure Execute_Upgrade_Orders
     (For_Empire : Minerva.Empire.Empire_Class)
   is
   begin
      for Order of
        Minerva.Upgrade_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn, For_Empire,
           1, Natural'Last)
      loop
         Athena.Ships.Orders.Apply_Upgrade_Order (Order);
      end loop;
   end Execute_Upgrade_Orders;

   ----------------
   -- Fleet_Cost --
   ----------------

   procedure Fleet_Cost
     (Fleet_Owner : Minerva.Empire.Empire_Class)
   is
      Total_Ships : Natural := 0;
      Total_Mass  : Non_Negative_Real := 0.0;

      procedure Bill_Ship (Ship : Minerva.Ship.Ship_Class);

      ---------------
      -- Bill_Ship --
      ---------------

      procedure Bill_Ship (Ship : Minerva.Ship.Ship_Class) is
      begin
         if Ship.Alive then
            Total_Ships := Total_Ships + 1;
            Total_Mass := Total_Mass + Athena.Ships.Dry_Mass (Ship);
         end if;
      end Bill_Ship;

   begin

      for Ship of Minerva.Ship.Select_By_Empire (Fleet_Owner) loop
         Bill_Ship (Ship);
      end loop;

      declare
         Total : constant Athena.Money.Money_Type :=
                   Athena.Money.To_Money
                     (Total_Mass / 2.0 + 5.0 * Real (Total_Ships));
      begin
         Athena.Empires.Pay (Fleet_Owner, Total, "fleet maintenance");
      end;
   end Fleet_Cost;

   ----------------------
   -- For_All_Colonies --
   ----------------------

   procedure For_All_Colonies
     (Update : not null access
        procedure (Colony : Minerva.Colony.Colony_Class))
   is
   begin
      for Colony of
        Minerva.Colony.Scan_By_Identifier
      loop
         Update (Colony);
      end loop;
   end For_All_Colonies;

   ---------------------
   -- For_All_Empires --
   ---------------------

   procedure For_All_Empires
     (Update : not null access
        procedure (Empire : Minerva.Empire.Empire_Class))
   is
   begin
      for Empire of
        Minerva.Empire.Scan_By_Identifier
      loop
         Update (Empire);
      end loop;
   end For_All_Empires;

   --------------------
   -- Run_Encounters --
   --------------------

   procedure Run_Encounters is

      type Force_Record is
         record
            Empire : Minerva.Empire.Empire_Handle;
            Size   : Natural;
         end record;

      package Force_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Force_Record);

      type Star_Record is
         record
            Star    : Minerva.Star.Star_Handle;
            War     : Minerva.War.War_Handle;
            Ships   : Athena.Encounters.Manager.Ship_Lists.List;
            Forces  : Force_Lists.List;
            Hostile : Boolean;
         end record;

      function Encounter_Size (Rec : Star_Record) return Positive
        with Pre => Rec.Hostile;

      package Star_Record_Maps is
        new WL.String_Maps (Star_Record);

      Star_Map : Star_Record_Maps.Map;

      procedure Record_Ship (Ship : Minerva.Ship.Ship_Class);

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

      procedure Record_Ship (Ship : Minerva.Ship.Ship_Class) is
      begin

         if not Ship.Alive
           or else not Athena.Ships.Has_Star_Location (Ship)
         then
            return;
         end if;

         if not Star_Map.Contains (Ship.Star.Identifier) then
            Star_Map.Insert
              (Ship.Star.Identifier,
               Star_Record'
                 (Star    => Ship.Star.To_Star_Handle,
                  War     => Minerva.War.Empty_Handle,
                  Ships   => <>,
                  Forces  => <>,
                  Hostile => False));
         end if;

         declare
            Rec : Star_Record renames Star_Map (Ship.Star.Identifier);
         begin
            if not Rec.Hostile then
               declare
                  Armed : constant Boolean := Athena.Ships.Is_Armed (Ship);
               begin
                  for Other_Ship of Rec.Ships loop
                     if (Armed or else Athena.Ships.Is_Armed (Other_Ship))
                       and then Other_Ship.Empire.Identifier
                         /= Ship.Empire.Identifier
                     then
                        Athena.Logging.Log
                          ("encounter: checking treaty between "
                           & Other_Ship.Empire.Name
                           & " and " & Ship.Empire.Name);
                     end if;

                     if Athena.Treaties.At_War
                       (Other_Ship.Empire, Ship.Empire)
                     then
                        Rec.War :=
                          Athena.Treaties.Get_War
                            (Other_Ship.Empire, Ship.Empire)
                          .To_War_Handle;
                        Rec.Hostile := True;
                     end if;
                  end loop;
               end;
            end if;

            declare
               Found_Empire : Boolean := False;
            begin
               Rec.Ships.Append (Ship.To_Ship_Handle);

               for Force of Rec.Forces loop
                  if Force.Empire.Identifier = Ship.Empire.Identifier then
                     Force.Size := Force.Size + 1;
                     Found_Empire := True;
                     exit;
                  end if;
               end loop;

               if not Found_Empire then
                  Rec.Forces.Append ((Ship.Empire.To_Empire_Handle, 1));
               end if;
            end;
         end;
      end Record_Ship;

   begin

      for Ship of Minerva.Ship.Select_By_Alive (True) loop
         Record_Ship (Ship);
      end loop;

      for Element of Star_Map loop
         if Element.Hostile then
            Athena.Encounters.Manager.Resolve_Encounter
              (Star  => Element.Star,
               War   => Element.War,
               Ships => Element.Ships,
               Size  => Encounter_Size (Element));
         end if;
      end loop;

   end Run_Encounters;

   ----------------
   -- Run_Update --
   ----------------

   procedure Run_Update is

      Managers : constant array (Positive range <>)
        of Minerva.Manager.Manager_Handle :=
          (Minerva.Manager.Get_By_Tag ("develop"),
           Minerva.Manager.Get_By_Tag ("research"),
           Minerva.Manager.Get_By_Tag ("defend"),
           Minerva.Manager.Get_By_Tag ("attack"),
           Minerva.Manager.Get_By_Tag ("explore"),
           Minerva.Manager.Get_By_Tag ("colonize"),
           Minerva.Manager.Get_By_Tag ("upgrade"),
           Minerva.Manager.Get_By_Tag ("transport"));

   begin
      Athena.Logging.Start_Logging
        ("update-"
         & Athena.Turns.Current_Turn_Image);

      For_All_Empires (Fleet_Cost'Access);

      --  Minerva.Ship.Iterate_All (Athena.Ships.Updates.Repair'Access);

      declare
         procedure Reload_Knowledge
           (Empire : Minerva.Empire.Empire_Class);

         ----------------------
         -- Reload_Knowledge --
         ----------------------

         procedure Reload_Knowledge
           (Empire : Minerva.Empire.Empire_Class)
         is null;
         --  begin
         --     Empire.Knowledge.Load;
         --  end Reload_Knowledge;

      begin
         For_All_Empires (Reload_Knowledge'Access);
      end;

      For_All_Colonies (Before_Production'Access);

      for Manager of Managers loop
         for Empire of Minerva.Empire.Scan_By_Top_Record loop
            declare
               Empire_Manager : constant Minerva.Empire_Manager
                 .Empire_Manager_Handle
                   := Minerva.Empire_Manager.Get_By_Empire_Manager
                     (Empire, Manager);
            begin
               if Empire_Manager.Has_Element
                 and then Empire_Manager.Enabled
               then
                  Athena.Empires.Managers.Run_Manager
                    (Empire, Manager, Empire_Manager.Script);
               end if;
            end;
         end loop;
      end loop;

      --  Minerva.Order.Execute_Orders;

      for Ship of Minerva.Ship.Select_By_Alive (True) loop
         Athena.Ships.Updates.Update_Experience
           (Ship, 0.01);
      end loop;

      For_All_Empires (Execute_Upgrade_Orders'Access);

      For_All_Empires (Execute_Colony_Orders'Access);

      For_All_Empires (Execute_Build_Orders'Access);

      For_All_Empires (Execute_Research_Orders'Access);

      --  declare
      --     use Minerva.Fleet_Order.Selections;
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

      --  Minerva.Ship.Iterate_All
      --    (Athena.Ships.Updates.Execute_Actions'Access);

      For_All_Colonies (After_Production'Access);

      for I in 1 .. 4 loop
         Athena.Ships.Updates.Update_Fleets (0.25);
         Athena.Ships.Updates.Update_Ships (0.25);
      end loop;

      --  Run_Encounters;

      For_All_Colonies (Check_Colony_Owner'Access);

      For_All_Empires (Update_Knowledge'Access);

      Run_Encounters;

      For_All_Empires (Debt_Service'Access);

      Athena.Turns.Next_Turn;

      Athena.Logging.Stop_Logging;
   end Run_Update;

   ----------------------
   -- Update_Knowledge --
   ----------------------

   procedure Update_Knowledge
     (Empire : Minerva.Empire.Empire_Class)
   is
      Colony_Ids  : WL.String_Sets.Set;
      Visited_Ids : WL.String_Sets.Set;

      procedure Update_Star_Knowledge
        (Id : String);

      ---------------------------
      -- Update_Star_Knowledge --
      ---------------------------

      procedure Update_Star_Knowledge
        (Id : String)
      is
         Star : constant Minerva.Star.Star_Handle :=
                  Minerva.Star.Get_By_Identifier (Id);

         Colony : constant Minerva.Colony.Colony_Class :=
                    Athena.Colonies.Get_Colony (Star);

         function Update_Star_Knowledge
           return Minerva.Star_Knowledge.Star_Knowledge_Class;

         ---------------------------
         -- Update_Star_Knowledge --
         ---------------------------

         function Update_Star_Knowledge
           return Minerva.Star_Knowledge.Star_Knowledge_Class
         is
            use Minerva.Star_Knowledge;
            Knowledge : constant Star_Knowledge_Class :=
                          Get_By_Star_Knowledge (Star, Empire);
            Last_Pop  : constant Non_Negative_Real :=
                          (if Colony.Has_Element
                           then Colony.Population
                           else 0.0);
            Last_Ind  : constant Non_Negative_Real :=
                          (if Colony.Has_Element
                           then Colony.Industry
                           else 0.0);
         begin
            if Knowledge.Has_Element then
               Knowledge.Update_Star_Knowledge
                 .Set_Last_Visit (Athena.Turns.Current_Turn)
                 .Set_Last_Pop (Last_Pop)
                 .Set_Last_Ind (Last_Ind)
                 .Set_Owner (Star.Owner)
                 .Set_Visited (True)
                 .Done;
               return Knowledge;
            else
               return Minerva.Star_Knowledge.Create
                 (Star       => Star,
                  Empire     => Empire,
                  Owner      => Star.Owner,
                  Last_Visit => Athena.Turns.Current_Turn,
                  Last_Pop   => Last_Pop,
                  Last_Ind   => Last_Ind,
                  Visited    => True,
                  Colonizing => False);
            end if;
         end Update_Star_Knowledge;

         Star_Knowledge : constant Minerva.Star_Knowledge.Star_Knowledge_Class
           := Update_Star_Knowledge;

      begin
         for Ship of
           Minerva.Ship.Select_By_Star (Star)
         loop
            if Ship.Empire.Identifier /= Empire.Identifier
              and then not Athena.Ships.Has_Destination (Ship)
            then
               Minerva.Ship_Knowledge.Create
                 (Star_Knowledge => Star_Knowledge,
                  Owner          => Ship.Empire,
                  Turn           => Athena.Turns.Current_Turn,
                  Identifier     => Ship.Identifier,
                  Name           => Ship.Name,
                  Mass           => Athena.Ships.Dry_Mass (Ship),
                  Weapon_Mass    => Athena.Ships.Weapon_Mass (Ship),
                  Drive          => Athena.Ships.Drive_Mass (Ship));
            end if;
         end loop;
      end Update_Star_Knowledge;

   begin
      for Colony of
        Minerva.Colony.Select_By_Empire (Empire)
      loop
         Colony_Ids.Include (Colony.Star.Identifier);
      end loop;

      for Ship of
        Minerva.Ship.Select_By_Empire (Empire)
      loop
         if Athena.Ships.Has_Star_Location (Ship)
           and then not Colony_Ids.Contains (Ship.Star.Identifier)
         then
            Visited_Ids.Include (Ship.Star.Identifier);
         end if;
      end loop;

      Colony_Ids.Iterate (Update_Star_Knowledge'Access);
      Visited_Ids.Iterate (Update_Star_Knowledge'Access);

   end Update_Knowledge;

end Athena.Updates;
