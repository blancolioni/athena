with Athena.Real_Images;

with Athena.Colonies;
with Athena.Stars;

with Minerva.Ship_Order;

with Minerva.Db;

package body Athena.Ships.Updates is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Next_Order
     (Ship       : Ship_Class;
      Turn_Delta : Unit_Real)
     with Pre => Has_Orders (Ship);

   procedure Move_Ship
     (Ship       : Ship_Class;
      Turn_Delta : Unit_Real)
     with Pre => Ship.Destination.Has_Element;

   procedure Move_Fleet
     (Fleet      : Fleet_Class;
      Turn_Delta : Unit_Real)
     with Pre => Fleet.Destination.Has_Element;

   ----------------
   -- Move_Fleet --
   ----------------

   procedure Move_Fleet
     (Fleet       : Fleet_Class;
      Turn_Delta : Unit_Real)
   is
      Total_Distance : constant Non_Negative_Real :=
                         Athena.Stars.Distance
                           (Fleet.Star, Fleet.Destination);
      Remaining      : constant Non_Negative_Real :=
                         Total_Distance * (1.0 - Fleet.Progress);
      Max_Distance   : constant Non_Negative_Real :=
                         Maximum_Speed (Fleet) * Turn_Delta;
      Travelled      : constant Non_Negative_Real :=
                         Real'Min (Remaining, Max_Distance);
   begin
      Log_Fleet
        (Fleet,
         "destination " & Fleet.Destination.Name
         & "; distance " & Image (Total_Distance)
         & "; travelled " & Image (Total_Distance - Remaining)
         & "; remaining " & Image (Remaining)
         & "; max " & Image (Max_Distance));

      for Ship of Minerva.Ship.Select_By_Fleet (Fleet) loop
         Update_Experience (Ship, Travelled / 1.0e4);
      end loop;

      if Remaining <= Max_Distance then
         On_Arrival (Fleet);
      else
         Fleet.Update_Fleet
           .Set_Progress ((Total_Distance - Remaining + Max_Distance)
                          / Total_Distance)
           .Done;
      end if;
   end Move_Fleet;

   ---------------
   -- Move_Ship --
   ---------------

   procedure Move_Ship
     (Ship       : Ship_Class;
      Turn_Delta : Unit_Real)
   is
      Total_Distance : constant Non_Negative_Real :=
                         Athena.Stars.Distance
                           (Ship.Star, Ship.Destination);
      Remaining      : constant Non_Negative_Real :=
                         Total_Distance * (1.0 - Ship.Progress);
      Max_Distance   : constant Non_Negative_Real :=
                         Maximum_Speed (Ship) * Turn_Delta;
      Travelled      : constant Non_Negative_Real :=
                         Real'Min (Remaining, Max_Distance);
   begin
      Log_Ship (Ship,
                "destination " & Ship.Destination.Name
                & "; distance " & Image (Total_Distance)
                & "; travelled " & Image (Total_Distance - Remaining)
                & "; remaining " & Image (Remaining)
                & "; max " & Image (Max_Distance));

      Update_Experience (Ship, Travelled / 1.0e4);

      if Remaining <= Max_Distance then
         On_Arrival (Ship);
      else
         Ship.Update_Ship
           .Set_Progress ((Total_Distance - Remaining + Max_Distance)
                          / Total_Distance)
           .Done;
      end if;
   end Move_Ship;

   ----------------
   -- Next_Order --
   ----------------

   procedure Next_Order
     (Ship       : Ship_Class;
      Turn_Delta : Unit_Real)
   is
      pragma Unreferenced (Turn_Delta);
      use all type Minerva.Db.Ship_Action;
      Order : constant Minerva.Ship_Order.Ship_Order_Class :=
                Minerva.Ship_Order.Get_By_Ship_Order
                  (Ship, Ship.First_Order);
   begin
      case Order.Action is
         when Load =>
            Log_Ship (Ship,
                      "loading " & Image (Order.Quantity)
                      & " " & Order.Cargo'Image);
            if Has_Star_Location (Ship) then
               declare
                  Colony : constant Athena.Colonies.Colony_Class :=
                             Athena.Colonies.Get_Colony (Ship.Star);
               begin
                  if Colony.Has_Element then
                     declare
                        Quantity : Non_Negative_Real := Order.Quantity;
                     begin
                        Athena.Colonies.Remove_Cargo
                          (Colony, Order.Cargo, Quantity);
                        Athena.Ships.Add_Cargo
                          (Ship, Order.Cargo, Quantity);
                     end;
                  else
                     Log_Ship (Ship, "no colony from which to load");
                  end if;
               end;
            else
               Log_Ship (Ship, "cannot load in deep space");
            end if;

            Athena.Ships.Next_Order (Ship);

         when Unload =>
            Log_Ship (Ship,
                      "unloading " & Image (Order.Quantity)
                      & " " & Order.Cargo'Image);

            if Has_Star_Location (Ship) then
               declare
                  use all type Minerva.Db.Cargo_Type;
                  Colony : constant Athena.Colonies.Colony_Class :=
                             Athena.Colonies.Get_Colony (Ship.Star);
               begin
                  if Colony.Has_Element then
                     declare
                        Quantity : Non_Negative_Real :=
                                     Real'Min (Order.Quantity,
                                               Current_Cargo
                                                 (Ship, Order.Cargo));
                     begin
                        Athena.Colonies.Add_Cargo
                          (Colony, Order.Cargo, Quantity);
                        Athena.Ships.Remove_Cargo
                          (Ship, Order.Cargo, Quantity);
                     end;
                  elsif Order.Cargo = Colonists then
                     declare
                        Quantity : Non_Negative_Real :=
                                     Real'Min (Order.Quantity,
                                               Current_Cargo
                                                 (Ship, Order.Cargo));
                     begin
                        Athena.Colonies.New_Colony
                          (Ship.Star, Ship.Empire, Quantity);
                        Athena.Ships.Remove_Cargo
                          (Ship, Order.Cargo, Quantity);
                     end;
                  else
                     Log_Ship (Ship, "no colony from which to load");
                  end if;
               end;
            else
               Log_Ship (Ship, "cannot load in deep space");
            end if;

            Athena.Ships.Next_Order (Ship);

         when Move =>
            Log_Ship (Ship,
                      "moving to " & Order.Star.Name);
            Ship.Update_Ship
              .Set_Destination (Order.Star)
              .Set_Progress (0.0)
              .Set_First_Order (Ship.First_Order + 1)
              .Done;

      end case;
   end Next_Order;

   -----------------------
   -- Update_Experience --
   -----------------------

   procedure Update_Experience
     (Ship : Ship_Class;
      XP   : Non_Negative_Real)
   is
   begin
      Ship.Update_Ship
        .Set_Experience (Ship.Experience + XP)
        .Done;
   end Update_Experience;

   -------------------
   -- Update_Fleets --
   -------------------

   procedure Update_Fleets
     (Turn_Delta : Unit_Real)
   is
   begin
      for Fleet of Minerva.Fleet.Scan_By_Identifier loop
         if Has_Destination (Fleet) then
            Move_Fleet (Fleet, Turn_Delta);
         end if;
      end loop;
   end Update_Fleets;

   ------------------
   -- Update_Ships --
   ------------------

   procedure Update_Ships (Turn_Delta : Unit_Real) is
   begin
      for Ship of Minerva.Ship.Select_By_Alive (True) loop
         if not Ship.Fleet.Has_Element
           and then not Has_Destination (Ship)
           and then Has_Orders (Ship)
         then
            Next_Order (Ship, Turn_Delta);
         end if;
         if Has_Destination (Ship) then
            Move_Ship (Ship, Turn_Delta);
         end if;
      end loop;
   end Update_Ships;

end Athena.Ships.Updates;
