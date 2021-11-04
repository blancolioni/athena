with Ada.Containers.Doubly_Linked_Lists;

with Athena.Empires;
with Athena.Ships;
with Athena.Stars;
with Athena.Technology;
with Athena.Treaties;
with Athena.Turns;

with Athena.Ships.Orders;
with Athena.Knowledge.Stars;

with Athena.Identifiers;
with Athena.Logging;

with Minerva.Colony;
with Minerva.Fleet;
with Minerva.Ship;
with Minerva.Ship_Build_Order;
with Minerva.Ship_Design;
with Minerva.Star;

package body Athena.Managers.Attack is

   type Attack_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Attack_Manager)
      return String
   is ("attack");

   overriding procedure Create_Orders
     (Manager : Attack_Manager);

   procedure Evaluate_Threats
     (Manager     : Attack_Manager'Class);

   function Find_Available_Fleet
     (Manager     : Attack_Manager'Class;
      Origin      : Minerva.Star.Star_Class;
      Destination : Minerva.Star.Star_Class)
      return Minerva.Fleet.Fleet_Class;

   function Sufficient_Attack_Force
     (Knowledge : Athena.Knowledge.Stars.Star_Knowledge'Class;
      Fleet     : Minerva.Fleet.Fleet_Class;
      Target    : Minerva.Star.Star_Class)
      return Boolean;

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Ship.Ship_Handle, Minerva.Ship."=");

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Attack_Manager)
   is

      Total_Industry : Non_Negative_Real := 0.0;

      procedure Add_Industry
        (Colony : Minerva.Colony.Colony_Class);

      procedure Check_Ships
        (Design            : Minerva.Ship_Design.Ship_Design_Class;
         Industry_Per_Ship : Positive);

      ------------------
      -- Add_Industry --
      ------------------

      procedure Add_Industry
        (Colony : Minerva.Colony.Colony_Class)
      is
      begin
         Total_Industry := Total_Industry + Colony.Industry;
      end Add_Industry;

      -----------------
      -- Check_Ships --
      -----------------

      procedure Check_Ships
        (Design            : Minerva.Ship_Design.Ship_Design_Class;
         Industry_Per_Ship : Positive)
      is
         Required  : constant Natural :=
                       Natural
                         (Real'Truncation
                            (Total_Industry / Real (Industry_Per_Ship)));
         Available : Natural := 0;
      begin

         for Ship of Minerva.Ship.Select_By_Empire_Manager
           (Manager.Manager)
         loop
            if Ship.Ship_Design.Identifier = Design.Identifier then
               Available := Available + 1;
            end if;
         end loop;

         if Available < Required then
            Manager.Log
              ("design: " & Design.Name
               & ": required" & Required'Image
               & "; available" & Available'Image);

            Minerva.Ship_Build_Order.Create
              (Turn        => Athena.Turns.Current_Turn,
               Empire      => Manager.Empire,
               Priority    => Manager.Priority,
               Ship_Design => Design,
               Manager     => Manager.Manager,
               Fleet       => Minerva.Fleet.Empty_Handle,
               Send_To     => Athena.Empires.Capital (Manager.Empire),
               Count       => Required - Available);
         end if;
      end Check_Ships;

   begin
      for Colony of
        Minerva.Colony.Select_By_Empire (Manager.Empire)
      loop
         Add_Industry (Colony);
      end loop;

      Check_Ships
        (Athena.Empires.Standard_Destroyer_Design (Manager.Empire), 500);

      Check_Ships
        (Athena.Empires.Standard_Cruiser_Design (Manager.Empire), 2000);

      Check_Ships
        (Athena.Empires.Standard_Battleship_Design (Manager.Empire), 5000);

      Check_Ships
        (Athena.Empires.Standard_Carrier_Design (Manager.Empire), 10_000);

      Evaluate_Threats (Manager);

   end Create_Orders;

   ----------------------------
   -- Default_Attack_Manager --
   ----------------------------

   function Default_Attack_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Attack_Manager'
        (Name     => +"attack",
         Empire   => <>,
         Manager  => <>,
         Priority => 1050);
   end Default_Attack_Manager;

   ----------------------
   -- Evaluate_Threats --
   ----------------------

   procedure Evaluate_Threats
     (Manager     : Attack_Manager'Class)
   is
      Max_Range   : constant Non_Negative_Real :=
                      Athena.Empires.Current_Tec_Level
                        (Manager.Empire, Athena.Technology.Drive)
                        * 5.0;
      Knowledge   : Athena.Knowledge.Stars.Star_Knowledge;

      Recon_Design    : constant Minerva.Ship_Design.Ship_Design_Class :=
                          Athena.Empires.Standard_Recon_Design
                            (Manager.Empire);

      Monitored_Count : Natural := 0;
      Required_Ships  : Natural := 0;
      Recon_Ships     : Ship_Lists.List;

      procedure Check_Threat
        (Threat  : Minerva.Empire.Empire_Class;
         Star    : Minerva.Star.Star_Class;
         Nearest : Minerva.Colony.Colony_Class;
         Stop    : out Boolean);

      procedure Monitor_Threat
        (Threat  : Minerva.Empire.Empire_Class;
         Star    : Minerva.Star.Star_Class;
         Nearest : Minerva.Colony.Colony_Class;
         Stop    : out Boolean);

      ------------------
      -- Check_Threat --
      ------------------

      procedure Check_Threat
        (Threat  : Minerva.Empire.Empire_Class;
         Star    : Minerva.Star.Star_Class;
         Nearest : Minerva.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         use Athena.Ships;

         Fleet          : constant Minerva.Fleet.Fleet_Class :=
                            Find_Available_Fleet
                              (Manager, Nearest.Star, Star);
         Can_Launch     : Boolean := True;
      begin

         if Has_Destination (Fleet, Star) then
            Stop := False;
            return;
         end if;

         Manager.Log
           ("checking threat from "
            & Threat.Name & " at " & Star.Name
            & ": nearest colony on "
              & Nearest.Star.Name
            & "; distance "
            & Image (Athena.Stars.Distance (Star, Nearest.Star)));

         if not Athena.Ships.Has_Star_Location (Fleet, Nearest.Star)
           and then not Athena.Ships.Has_Destination (Fleet)
         then
            Athena.Ships.Orders.Move_To (Fleet, Nearest.Star);
            Can_Launch := False;
         end if;

         for Ship of
           Minerva.Ship.Select_By_Empire_Manager (Manager.Manager)
         loop
            if Ship.Ship_Design.Identifier /= Recon_Design.Identifier then
               if not Ship.Fleet.Has_Element
                 and then not Has_Destination (Ship)
                 and then not Has_Orders (Ship)
               then
                  if Ship.Star.Identifier /= Nearest.Star.Identifier then
                     Athena.Ships.Orders.Move_To (Ship, Nearest.Star);
                  elsif Has_Star_Location (Fleet, Nearest.Star) then
                     declare
                        Update : constant Minerva.Ship.Ship_Update_Handle'Class
                          := Minerva.Ship.Update_Ship (Ship);
                     begin
                        Minerva.Ship.Done
                          (Update =>
                             Minerva.Ship.Set_Fleet (Update, Fleet));
                     end;

                     --  Ship.Update_Ship
                     --    .Set_Fleet (Fleet)
                     --    .Done;
                  end if;
               elsif Ship.Destination.Has_Element
                 and then Ship.Destination.Identifier = Nearest.Star.Identifier
               then
                  null;
                  --  Can_Launch := False;
               end if;
            end if;
         end loop;

         if Can_Launch
           and then Sufficient_Attack_Force (Knowledge, Fleet, Star)
         then
            if not Athena.Treaties.At_War (Manager.Empire, Star.Owner) then
               Athena.Treaties.Declare_War (Manager.Empire, Star.Owner);
            end if;
            Athena.Ships.Log_Fleet
              (Fleet, "ordered to attack " & Star.Name);
            Athena.Ships.Orders.Move_To (Fleet, Star);
         end if;

         Stop := True;

      end Check_Threat;

      --------------------
      -- Monitor_Threat --
      --------------------

      procedure Monitor_Threat
        (Threat  : Minerva.Empire.Empire_Class;
         Star    : Minerva.Star.Star_Class;
         Nearest : Minerva.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         pragma Unreferenced (Nearest);
         Min_Distance : Non_Negative_Real := Non_Negative_Real'Last;
         Assigned     : Minerva.Ship.Ship_Handle :=
                          Minerva.Ship.Empty_Handle;
      begin
         Stop := False;

         Monitored_Count := Monitored_Count + 1;
         if Knowledge.Turns_Since_Last_Visit (Star) <= Monitored_Count then
            return;
         end if;

         for Ship of Recon_Ships loop
            if Athena.Ships.Is_Idle (Ship) then
               declare
                  D : constant Non_Negative_Real :=
                        Athena.Stars.Distance (Ship.Star, Star);
               begin
                  if D < Min_Distance then
                     Min_Distance := D;
                     Assigned := Minerva.Ship.Get (Ship.Reference_Ship);
                  end if;
               end;
            end if;
         end loop;

         if Assigned.Has_Element then
            Manager.Log
              ("ordering " & Assigned.Name
               & " to observe "
               & Threat.Name & " colony on " & Star.Name);

            Athena.Ships.Orders.Move_To
              (Assigned, Star);
         else
            Required_Ships := Required_Ships + 1;
         end if;

      end Monitor_Threat;

   begin
      Knowledge.Load (Manager.Empire);

      for Recon_Ship of
        Minerva.Ship.Select_By_Empire_Manager (Manager.Manager)
      loop
         if Recon_Ship.Ship_Design.Identifier
           = Recon_Design.Identifier
         then
            Recon_Ships.Append (Recon_Ship.To_Ship_Handle);
         end if;
      end loop;

      Knowledge.Iterate_Threats
        (Max_Range, Monitor_Threat'Access);
      Knowledge.Iterate_Threats
        (Max_Range, Check_Threat'Access);

      if Required_Ships > 0 then
         Minerva.Ship_Build_Order.Create
           (Turn        => Athena.Turns.Current_Turn,
            Empire      => Manager.Empire,
            Priority    => Manager.Priority,
            Ship_Design => Recon_Design,
            Manager     => Manager.Manager,
            Count       => Required_Ships);
      end if;

   end Evaluate_Threats;

   --------------------------
   -- Find_Available_Fleet --
   --------------------------

   function Find_Available_Fleet
     (Manager     : Attack_Manager'Class;
      Origin      : Minerva.Star.Star_Class;
      Destination : Minerva.Star.Star_Class)
      return Minerva.Fleet.Fleet_Class
   is
      use Athena.Ships;
   begin
      for Fleet of
        Minerva.Fleet.Select_By_Empire (Manager.Empire)
      loop
         if Has_Destination (Fleet, Origin) then
            return Fleet;
         elsif not Has_Destination (Fleet)
           and then Has_Star_Location (Fleet, Origin)
         then
            return Fleet;
         elsif Has_Destination (Fleet, Destination) then
            return Fleet;
         end if;
      end loop;

      declare
         Closest_D : Non_Negative_Real := Non_Negative_Real'Last;
         Closest_F : Minerva.Fleet.Fleet_Handle :=
                       Minerva.Fleet.Empty_Handle;
      begin
         for Fleet of
           Minerva.Fleet.Select_By_Empire (Manager.Empire)
         loop
            if not Has_Destination (Fleet) then
               declare
                  D : constant Non_Negative_Real :=
                        Athena.Stars.Distance
                          (Fleet.Star, Origin);
               begin
                  if D < Closest_D then
                     Closest_D := D;
                     Closest_F := Fleet.To_Fleet_Handle;
                  end if;
               end;
            end if;
         end loop;

         if Closest_F.Has_Element then
            return Closest_F;
         end if;
      end;

      Manager.Log
        ("creating new fleet for attack from "
         & Origin.Name & " to " & Destination.Name);

      return Minerva.Fleet.Create
        (Identifier  => Athena.Identifiers.Next_Identifier,
         Star        => Origin,
         Name        =>
           Athena.Ships.New_Fleet_Name (Manager.Empire, "Task Force"),
         Empire      => Manager.Empire,
         Manager     => Manager.Manager);

   end Find_Available_Fleet;

   -----------------------------
   -- Sufficient_Attack_Force --
   -----------------------------

   function Sufficient_Attack_Force
     (Knowledge : Athena.Knowledge.Stars.Star_Knowledge'Class;
      Fleet     : Minerva.Fleet.Fleet_Class;
      Target    : Minerva.Star.Star_Class)
      return Boolean
   is
      Fleet_Ships   : Ship_Lists.List;
      Opposition    : constant Athena.Knowledge.Stars.Known_Ship_Lists.List :=
                        Knowledge.Get_Known_Ships (Target);
      Their_Weapons : Non_Negative_Real := 0.0;
      Our_Weapons   : Non_Negative_Real := 0.0;
   begin
      for Ship of Opposition loop
         Their_Weapons := Their_Weapons + Ship.Weapon_Mass;
      end loop;

      for Ship of Minerva.Ship.Select_By_Fleet (Fleet) loop
         Fleet_Ships.Append (Ship.To_Ship_Handle);
      end loop;

      for Ship of Fleet_Ships loop
         Our_Weapons := Our_Weapons + Athena.Ships.Weapon_Mass (Ship);
      end loop;

      Athena.Logging.Log
        (Fleet.Empire.Name & ": checking forces for attack on "
         & Target.Name & " owned by " & Target.Owner.Name
         & ": we have" & Fleet_Ships.Length'Image
         & " ships with weapon mass "
         & Image (Our_Weapons)
         & "; they have " & Opposition.Length'Image
         & " ships with weapon mass "
         & Image (Their_Weapons));
      return Our_Weapons > Their_Weapons * 1.5;

   end Sufficient_Attack_Force;

end Athena.Managers.Attack;
