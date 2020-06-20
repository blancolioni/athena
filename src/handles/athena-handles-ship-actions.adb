with Athena.Calendar;
with Athena.Trigonometry;

with Athena.Solar_System;

with Athena.Handles.Colony;

with Athena.Colonies;
with Athena.Ships;
with Athena.Stars;

package body Athena.Handles.Ship.Actions is

   type System_Departure_Action is
     new Root_Ship_Action with
      record
         Destination : Athena.Handles.Star.Star_Handle;
         Rho         : Non_Negative_Real;
         Theta       : Athena.Trigonometry.Angle;
      end record;

   overriding function Image (Action : System_Departure_Action) return String
   is ("departing");

   overriding function Start
     (Action : System_Departure_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : System_Departure_Action;
      Ship   : Ship_Handle'Class)
   is null;

   type System_Arrival_Action is
     new Root_Ship_Action with
      record
         World : World_Reference;
      end record;

   overriding function Image (Action : System_Arrival_Action) return String
   is ("arriving");

   overriding function Start
     (Action : System_Arrival_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : System_Arrival_Action;
      Ship   : Ship_Handle'Class);

   type Jump_To_Action is
     new Root_Ship_Action with
      record
         Destination : Athena.Handles.Star.Star_Handle;
      end record;

   overriding function Image (Action : Jump_To_Action) return String
   is ("jumping to " & Action.Destination.Name);

   overriding function Start
     (Action : Jump_To_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : Jump_To_Action;
      Ship   : Ship_Handle'Class);

   type Load_Cargo_Action is
     new Root_Ship_Action with
      record
         Cargo    : Athena.Cargo.Cargo_Container;
      end record;

   overriding function Image (Action : Load_Cargo_Action) return String
   is ("loading " & Action.Cargo.Content_Summary);

   overriding function Start
     (Action : Load_Cargo_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : Load_Cargo_Action;
      Ship   : Ship_Handle'Class)
   is null;

   type Unload_Cargo_Action is
     new Root_Ship_Action with
      record
         Cargo    : Athena.Cargo.Cargo_Container;
      end record;

   overriding function Image (Action : Unload_Cargo_Action) return String
   is ("unloading " & Action.Cargo.Content_Summary);

   overriding function Start
     (Action : Unload_Cargo_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : Unload_Cargo_Action;
      Ship   : Ship_Handle'Class)
   is null;

   ----------------
   -- Load_Cargo --
   ----------------

   function Load_Cargo
     (Cargo    : Athena.Cargo.Cargo_Container)
      return Root_Ship_Action'Class
   is
   begin
      return Load_Cargo_Action'
        (Complete => False,
         Cargo    => Cargo);
   end Load_Cargo;

   -------------
   -- Move_To --
   -------------

   procedure Move_To_Star
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
   is
      use Athena.Trigonometry;
      Rho : constant Non_Negative_Real :=
              Athena.Solar_System.Earth_Orbit
                * Ship.Location_Star.Mass / Athena.Solar_System.Solar_Mass
              * 10.0;
      Theta : constant Angle :=
                Arctan (Star.Y - Ship.Location_Star.Y,
                        Star.X - Ship.Location_Star.X);
   begin

      Athena.Ships.Add_Refuel_Action (Ship);

      Ship.Add_Action
        (Action => System_Departure_Action'
           (Complete    => False,
            Destination => Star,
            Rho         => Rho,
            Theta       => Theta));

      Ship.Add_Action
        (Action => Jump_To_Action'
           (Complete => False,
            Destination => Star));

      --  Ship.Add_Action
      --    (Action => System_Arrival_Action'(Complete => False));

   end Move_To_Star;

   -------------------
   -- Move_To_World --
   -------------------

   procedure Move_To_World
     (Ship  : Ship_Handle;
      World : Athena.Handles.World.World_Handle)
   is
   begin

      Move_To_Star (Ship, World.Star);

      Ship.Add_Action
        (Action => System_Arrival_Action'
           (Complete => False,
            World    => World.Reference));

   end Move_To_World;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished
     (Action : System_Arrival_Action;
      Ship   : Ship_Handle'Class)
   is
   begin
      Ship.Clear_Destination;
      Ship.Set_World_Location
        (Athena.Handles.World.Get (Action.World));
      Set_Activity (Ship, Idle);
      Athena.Ships.On_Arrival (Ship);
      Ship.Owner.Send_Signal (Attack_Manager);
   end On_Finished;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished
     (Action : Jump_To_Action;
      Ship   : Ship_Handle'Class)
   is
   begin
      Action.Destination.Add_Ship (Ship.Reference);
      Ship.Owner.Knowledge.Visit (Action.Destination);
   end On_Finished;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : Load_Cargo_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is
      use type Athena.Movers.Mover_Location_Type;
   begin

      Ship.Log
        ("loading " & Action.Cargo.Content_Summary);

      if Ship.Location /= Athena.Movers.World_Orbit
        or else not Ship.Location_World.Has_Owner
        or else Ship.Owner.Reference /= Ship.Location_World.Owner
      then
         --  loading impossible
         return 0.0;
      else
         declare
            Colony       : constant Athena.Handles.Colony.Colony_Handle :=
                             Athena.Handles.Colony.Get
                               (Ship.Location_World.Colony);
            Elapsed      : Duration := 0.0;

            procedure Process_Cargo
              (Item     : Athena.Cargo.Cargo_Interface'Class;
               Quantity : Non_Negative_Real);

            -------------------
            -- Process_Cargo --
            -------------------

            procedure Process_Cargo
              (Item     : Athena.Cargo.Cargo_Interface'Class;
               Quantity : Non_Negative_Real)
            is
            begin
               Athena.Colonies.Load_Cargo_From_Colony
                 (From => Colony,
                  To   => Ship,
                  Cargo => Item,
                  Max   => Quantity);
               Elapsed := Duration'Max (Elapsed, Duration (Quantity * 3600.0));
            end Process_Cargo;

         begin

            Ship.Log ("loading: " & Action.Cargo.Content_Summary);

            Action.Cargo.Iterate (Process_Cargo'Access);

            return Elapsed;

         end;
      end if;

   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : System_Departure_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is
   begin
      if Ship.At_Star (Action.Destination) then
         return 0.0;
      end if;

      if not Ship.Travelling_To (Action.Destination) then
         Ship.Log
           ("departure: set destination to "
            & Action.Destination.Name);
         Ship.Set_Destination (Action.Destination);
      end if;

      Ship.Set_Activity (Departing);
      return Athena.Calendar.Days
        (10.0 / Athena.Ships.Get_Impulse_Speed (Ship));
   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : Jump_To_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is
   begin
      if Ship.At_Star (Action.Destination) then
         return 0.0;
      end if;

      if not Ship.Has_Destination
        or else not Ship.Travelling_To (Action.Destination)
      then
         return 0.0;
      end if;

      declare
         Total_Distance : constant Non_Negative_Real :=
                            Athena.Stars.Distance
                              (Ship.Origin_Star, Ship.Destination_Star);
         Speed          : constant Non_Negative_Real :=
                            Athena.Ships.Get_Jump_Speed (Ship);
         Journey_Time   : constant Duration :=
                            Athena.Calendar.Days (Total_Distance / Speed);
         XP             : constant Non_Negative_Real :=
                            Total_Distance / 1000.0;
      begin

         Log (Ship,
              "moving to "
              & Ship.Destination_Star.Name
              & ": distance " & Image (Total_Distance)
              & "; jump speed " & Image (Speed)
              & "; travel time " & Image (Total_Distance / Speed)
              & " days");

         Ship.Location_Star.Remove_Ship (Ship.Reference);
         Ship.Add_Experience (XP);
         Ship.Set_Activity (Jumping);
         return Journey_Time;
      end;
   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : Unload_Cargo_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is

      Colony : Athena.Handles.Colony.Colony_Handle :=
                 Athena.Handles.Colony.Get
                   (Ship.Location_World.Colony);

      Elapsed : Duration := 0.0;

      procedure Process_Cargo
        (Item     : Athena.Cargo.Cargo_Interface'Class;
         Quantity : Non_Negative_Real);

      -------------------
      -- Process_Cargo --
      -------------------

      procedure Process_Cargo
        (Item     : Athena.Cargo.Cargo_Interface'Class;
         Quantity : Non_Negative_Real)
      is
      begin
         Athena.Colonies.Unload_Cargo_To_Colony
           (To    => Colony,
            From  => Ship,
            Cargo => Item,
            Max   => Real'Min (Quantity, Ship.Current_Quantity (Item)));
         Elapsed := Duration'Max (Elapsed, Duration (Quantity * 3600.0));
      end Process_Cargo;

   begin

      Ship.Log ("unloading: " & Action.Cargo.Content_Summary);

      if not Colony.Has_Element then
         if Action.Cargo.Tonnage (Athena.Cargo.People) = 0.0 then
            Ship.Log ("unable to unload because there is no colony");
            return 0.0;
         end if;

         Ship.Log ("creating a new colony");
         Colony :=
           Athena.Colonies.New_Colony
             (World    => Ship.Location_World,
              Owner    => Ship.Owner,
              Pop      => 0.0,
              Industry => 0.0,
              Material => 0.0);
      end if;

      Action.Cargo.Iterate (Process_Cargo'Access);

      return Elapsed;

   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : System_Arrival_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is
   begin
      Ship.Set_Activity (Arriving);
      return Athena.Calendar.Days
        (10.0 / Athena.Ships.Get_Impulse_Speed (Ship));
   end Start;

   ------------------
   -- Unload_Cargo --
   ------------------

   function Unload_Cargo
     (Cargo    : Athena.Cargo.Cargo_Container)
      return Root_Ship_Action'Class
   is
   begin
      return Unload_Cargo_Action'
        (Complete   => False,
         Cargo      => Cargo);
   end Unload_Cargo;

end Athena.Handles.Ship.Actions;
