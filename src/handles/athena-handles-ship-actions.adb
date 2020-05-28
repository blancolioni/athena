with Athena.Calendar;

with Athena.Handles.Colony;

with Athena.Colonies;
with Athena.Ships;
with Athena.Stars;

package body Athena.Handles.Ship.Actions is

   type System_Departure_Action is
     new Root_Ship_Action with
      record
         Destination : Athena.Handles.Star.Star_Handle;
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
         null;
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
      Ship   : Ship_Handle'Class)
   is null;

   type Load_Cargo_Action is
     new Root_Ship_Action with
      record
         Cargo    : Cargo_Class;
         Quantity : Non_Negative_Real;
         Maximum  : Boolean;
      end record;

   overriding function Image (Action : Load_Cargo_Action) return String
   is ("loading " & Action.Cargo'Image);

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
         Cargo      : Cargo_Class;
         Quantity   : Non_Negative_Real;
         Everything : Boolean;
      end record;

   overriding function Image (Action : Unload_Cargo_Action) return String
   is ("unloading " & Action.Cargo'Image);

   overriding function Start
     (Action : Unload_Cargo_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : Unload_Cargo_Action;
      Ship   : Ship_Handle'Class)
   is null;

   -----------------
   -- Empty_Cargo --
   -----------------

   function Empty_Cargo
     (Cargo    : Cargo_Class)
      return Root_Ship_Action'Class
   is
   begin
      return Unload_Cargo_Action'
        (Complete   => False,
         Cargo      => Cargo,
         Quantity   => 0.0,
         Everything => True);
   end Empty_Cargo;

   ----------------
   -- Fill_Cargo --
   ----------------

   function Fill_Cargo
     (Cargo    : Cargo_Class)
      return Root_Ship_Action'Class
   is
   begin
      return Load_Cargo_Action'
        (Complete => False,
         Cargo    => Cargo,
         Quantity => 0.0,
         Maximum  => True);
   end Fill_Cargo;

   ----------------
   -- Load_Cargo --
   ----------------

   function Load_Cargo
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class
   is
   begin
      return Load_Cargo_Action'
        (Complete => False,
         Cargo    => Cargo,
         Quantity => Quantity,
         Maximum  => False);
   end Load_Cargo;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
   is
   begin

      Ship.Add_Action
        (Action => Load_Cargo_Action'
           (Complete => False,
            Cargo    => Fuel,
            Quantity => 0.0,
            Maximum  => True));

      Ship.Add_Action
        (Action => System_Departure_Action'
           (Complete    => False,
            Destination => Star));

      Ship.Add_Action
        (Action => Jump_To_Action'
           (Complete => False,
            Destination => Star));

      Ship.Add_Action
        (Action => System_Arrival_Action'(Complete => False));

   end Move_To;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished
     (Action : System_Arrival_Action;
      Ship   : Ship_Handle'Class)
   is
   begin
      Ship.Set_Star_Location (Ship.Destination);
      Ship.Clear_Destination;
      Athena.Ships.On_Arrival (Ship);
      Set_Activity (Ship, Idle);
   end On_Finished;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : Load_Cargo_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is
      Max_Quantity : constant Non_Negative_Real :=
                       Real'Min (Action.Quantity,
                                 Athena.Ships.Available_Cargo_Space
                                   (Ship, Action.Cargo));
   begin

      Ship.Log
        ("loading " & Image (Action.Quantity) & " " & Action.Cargo'Image);

      if not Ship.Has_Star_Location
        or else not Ship.Star_Location.Has_Owner
        or else (Ship.Star_Location.Has_Owner
                 and then Ship.Owner.Reference /= Ship.Star_Location.Owner)
      then
         --  loading impossible
         return 0.0;
      else
         declare
            Colony : constant Athena.Handles.Colony.Colony_Handle :=
                       Athena.Stars.Get_Colony (Ship.Star_Location);
         begin
            case Action.Cargo is
               when Colonists =>
                  declare
                     Required_Pop      : constant Non_Negative_Real :=
                                           Max_Quantity;
                     Available_Pop     : constant Non_Negative_Real :=
                                           Colony.Population;
                     Loaded_Pop        : constant Non_Negative_Real :=
                                           Real'Min (Required_Pop,
                                                     Available_Pop);
                     Remaining_Pop     : constant Non_Negative_Real :=
                                           Available_Pop - Loaded_Pop;
                  begin
                     Ship.Log ("loading " & Image (Loaded_Pop)
                               & " colonists");
                     Athena.Ships.Load_Cargo
                       (Ship, Colonists, Loaded_Pop);
                     Ship.Log ("contains "
                               & Image
                                 (Athena.Ships.Current_Cargo
                                    (Ship, Colonists)));

                     Colony.Set_Population (Remaining_Pop);
                     return Duration (Loaded_Pop * 600.0);
                  end;

               when Fuel =>
                  Athena.Ships.Load_Cargo
                    (Ship, Fuel, Max_Quantity);
                  return Duration (Max_Quantity);

               when Material =>

                  declare
                     Loaded    : constant Non_Negative_Real :=
                                   Real'Min (Colony.Material, Max_Quantity);
                     Remaining : constant Non_Negative_Real :=
                                   Colony.Material - Loaded;
                  begin
                     Athena.Ships.Load_Cargo
                       (Ship, Material, Loaded);
                     Colony.Set_Material (Remaining);
                     return Duration (Loaded * 60.0);
                  end;

               when Industry =>

                  declare
                     Loaded    : constant Non_Negative_Real :=
                                   Real'Min (Colony.Industry, Max_Quantity);
                     Remaining : constant Non_Negative_Real :=
                                   Colony.Industry - Loaded;
                  begin
                     Athena.Ships.Load_Cargo (Ship, Industry, Loaded);
                     Colony.Set_Industry (Remaining);
                     return Duration (Loaded * 600.0);
                  end;
            end case;
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
      use type Athena.Handles.Star.Star_Handle;
   begin
      if Ship.Has_Star_Location
        and then Ship.Star_Location = Action.Destination
      then
         return 0.0;
      end if;

      if not Ship.Has_Destination
        or else Ship.Destination /= Action.Destination
      then
         Ship.Set_Destination (Action.Destination);
      end if;

      Ship.Set_Activity (Departing);
      return Athena.Calendar.Days
        (100.0 / Athena.Ships.Get_Impulse_Speed (Ship));
   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Action : Jump_To_Action;
      Ship   : Ship_Handle'Class)
      return Duration
   is
      use type Athena.Handles.Star.Star_Handle;
   begin
      if Ship.Has_Star_Location
        and then Ship.Star_Location = Action.Destination
      then
         return 0.0;
      end if;

      if not Ship.Has_Destination
        or else Ship.Destination /= Action.Destination
      then
         return 0.0;
      end if;

      declare
         Total_Distance : constant Non_Negative_Real :=
                            Athena.Stars.Distance
                              (Ship.Origin, Ship.Destination);
         Speed          : constant Non_Negative_Real :=
                            Athena.Ships.Get_Jump_Speed (Ship);
         Journey_Time   : constant Duration :=
                            Athena.Calendar.Days (Total_Distance / Speed);
         XP             : constant Non_Negative_Real :=
                            Total_Distance / 1000.0;
      begin

         Log (Ship,
              "moving to "
              & Ship.Destination.Name
              & ": distance " & Image (Total_Distance)
              & "; jump speed " & Image (Speed)
              & "; travel time " & Image (Total_Distance / Speed)
              & " days");

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
      use type Athena.Handles.Empire.Empire_Handle;
      Cargo_Quantity    : constant Non_Negative_Real :=
                            Athena.Ships.Current_Cargo
                              (Ship, Action.Cargo);
      Unloaded_Quantity : constant Real :=
                            Real'Min (Action.Quantity, Cargo_Quantity);
      Colony            : constant Athena.Handles.Colony.Colony_Handle :=
                            Athena.Stars.Get_Colony (Ship.Star_Location);
   begin

      Ship.Log
        ("unloading "
         & Image (Unloaded_Quantity)
         & "/"
         & Image (Action.Quantity)
         & " " & Action.Cargo'Image);

      if Unloaded_Quantity = 0.0 then
         return 0.0;
      end if;

      case Action.Cargo is
         when Colonists =>
            if not Colony.Has_Element then
               --  create a new colony

               Ship.Log ("creating a new colony");
               declare
                  Colony : constant Athena.Handles.Colony.Colony_Handle :=
                             Athena.Colonies.New_Colony
                               (At_Star  => Ship.Star_Location,
                                Owner    => Ship.Owner,
                                Pop      => Unloaded_Quantity,
                                Industry => 0.0,
                                Material => 0.0);
               begin
                  pragma Unreferenced (Colony);
               end;

               Athena.Ships.Unload_Cargo (Ship, Colonists, Unloaded_Quantity);

               Ship.Owner.Knowledge.Clear_Colonizing (Ship.Star_Location);

               return Duration (Unloaded_Quantity * 6000.0);
            elsif Colony.Owner /= Ship.Owner then
               --  this means war!
               Colony.Log ("can't unload to foreign colony");
               return 0.0;
               --  but that's not implemented
            else
               declare
                  New_Pop : constant Non_Negative_Real :=
                              Colony.Population + Unloaded_Quantity;
               begin
                  Colony.Log ("add pop to existing colony");
                  Athena.Ships.Unload_Cargo
                    (Ship, Colonists, Unloaded_Quantity);
                  Colony.Set_Population (New_Pop);
               end;
               return Duration (Unloaded_Quantity * 600.0);
            end if;

         when Fuel =>
            return 0.0;

         when Material =>

            if not Colony.Has_Element then

               --  Canceled
               return 0.0;

               --  create a new colony
               --                 Athena.Handles.Colony.Create
               --                   (Star      => Ship.Star,
               --                    Empire    => Ship.Empire,
               --                    Construct => 0.0,
               --                    Pop       => 0.0,
               --                    Colonists => 0.0,
               --                    Industry  => 0.0,
               --                    Material  => Unloaded_Quantity);
               --
               --                 Ship.Star.Update_Star
               --                   .Set_Owner (Ship.Empire.Reference_Empire)
               --                   .Done;

            else
               declare
                  New_Quantity : constant Non_Negative_Real :=
                                   Colony.Material + Unloaded_Quantity;
               begin
                  Athena.Ships.Unload_Cargo
                    (Ship, Action.Cargo, Unloaded_Quantity);
                  Colony.Set_Material (New_Quantity);
               end;
            end if;

            return Duration (Unloaded_Quantity * 60.0);

         when Industry =>

            if not Colony.Has_Element then

               --  Canceled
               return 0.0;

               --  create a new colony
               --                 Athena.Handles.Colony.Create
               --                   (Star      => Ship.Star,
               --                    Empire    => Ship.Empire,
               --                    Construct => 0.0,
               --                    Pop       => 0.0,
               --                    Colonists => 0.0,
               --                    Industry  => Unloaded_Quantity,
               --                    Material  => 0.0);
               --
               --                 Ship.Star.Update_Star
               --                   .Set_Owner (Ship.Empire.Reference_Empire)
               --                   .Done;

            else
               declare
                  New_Quantity : constant Non_Negative_Real :=
                                   Colony.Industry + Unloaded_Quantity;
               begin
                  Athena.Ships.Unload_Cargo
                    (Ship, Action.Cargo, Unloaded_Quantity);
                  Colony.Set_Industry (New_Quantity);
                  return Duration (Unloaded_Quantity * 600.0);
               end;
            end if;

      end case;
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
        (100.0 / Athena.Ships.Get_Impulse_Speed (Ship));
   end Start;

   ------------------
   -- Unload_Cargo --
   ------------------

   function Unload_Cargo
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class
   is
   begin
      return Unload_Cargo_Action'
        (Complete   => False,
         Cargo      => Cargo,
         Quantity   => Quantity,
         Everything => False);
   end Unload_Cargo;

end Athena.Handles.Ship.Actions;
