with Athena.Calendar;

with Athena.Handles.Colony;

with Athena.Colonies;
with Athena.Ships;
with Athena.Stars;

package body Athena.Handles.Ship.Actions is

   type Move_To_Action is
     new Root_Ship_Action with
      record
         Destination : Athena.Handles.Star.Star_Handle;
      end record;

   overriding function Start
     (Action : Move_To_Action;
      Ship   : Ship_Handle'Class)
      return Duration;

   overriding procedure On_Finished
     (Action : Move_To_Action;
      Ship   : Ship_Handle'Class);

   type Load_Cargo_Action is
     new Root_Ship_Action with
      record
         Cargo : Cargo_Class;
         Quantity : Non_Negative_Real;
      end record;

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
         Cargo    : Cargo_Class;
         Quantity : Non_Negative_Real;
      end record;

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
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class
   is
   begin
      return Load_Cargo_Action'
        (Complete => False,
         Cargo    => Cargo,
         Quantity => Quantity);
   end Load_Cargo;

   -------------
   -- Move_To --
   -------------

   function Move_To
     (Star : Athena.Handles.Star.Star_Handle)
      return Root_Ship_Action'Class
   is
   begin
      return Move_To_Action'
        (Complete    => False,
         Destination => Star);
   end Move_To;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished
     (Action : Move_To_Action;
      Ship   : Ship_Handle'Class)
   is
   begin
      Ship.Set_Star_Location (Ship.Destination);
      Ship.Clear_Destination;
      Athena.Ships.On_Arrival (Ship);
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
                                 Athena.Ships.Available_Cargo_Space (Ship));
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
     (Action : Move_To_Action;
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

         --  if Speed >= Remaining then
         --     Log (Ship, "arrives at " & Ship.Destination.Name);
         --     return True;
         --
         --  else
         --
         --     Ship.Set_Progress ((Travelled + Speed) / Total_Distance);
         --     return False;
         --  end if;
         --
      --  if Move_Ship (Ship) then
      --     Ship.Set_Star_Location (Ship.Destination);
      --     Ship.Clear_Destination;
      --     Athena.Ships.On_Arrival (Ship);
      --  end if;

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
                              Colony.Population + Unloaded_Quantity * 10.0;
               begin
                  Colony.Log ("add pop to existing colony");
                  Athena.Ships.Unload_Cargo
                    (Ship, Colonists, Unloaded_Quantity);
                  Colony.Set_Population (New_Pop);
               end;
               return Duration (Unloaded_Quantity * 600.0);
            end if;

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
        (Complete => False,
         Cargo    => Cargo,
         Quantity => Quantity);
   end Unload_Cargo;

end Athena.Handles.Ship.Actions;
