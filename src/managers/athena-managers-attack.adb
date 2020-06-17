with Athena.Ships;
with Athena.Stars;
with Athena.Treaties;

with Athena.Ships.Lists;

--  with Athena.Logging;

with Athena.Handles.Colony;
with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.Knowledge;
with Athena.Handles.Ship.Actions;
with Athena.Handles.Star;

package body Athena.Managers.Attack is

   type Attack_Manager is
     new Root_Manager_Type with null record;

   overriding function Identifier
     (Manager : Attack_Manager)
      return String
   is ("attack");

   overriding procedure Dispatch_Create_Orders
     (Manager : in out Attack_Manager);

   procedure Evaluate_Threats
     (Manager    : in out Attack_Manager'Class);

   function Find_Available_Fleet
     (Manager    : in out Attack_Manager'Class;
      Origin      : Athena.Handles.Star.Star_Handle;
      Destination : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Fleet.Fleet_Handle;

   function Sufficient_Attack_Force
     (Knowledge : Athena.Handles.Knowledge.Knowledge_Handle;
      Fleet     : Athena.Handles.Fleet.Fleet_Handle;
      Target    : Athena.Handles.Star.Star_Handle)
      return Boolean;

   ----------------------------
   -- Default_Attack_Manager --
   ----------------------------

   function Default_Attack_Manager
     return Root_Manager_Type'Class
   is
   begin
      return Manager : constant Attack_Manager :=
        Attack_Manager'
          (Name     => +"attack",
           Empire   => <>,
           Priority => 1020,
           Next_Update => Athena.Calendar.Clock,
           Has_Next_Update => True,
           Messages => <>);
   end Default_Attack_Manager;

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Dispatch_Create_Orders
     (Manager : in out Attack_Manager)
   is
   begin
      Evaluate_Threats (Manager);
   end Dispatch_Create_Orders;

   ----------------------
   -- Evaluate_Threats --
   ----------------------

   procedure Evaluate_Threats
     (Manager    : in out Attack_Manager'Class)
   is
      Max_Range   : constant Non_Negative_Real := 10.0;
      Empire      : constant Athena.Handles.Empire.Empire_Handle :=
                      Athena.Handles.Empire.Get (Manager.Empire);
      Knowledge   : constant Athena.Handles.Knowledge.Knowledge_Handle :=
                      Empire.Knowledge;

      Monitored_Count : Natural := 0;
      Required_Ships  : Natural := 0;
      Recon_Ships     : Athena.Ships.Lists.List;

      Recon_Design : constant Athena.Handles.Design.Design_Handle :=
                       Athena.Handles.Design.Get
                         (Empire.Standard_Design
                            (Athena.Handles.Empire.Recon));

      procedure Check_Threat
        (Reference  : Athena.Handles.Empire_Reference;
         Star       : Athena.Handles.Star.Star_Handle;
         Nearest    : Athena.Handles.Colony_Reference;
         Stop       : out Boolean);

      procedure Monitor_Threat
        (Reference  : Athena.Handles.Empire_Reference;
         Star       : Athena.Handles.Star.Star_Handle;
         Nearest    : Athena.Handles.Colony_Reference;
         Stop       : out Boolean);

      ------------------
      -- Check_Threat --
      ------------------

      procedure Check_Threat
        (Reference  : Athena.Handles.Empire_Reference;
         Star       : Athena.Handles.Star.Star_Handle;
         Nearest    : Athena.Handles.Colony_Reference;
         Stop       : out Boolean)
      is
         use type Athena.Handles.Star.Star_Handle;
         Threat     : constant Athena.Handles.Empire.Empire_Handle :=
                        Athena.Handles.Empire.Get (Reference);
         Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                        Athena.Handles.Colony.Get (Nearest);
         Fleet      : constant Athena.Handles.Fleet.Fleet_Handle :=
                        Find_Available_Fleet
                          (Manager, Colony.Star, Star);
         Can_Launch : Boolean := True;
      begin

         if Fleet.Has_Destination
           and then Fleet.Destination = Star
         then
            Stop := False;
            return;
         end if;

         Manager.Log
           ("checking threat from "
            & Threat.Name & " at " & Star.Name
            & ": nearest colony on "
            & Colony.Star.Name
            & "; distance "
            & Image (Athena.Stars.Distance (Star, Colony.Star)));

         if Fleet.Location.Identifier /= Colony.Star.Identifier
           and then not Fleet.Has_Destination
         then
            Fleet.Log ("sending to " & Colony.Star.Short_Name);
            Fleet.Set_Destination (Colony.Star);
            Can_Launch := False;
         end if;

         declare

            procedure Add_Attack_Ship
              (Reference : Athena.Handles.Ship_Reference);

            ---------------------
            -- Add_Attack_Ship --
            ---------------------

            procedure Add_Attack_Ship
              (Reference : Athena.Handles.Ship_Reference)
            is
               Ship : constant Athena.Handles.Ship.Ship_Handle :=
                        Athena.Handles.Ship.Get (Reference);
            begin
               if not Athena.Ships.Is_Armed (Ship) then
                  return;
               end if;

               Ship.Log ("checking attack");
               if not Ship.Has_Fleet
                 and then Ship.Is_Idle
               then
                  if Ship.Star_Location /= Colony.Star then
                     Athena.Handles.Ship.Actions.Move_To
                       (Ship, Colony.Star);
                  elsif Fleet.Location = Colony.Star then
                     Fleet.Add_Ship (Ship.Reference);
                  end if;
               end if;
            end Add_Attack_Ship;

         begin
            Empire.Iterate_Managed_Ships
              (Manager => Athena.Handles.Attack_Manager,
               Process => Add_Attack_Ship'Access);
         end;

         if Can_Launch
           and then Sufficient_Attack_Force (Knowledge, Fleet, Star)
         then
            if not Athena.Treaties.At_War (Empire, Threat) then
               Athena.Treaties.Declare_War (Empire, Threat);
            end if;

            Fleet.Set_Destination (Star);

         --     Athena.Orders.Move_Fleet (Fleet, Star);
         end if;

         Stop := True;

      end Check_Threat;

      --------------------
      -- Monitor_Threat --
      --------------------

      procedure Monitor_Threat
        (Reference : Athena.Handles.Empire_Reference;
         Star      : Athena.Handles.Star.Star_Handle;
         Nearest   : Athena.Handles.Colony_Reference;
         Stop      : out Boolean)
      is
         pragma Unreferenced (Nearest);
         Threat       : constant Athena.Handles.Empire.Empire_Handle :=
                          Athena.Handles.Empire.Get (Reference);
         Min_Distance : Non_Negative_Real := Non_Negative_Real'Last;
         Assigned     : Athena.Handles.Ship.Ship_Handle :=
                          Athena.Handles.Ship.Empty_Handle;
      begin
         Stop := False;

         Monitored_Count := Monitored_Count + 1;
         if Knowledge.Turns_Since_Last_Visit (Star) <= Monitored_Count then
            return;
         end if;

         for Ship of Recon_Ships loop
            if Ship.Is_Idle then
               declare
                  D : constant Non_Negative_Real :=
                        Athena.Stars.Distance (Ship.Star_Location, Star);
               begin
                  if D < Min_Distance then
                     Min_Distance := D;
                     Assigned := Ship;
                  end if;
               end;
            end if;
         end loop;

         if Assigned.Has_Element then
            Manager.Log
              ("ordering " & Assigned.Name
               & " to observe "
               & Threat.Name & " colony on " & Star.Name);

            Athena.Handles.Ship.Actions.Move_To (Assigned, Star);
         else
            Required_Ships := Required_Ships + 1;
         end if;

      end Monitor_Threat;

   begin

      declare
         procedure Add_Recon_Ship (Reference : Athena.Handles.Ship_Reference);

         --------------------
         -- Add_Recon_Ship --
         --------------------

         procedure Add_Recon_Ship
           (Reference : Athena.Handles.Ship_Reference)
         is
            use type Athena.Handles.Design.Design_Handle;
            Ship : constant Athena.Handles.Ship.Ship_Handle :=
                     Athena.Handles.Ship.Get (Reference);
         begin
            if Ship.Design = Recon_Design then
               Recon_Ships.Append (Ship);
            end if;
         end Add_Recon_Ship;

      begin
         Empire.Iterate_Managed_Ships (Athena.Handles.Attack_Manager,
                                       Add_Recon_Ship'Access);
      end;

      Knowledge.Iterate_Threats
        (Max_Range, Monitor_Threat'Access);
      Knowledge.Iterate_Threats
        (Max_Range, Check_Threat'Access);

      if Required_Ships > 0 then
         Manager.Log ("need" & Required_Ships'Image
                      & " recon ships");
         --  Athena.Orders.Build_Ships
         --    (Empire   => For_Empire,
         --     Design   => Athena.Empires.Recon_Design (For_Empire),
         --     Fleet    => Athena.Handles.Fleet.Empty_Handle,
         --     Manager  => Manager,
         --     Send_To  => Athena.Handles.Star.Empty_Handle,
         --     Count    => Required_Ships,
         --     Priority => Manager.Priority);
      end if;

   end Evaluate_Threats;

   --------------------------
   -- Find_Available_Fleet --
   --------------------------

   function Find_Available_Fleet
     (Manager     : in out Attack_Manager'Class;
      Origin      : Athena.Handles.Star.Star_Handle;
      Destination : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Fleet.Fleet_Handle
   is
      Empire : constant Athena.Handles.Empire.Empire_Handle :=
                 Athena.Handles.Empire.Get (Manager.Empire);

      Available : Athena.Handles.Fleet.Fleet_Handle :=
                    Athena.Handles.Fleet.Empty_Handle;
      Closest : Non_Negative_Real := Non_Negative_Real'Last;

      procedure Check_Assigned_Fleet
        (Reference : Athena.Handles.Fleet_Reference);

      procedure Check_Idle_Fleet
        (Reference : Athena.Handles.Fleet_Reference);

      --------------------------
      -- Check_Assigned_Fleet --
      --------------------------

      procedure Check_Assigned_Fleet
        (Reference : Athena.Handles.Fleet_Reference)
      is
         use type Athena.Handles.Star.Star_Handle;
         Fleet : constant Athena.Handles.Fleet.Fleet_Handle :=
                   Athena.Handles.Fleet.Get (Reference);
      begin
         if Fleet.Destination.Has_Element
           and then Fleet.Destination = Origin
         then
            Available := Fleet;
         elsif not Fleet.Destination.Has_Element
           and then Fleet.Location = Origin
         then
            Available := Fleet;
         elsif Fleet.Destination.Has_Element
           and then Fleet.Destination = Destination
         then
            Available := Fleet;
         end if;
      end Check_Assigned_Fleet;

      ----------------------
      -- Check_Idle_Fleet --
      ----------------------

      procedure Check_Idle_Fleet
        (Reference : Athena.Handles.Fleet_Reference)
      is
         Fleet : constant Athena.Handles.Fleet.Fleet_Handle :=
                   Athena.Handles.Fleet.Get (Reference);
         D     : constant Non_Negative_Real :=
                   Athena.Stars.Distance
                     (Fleet.Location, Origin);
      begin
         if not Fleet.Has_Destination
           and then D < Closest
         then
            Closest   := D;
            Available := Fleet;
         end if;
      end Check_Idle_Fleet;

   begin
      Empire.Iterate_Fleets (Check_Assigned_Fleet'Access);

      if Available.Has_Element then
         return Available;
      end if;

      Empire.Iterate_Fleets (Check_Idle_Fleet'Access);

      if Available.Has_Element then
         return Available;
      end if;

      Manager.Log
        ("creating new fleet for attack from "
         & Origin.Name & " to " & Destination.Name);

      return Athena.Handles.Fleet.Create
        (Name  => "Task Force",
         Star  => Origin,
         Owner => Empire);

   end Find_Available_Fleet;

   function Sufficient_Attack_Force
     (Knowledge : Athena.Handles.Knowledge.Knowledge_Handle;
      Fleet     : Athena.Handles.Fleet.Fleet_Handle;
      Target    : Athena.Handles.Star.Star_Handle)
      return Boolean
   is
      Fleet_Ships   : Athena.Ships.Lists.List;
      Opposition    : constant Handles.Knowledge.Known_Ship_Lists.List :=
                        Knowledge.Get_Known_Ships (Target);
      --  Their_Weapons : Non_Negative_Real := 0.0;
      --  Our_Weapons   : Non_Negative_Real := 0.0;

      procedure Add_Fleet_Ship (Reference : Athena.Handles.Ship_Reference);

      --------------------
      -- Add_Fleet_Ship --
      --------------------

      procedure Add_Fleet_Ship (Reference : Athena.Handles.Ship_Reference) is
      begin
         Fleet_Ships.Append (Athena.Handles.Ship.Get (Reference));
      end Add_Fleet_Ship;

   begin

      Fleet.Iterate_Ships (Add_Fleet_Ship'Access);

      --  for Ship of Opposition loop
      --     Their_Weapons := Their_Weapons + Ship.Weapon_Mass;
      --  end loop;
      --  for Ship of Fleet_Ships loop
      --     Our_Weapons := Our_Weapons + Athena.Ships.Weapon_Mass (Ship);
      --  end loop;
      --
      --  Athena.Logging.Log
      --    (Fleet.Empire.Name & ": checking forces for attack on "
      --     & Target.Name & " owned by " & Target.Owner.Name
      --     & ": we have" & Fleet_Ships.Length'Image
      --     & " ships with weapon mass "
      --     & Image (Our_Weapons)
      --     & "; they have " & Opposition.Length'Image
      --     & " ships with weapon mass "
      --     & Image (Their_Weapons));
      --  return Our_Weapons > Their_Weapons * 1.5;

      return Natural (Opposition.Length) < Natural (Fleet_Ships.Length);
   end Sufficient_Attack_Force;

end Athena.Managers.Attack;
