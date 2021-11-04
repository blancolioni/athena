with Athena.Empires;
with Athena.Ships;
with Athena.Turns;

with Minerva.Colony;
with Minerva.Fleet;
with Minerva.Ship;
with Minerva.Ship_Build_Order;

package body Athena.Managers.Defend is

   type Defend_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Defend_Manager)
      return String
   is ("defend");

   overriding procedure Create_Orders
     (Manager : Defend_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Defend_Manager)
   is

      procedure Check_Defenders (Colony : Minerva.Colony.Colony_Class);

      ---------------------
      -- Check_Defenders --
      ---------------------

      procedure Check_Defenders
        (Colony : Minerva.Colony.Colony_Class)
      is
         Required  : constant Positive :=
                       Natural'Max
                         (Natural (Colony.Industry / 1000.0),
                          Natural (Colony.Population / 1000.0))
                       + 1;
         Available : Natural := 0;
      begin
         for Ship of
           Minerva.Ship.Select_By_Empire_Manager
             (Manager.Manager)
         loop
            if (Ship.Star.Identifier = Colony.Star.Identifier
                and then not Athena.Ships.Has_Destination (Ship))
              or else Athena.Ships.Has_Destination (Ship, Colony.Star)
            then
               Available := Available + 1;
            end if;
         end loop;

         if Available < Required then
            Manager.Log
              ("colony on "
               & Colony.Star.Name
               & ": assigned defenders" & Available'Image
               & "; required" & Required'Image);

            Minerva.Ship_Build_Order.Create
              (Turn        => Athena.Turns.Current_Turn,
               Empire      => Manager.Empire,
               Priority    => Manager.Priority,
               Ship_Design =>
                 Athena.Empires.Standard_Defender_Design (Manager.Empire),
               Manager     => Manager.Manager,
               Fleet       => Minerva.Fleet.Empty_Handle,
               Send_To     => Colony.Star,
               Count       => Required - Available);
         end if;

      end Check_Defenders;

   begin
      for Colony of
        Minerva.Colony.Select_By_Empire (Manager.Empire)
      loop
         Check_Defenders (Colony);
      end loop;
   end Create_Orders;

   ----------------------------
   -- Default_Defend_Manager --
   ----------------------------

   function Default_Defend_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Defend_Manager'
        (Name     => +"defend",
         Empire   => <>,
         Manager  => <>,
         Priority => 1050);
   end Default_Defend_Manager;

end Athena.Managers.Defend;
