with Athena.Colonies;
with Athena.Empires;
with Athena.Turns;

with Minerva.Ship;
with Minerva.Ship_Module;
with Minerva.Upgrade_Order;

package body Athena.Managers.Upgrade is

   type Upgrade_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Upgrade_Manager)
      return String
   is ("upgrade");

   overriding procedure Create_Orders
     (Manager : Upgrade_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Upgrade_Manager)
   is

      procedure Check_Components
        (On_Ship : Minerva.Ship.Ship_Class);

      ----------------------
      -- Check_Components --
      ----------------------

      procedure Check_Components
        (On_Ship : Minerva.Ship.Ship_Class)
      is
         procedure Check_Component_Upgrade
           (Component : Minerva.Ship_Module.Ship_Module_Class);

         -----------------------------
         -- Check_Component_Upgrade --
         -----------------------------

         procedure Check_Component_Upgrade
           (Component : Minerva.Ship_Module.Ship_Module_Class)
         is
            Component_Tec : constant Non_Negative_Real := Component.Tec_Level;
            Condition     : constant Unit_Real := Component.Condition;
            Empire_Tec    : constant Non_Negative_Real :=
                              Athena.Empires.Current_Tec_Level
                                (Component.Ship.Empire,
                                 Component.Design_Component.Technology);
         begin

            if Component_Tec < Empire_Tec - 2.0
              or else (Component_Tec < Empire_Tec - 1.0
                       and then Condition <= 0.5)
              or else (Component_Tec < Empire_Tec and then Condition <= 0.25)
            then

               Manager.Log
                 (On_Ship.Name
                  & "/" & Component.Design_Component.Top_Record'Image
                  & ": upgrading from "
                  & Image (Component_Tec)
                  & " to " & Image (Empire_Tec));
               Minerva.Upgrade_Order.Create
                 (Turn           => Athena.Turns.Current_Turn,
                  Empire         => Component.Ship.Empire,
                  Priority       => Manager.Priority,
                  Ship_Module => Component);
            end if;
         end Check_Component_Upgrade;

      begin
         if On_Ship.Star.Has_Element
           and then not On_Ship.Destination.Has_Element
           and then Athena.Colonies.Get_Colony (On_Ship.Star).Has_Element
           and then On_Ship.Star.Owner.Identifier = On_Ship.Empire.Identifier
         then
            for Module of Minerva.Ship_Module.Select_By_Ship (On_Ship) loop
               Check_Component_Upgrade (Module);
            end loop;
         end if;
      end Check_Components;

   begin
      for Ship of
        Minerva.Ship.Select_By_Empire (Manager.Empire)
      loop
         Check_Components (Ship);
      end loop;
   end Create_Orders;

   -----------------------------
   -- Default_Upgrade_Manager --
   -----------------------------

   function Default_Upgrade_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Upgrade_Manager'
        (Name     => +"upgrade",
         Empire   => <>,
         Manager  => <>,
         Priority => 1050);
   end Default_Upgrade_Manager;

end Athena.Managers.Upgrade;
