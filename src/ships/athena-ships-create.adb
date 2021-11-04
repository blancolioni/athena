with Athena.Empires;
with Athena.Identifiers;
with Athena.Ships.Orders;

with Minerva.Design_Component;
with Minerva.Ship;
with Minerva.Ship_Module;

package body Athena.Ships.Create is

   ------------------
   -- Initial_Ship --
   ------------------

   procedure Create_Ship
     (Empire      : Minerva.Empire.Empire_Class;
      Star        : Minerva.Star.Star_Class;
      Design      : Minerva.Ship_Design.Ship_Design_Class;
      Fleet       : Minerva.Fleet.Fleet_Class;
      Manager     : Minerva.Empire_Manager.Empire_Manager_Class;
      Name        : String;
      Destination : Minerva.Star.Star_Class)
   is
      Unique_Name : constant String := New_Ship_Name (Empire, Name);
      Ship : constant Minerva.Ship.Ship_Handle :=
               Minerva.Ship.Create
                 (Identifier     => Athena.Identifiers.Next_Identifier,
                  Name           => Unique_Name,
                  Empire         => Empire,
                  Fleet          => Fleet,
                  Star           => Star,
                  Ship_Design    => Design,
                  Empire_Manager => Manager,
                  Alive          => True,
                  Experience     => 1.0,
                  First_Order    => 1,
                  Last_Order     => 0,
                  Script         => Design.Default_Script);
   begin
      for Component of
        Minerva.Design_Component.Select_By_Ship_Design (Design)
      loop
         Minerva.Ship_Module.Create
           (Identifier       => Athena.Identifiers.Next_Identifier,
            Ship             => Ship,
            Design_Component => Component,
            Tec_Level        =>
              Athena.Empires.Current_Tec_Level
                (Empire, Component.Technology),
            Condition        => 1.0,
            Damage           => 0.0);
      end loop;

      if Destination.Has_Element
        and then Destination.Identifier /= Star.Identifier
      then
         Athena.Ships.Orders.Move_To
           (Ship        => Ship,
            Destination => Destination);
      end if;
   end Create_Ship;

end Athena.Ships.Create;
