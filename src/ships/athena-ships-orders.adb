with Athena.Colonies;
with Athena.Empires;

with Minerva.Colony;
with Minerva.Ship;
with Minerva.Ship_Order;

with Minerva.Db;

package body Athena.Ships.Orders is

   function Next_Order
     (Ship : Ship_Class)
      return Minerva.Ship_Order.Ship_Order_Class;

   -------------------------
   -- Apply_Upgrade_Order --
   -------------------------

   procedure Apply_Upgrade_Order
     (Order : Minerva.Upgrade_Order.Upgrade_Order_Class)
   is
      Material_Cost : constant Non_Negative_Real :=
                        Order.Ship_Module.Design_Component.Mass;
      Construct_Cost : constant Non_Negative_Real :=
                         Material_Cost * 10.0;
      Colony         : constant Minerva.Colony.Colony_Class :=
                         Athena.Colonies.Get_Colony
                           (Order.Ship_Module.Ship.Star);
      Ship           : constant Minerva.Ship.Ship_Class :=
                         Order.Ship_Module.Ship;
   begin
      if Athena.Colonies.Can_Provide
        (Colony    => Colony,
         Construct => Construct_Cost,
         Material  => Material_Cost)
      then
         Athena.Colonies.Use_Assets
           (Colony      => Colony,
            Description =>
              "upgrading "
            & Order.Ship_Module.Design_Component.Top_Record'Image
            & " on "
            & Ship.Identifier
            & " "
            & Ship.Name,
            Construct   => Construct_Cost,
            Material    => Material_Cost);
         Order.Ship_Module.Update_Ship_Module
           .Set_Tec_Level
             (Athena.Empires.Current_Tec_Level
                (Order.Empire,
                 Order.Ship_Module.Design_Component.Technology))
           .Done;
      end if;
   end Apply_Upgrade_Order;

   ----------------
   -- Load_Cargo --
   ----------------

   procedure Load_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
   is
      Order : constant Minerva.Ship_Order.Ship_Order_Class :=
               Next_Order (Ship);
   begin
      Order.Update_Ship_Order
        .Set_Action (Minerva.Db.Load)
        .Set_Cargo (Cargo)
        .Set_Quantity (Quantity)
        .Done;
   end Load_Cargo;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Ship        : Ship_Class;
      Destination : Minerva.Star.Star_Class)
   is
      Move : constant Minerva.Ship_Order.Ship_Order_Class :=
               Next_Order (Ship);
   begin
      Move.Update_Ship_Order
        .Set_Action (Minerva.Db.Move)
        .Set_Star (Destination)
        .Done;
   end Move_To;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Fleet       : Fleet_Class;
      Destination : Minerva.Star.Star_Class)
   is
   begin
      Fleet.Update_Fleet
        .Set_Destination (Destination)
        .Set_Progress (0.0)
        .Done;
   end Move_To;

   ----------------
   -- Next_Order --
   ----------------

   function Next_Order
     (Ship : Ship_Class)
      return Minerva.Ship_Order.Ship_Order_Class
   is
      First : constant Positive :=
                (if Ship.Last_Order < Ship.First_Order
                 then 1
                 else Ship.First_Order);
      Current_Last : constant Natural :=
                       (if Ship.Last_Order < Ship.First_Order
                        then 0
                        else Ship.Last_Order);
      Next_Last    : constant Positive :=
                       Current_Last + 1;
      Order        : constant Minerva.Ship_Order.Ship_Order_Handle :=
                       Minerva.Ship_Order.Get_By_Ship_Order
                         (Ship, Next_Last);
   begin
      Ship.Update_Ship
        .Set_First_Order (First)
        .Set_Last_Order (Next_Last)
        .Done;

      if Order.Has_Element then
         return Order;
      else
         return Minerva.Ship_Order.Create
           (Ship     => Ship,
            Sequence => Next_Last);
      end if;
   end Next_Order;

   ------------------
   -- Unload_Cargo --
   ------------------

   procedure Unload_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
   is
      Order : constant Minerva.Ship_Order.Ship_Order_Class :=
                Next_Order (Ship);
   begin
      Order.Update_Ship_Order
        .Set_Action (Minerva.Db.Unload)
        .Set_Cargo (Cargo)
        .Set_Quantity (Quantity)
        .Done;
   end Unload_Cargo;

end Athena.Ships.Orders;
