with Minerva.Star;
with Minerva.Upgrade_Order;

package Athena.Ships.Orders is

   procedure Move_To
     (Fleet       : Fleet_Class;
      Destination : Minerva.Star.Star_Class);

   procedure Move_To
     (Ship        : Ship_Class;
      Destination : Minerva.Star.Star_Class);

   procedure Load_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real);

   procedure Unload_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real);

   procedure Apply_Upgrade_Order
     (Order : Minerva.Upgrade_Order.Upgrade_Order_Class);

end Athena.Ships.Orders;
