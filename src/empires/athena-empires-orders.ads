with Minerva.Research_Order;
with Minerva.Ship_Build_Order;

package Athena.Empires.Orders is

   procedure Apply_Research_Order
     (Order : Minerva.Research_Order.Research_Order_Class);

   procedure Apply_Ship_Build_Order
     (Order : Minerva.Ship_Build_Order.Ship_Build_Order_Class);

end Athena.Empires.Orders;
