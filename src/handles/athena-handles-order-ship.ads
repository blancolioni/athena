package Athena.Handles.Order.Ship is

   procedure Create_Move_Ship_Order
     (Ship      : Ship_Reference;
      To        : Star_Reference;
      Priority  : Order_Priority);

end Athena.Handles.Order.Ship;
