package Athena.Handles.Order.Transport is

   procedure Create_Transport_Order
     (Empire   : Empire_Reference;
      From     : Star_Reference;
      To       : Star_Reference;
      Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real;
      Priority : Order_Priority);

end Athena.Handles.Order.Transport;
