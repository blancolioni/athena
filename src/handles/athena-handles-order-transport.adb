package body Athena.Handles.Order.Transport is

   type Transport_Cargo_Record is
     new Root_Order_Record with
      record
         Empire   : Empire_Reference;
         From     : Star_Reference;
         To       : Star_Reference;
         Cargo    : Cargo_Class;
         Quantity : Non_Negative_Real;
      end record;

   overriding procedure Execute
     (Order : in out Transport_Cargo_Record);

   ----------------------------
   -- Create_Transport_Order --
   ----------------------------

   procedure Create_Transport_Order
     (Empire   : Empire_Reference;
      From     : Star_Reference;
      To       : Star_Reference;
      Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real;
      Priority : Order_Priority)
   is
   begin
      Add_Order
        (Transport_Cargo_Record'
           (Priority    => Priority,
            Complete    => False,
            Empire      => Empire,
            From        => From,
            To          => To,
            Cargo       => Cargo,
            Quantity    => Quantity));
   end Create_Transport_Order;

   overriding procedure Execute
     (Order : in out Transport_Cargo_Record)
   is
   begin
      null;
   end Execute;

end Athena.Handles.Order.Transport;
