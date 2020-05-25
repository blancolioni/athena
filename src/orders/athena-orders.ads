with Athena.Handles;

package Athena.Orders is

   procedure Order_Industry
     (Colony   : Athena.Handles.Colony_Reference;
      Quantity : Non_Negative_Real;
      Priority : Athena.Handles.Order_Priority);

   --  procedure Set_Destination
   --    (Ship        : Athena.Handles.Ship_Reference;
   --     Destination : Athena.Handles.Star_Reference;
   --     Priority    : Athena.Handles.Order_Priority);

   --  procedure Move_Cargo
   --    (Cargo    : Athena.Handles.Cargo_Class;
   --     Quantity : Non_Negative_Real;
   --     From     : Athena.Handles.Colony_Reference;
   --     To       : Athena.Handles.Star_Reference;
   --     Priority : Athena.Handles.Order_Priority);

   procedure Move_Fleet
     (Fleet       : Athena.Handles.Fleet_Reference;
      Destination : Athena.Handles.Star_Reference;
      Priority    : Athena.Handles.Order_Priority);

   --  procedure Build_Ships
   --    (Empire   : Athena.Handles.Empire_Reference;
   --     Design   : Athena.Handles.Design_Reference;
   --     Fleet    : Athena.Handles.Fleet_Reference;
   --     Manager  : Athena.Managers.Manager_Class;
   --     Send_To  : Athena.Handles.Star_Reference;
   --     Count    : Positive;
   --     Priority : Order_Priority);

   procedure Research_Technology
     (Empire     : Athena.Handles.Empire_Reference;
      Technology : Athena.Handles.Technology_Reference;
      Construct  : Non_Negative_Real;
      Priority   : Athena.Handles.Order_Priority);

end Athena.Orders;
