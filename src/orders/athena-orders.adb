--  with Athena.Logging;
with Athena.Real_Images;

--  with Athena.Handles.Colony;
--  with Athena.Handles.Star;

with Athena.Handles.Order.Colony;
with Athena.Handles.Order.Fleet;
with Athena.Handles.Order.Research;
--  with Athena.Handles.Order.Transport;

package body Athena.Orders is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image
     with Unreferenced;

   -----------------
   -- Build_Ships --
   -----------------

   --  procedure Build_Ships
   --    (Empire   : Athena.Handles.Empire.Empire_Class;
   --     Design   : Athena.Handles.Ship_Design.Ship_Design_Class;
   --     Fleet    : Athena.Handles.Fleet.Fleet_Class;
   --     Manager  : Athena.Handles.Manager.Manager_Class;
   --     Send_To  : Athena.Handles.Star.Star_Class;
   --     Count    : Positive;
   --     Priority : Positive)
   --  is
   --  begin
   --     Athena.Logging.Log
   --       (Empire.Name & " orders"
   --        & (if Count = 1 then " " else Count'Image & " x ")
   --        & Design.Name);
   --
   --     for I in 1 .. Count loop
   --        Athena.Handles.Ship_Build_Order.Create
   --          (Turn        => Athena.Turns.Current_Turn,
   --           Empire      => Empire,
   --           Priority    => Priority,
   --           Ship_Design => Design,
   --           Manager     =>
   --             Athena.Empires.Get_Manager (Empire, Manager),
   --           Fleet       => Fleet,
   --           Send_To     => Send_To);
   --     end loop;
   --
   --  end Build_Ships;

   ----------------
   -- Move_Cargo --
   ----------------

   --  procedure Move_Cargo
   --    (Cargo    : Athena.Handles.Cargo_Class;
   --     Quantity : Non_Negative_Real;
   --     From     : Athena.Handles.Colony_Reference;
   --     To       : Athena.Handles.Star_Reference;
   --     Priority : Athena.Handles.Order_Priority)
   --  is
   --     Colony : constant Athena.Handles.Colony.Colony_Handle :=
   --                Athena.Handles.Colony.Get (From);
   --     Star   : constant Athena.Handles.Star.Star_Handle :=
   --                Athena.Handles.Star.Get (To);
   --  begin
   --     Athena.Logging.Log
   --       (Colony.Owner.Name & " colony on "
   --        & Colony.Star.Name
   --        & " ordered to move "
   --        & Image (Quantity)
   --        & " "
   --        & Cargo'Image
   --        & " to "
   --        & Star.Name);
   --     Athena.Handles.Order.Transport.Create_Transport_Order
   --       (Empire   => Colony.Owner.Reference,
   --        From     => Colony.Star.Reference,
   --        To       => To,
   --        Cargo    => Cargo,
   --        Quantity => Quantity,
   --        Priority => Priority);
   --  end Move_Cargo;

   ----------------
   -- Move_Fleet --
   ----------------

   procedure Move_Fleet
     (Fleet       : Athena.Handles.Fleet_Reference;
      Destination : Athena.Handles.Star_Reference;
      Priority    : Athena.Handles.Order_Priority)
   is
   begin
      Athena.Handles.Order.Fleet.Create_Move_Fleet_Order
        (Fleet    => Fleet,
         To       => Destination,
         Priority => Priority);
   end Move_Fleet;

   --------------------
   -- Order_Industry --
   --------------------

   procedure Order_Industry
     (Colony   : Athena.Handles.Colony_Reference;
      Quantity : Non_Negative_Real;
      Priority : Athena.Handles.Order_Priority)
   is
   begin
      Athena.Handles.Order.Colony.Build_Industry_Order
        (Colony      => Colony,
         Quantity    => Quantity,
         Priority    => Priority);
   end Order_Industry;

   -------------------------
   -- Research_Technology --
   -------------------------

   procedure Research_Technology
     (Empire     : Athena.Handles.Empire_Reference;
      Technology : Athena.Handles.Technology_Reference;
      Construct  : Non_Negative_Real;
      Priority   : Athena.Handles.Order_Priority)
   is
   begin
      Athena.Handles.Order.Research.Research_Investment_Order
        (Empire     => Empire,
         Technology => Technology,
         Construct  => Construct,
         Priority   => Priority);
   end Research_Technology;

   ---------------------
   -- Set_Destination --
   ---------------------

   --  procedure Set_Destination
   --    (Ship        : Athena.Handles.Ship.Ship_Class;
   --     Destination : Athena.Handles.Star.Star_Class;
   --     Priority    : Positive)
   --  is
   --     pragma Unreferenced (Priority);
   --  begin
   --     Athena.Logging.Log
   --       (Ship.Empire.Adjective
   --        & " ship " & Ship.Name
   --        & " at " & Ship.Star.Name
   --        & " ordered to " & Destination.Name
   --        & " distance "
   --        & Image (Athena.Stars.Distance (Ship.Star, Destination)));
   --     Ship.Update_Ship
   --       .Set_Destination (Destination.Reference_Star)
   --       .Set_Progress (0.0)
   --       .Done;
   --  end Set_Destination;

end Athena.Orders;
