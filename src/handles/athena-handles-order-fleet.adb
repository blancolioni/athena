with Athena.Handles.Fleet;
with Athena.Handles.Star;

package body Athena.Handles.Order.Fleet is

   type Move_Fleet_Record is
     new Root_Order_Record with
      record
         Fleet       : Fleet_Reference;
         Destination : Star_Reference;
      end record;

   overriding function Short_Name
     (Order : Move_Fleet_Record)
      return String
   is ("move fleet");

   overriding procedure Execute
     (Order : in out Move_Fleet_Record);

   --------------------------
   -- Build_Industry_Order --
   --------------------------

   procedure Create_Move_Fleet_Order
     (Fleet     : Fleet_Reference;
      To        : Star_Reference;
      Priority  : Order_Priority)
   is
   begin
      Add_Order
        (Move_Fleet_Record'
           (Priority => Priority,
            Complete => False,
            Fleet    => Fleet,
            Destination => To));
   end Create_Move_Fleet_Order;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Order : in out Move_Fleet_Record)
   is
      use type Athena.Handles.Star.Star_Handle;
      Fleet : constant Athena.Handles.Fleet.Fleet_Handle :=
                Athena.Handles.Fleet.Get (Order.Fleet);
      Destination : constant Athena.Handles.Star.Star_Handle :=
                      Athena.Handles.Star.Get (Order.Destination);
   begin
      if Fleet.Destination /= Destination then
         Fleet.Set_Destination (Destination);
      end if;
      Order.Complete := Fleet.Location = Fleet.Destination;
   end Execute;

end Athena.Handles.Order.Fleet;
