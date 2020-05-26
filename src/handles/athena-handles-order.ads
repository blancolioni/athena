with Ada.Streams.Stream_IO;

package Athena.Handles.Order is

   type Order_Handle is
     new Root_Athena_Handle
   with private;

   procedure Execute
     (Order : Order_Handle);

   function Reference (Order : Order_Handle) return Order_Reference;
   function Get (Reference : Order_Reference) return Order_Handle;

   procedure Execute_Orders;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Root_Order_Record is abstract tagged
      record
         Priority : Order_Priority;
         Complete : Boolean;
      end record;

   function Short_Name
     (Order : Root_Order_Record)
      return String
      is abstract;

   procedure Execute (Order : in out Root_Order_Record) is abstract;

   procedure Add_Order
     (Order : Root_Order_Record'Class);

   type Order_Handle is
     new Root_Athena_Handle with
      record
         Reference : Order_Reference := 0;
      end record;

   overriding function Short_Name
     (Order : Order_Handle)
      return String;

   function Reference (Order : Order_Handle) return Order_Reference
   is (Order.Reference);

   function Get (Reference : Order_Reference) return Order_Handle
   is (Reference /= 0, Reference);

end Athena.Handles.Order;
