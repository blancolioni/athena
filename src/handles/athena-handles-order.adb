with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;

package body Athena.Handles.Order is

   package Order_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Real_Order_Reference, Root_Order_Record'Class);

   package Order_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Order_Reference);

   package Order_Maps is
     new Ada.Containers.Ordered_Maps
       (Order_Priority,
        Order_Reference_Lists.List,
        "<",
        Order_Reference_Lists."=");

   Vector : Order_Vectors.Vector;
   Map    : Order_Maps.Map;

   procedure Add_Order
     (Reference : Order_Reference;
      Order     : Root_Order_Record'Class);

   ---------------
   -- Add_Order --
   ---------------

   procedure Add_Order
     (Order : Root_Order_Record'Class)
   is
   begin
      Vector.Append (Order);
      Add_Order (Vector.Last_Index, Order);
   end Add_Order;

   ---------------
   -- Add_Order --
   ---------------

   procedure Add_Order
     (Reference : Order_Reference;
      Order     : Root_Order_Record'Class)
   is
   begin
      if not Map.Contains (Order.Priority) then
         Map.Insert (Order.Priority, Order_Reference_Lists.Empty_List);
      end if;
      Map (Order.Priority).Append (Reference);
   end Add_Order;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Order : Order_Handle)
   is
   begin
      Vector (Order.Reference).Execute;
   end Execute;

   --------------------
   -- Execute_Orders --
   --------------------

   procedure Execute_Orders is
   begin
      for List of Map loop
         declare
            Old_List : constant Order_Reference_Lists.List :=
                         List;
         begin
            List.Clear;
            for Order of Old_List loop
               declare
                  Rec : Root_Order_Record'Class renames
                            Vector (Order);
               begin
                  Rec.Execute;
                  if not Rec.Complete then
                     List.Append (Order);
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Execute_Orders;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Order_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Add_Order (I, Vector.Element (I));
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Order_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ----------------
   -- Short_Name --
   ----------------

   overriding function Short_Name
     (Order : Order_Handle)
      return String
   is
      Rec : Root_Order_Record'Class renames Vector (Order.Reference);
   begin
      return Rec.Short_Name;
   end Short_Name;

end Athena.Handles.Order;
