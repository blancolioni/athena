with Athena.Logging;
with Athena.Money;

with Athena.Colonies;
with Athena.Empires;

with Athena.Handles.Colony;

package body Athena.Handles.Order.Colony is

   type Build_Industry_Record is
     new Root_Order_Record with
      record
         Colony   : Colony_Reference;
         Quantity : Non_Negative_Real;
      end record;

   overriding function Short_Name
     (Order : Build_Industry_Record)
      return String
   is ("build industry");

   overriding procedure Execute
     (Order : in out Build_Industry_Record);

   --------------------------
   -- Build_Industry_Order --
   --------------------------

   procedure Build_Industry_Order
     (Colony    : Colony_Reference;
      Quantity  : Non_Negative_Real;
      Priority  : Order_Priority)
   is
   begin
      Add_Order
        (Build_Industry_Record'
           (Priority => Priority,
            Complete => False,
            Colony   => Colony,
            Quantity => Quantity));
   end Build_Industry_Order;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Order : in out Build_Industry_Record)
   is
      use Athena.Money;
      Colony : constant Athena.Handles.Colony.Colony_Handle :=
                 Athena.Handles.Colony.Get (Order.Colony);
      Max : Non_Negative_Real := Order.Quantity;
   begin
      Max := Real'Min (Max, Colony.Construct / 4.0);
      Max := Real'Min (Max, To_Real (Colony.Owner.Cash));

      if Colony.Material < Max then
         Athena.Colonies.Produce_Material (Colony, Max - Colony.Material);
         Max := Real'Min (Max, Colony.Construct / 4.0);
         Max := Real'Min (Max, To_Real (Colony.Owner.Cash));
      end if;

      Max := Real'Min (Max, Colony.Material);

      declare
         Produced      : constant Non_Negative_Real := Max;
         New_Construct : constant Real :=
                           Colony.Construct - Produced * 4.0;
         New_Material  : constant Real :=
                           Colony.Material - Produced;
         New_Industry  : constant Non_Negative_Real :=
                           Colony.Industry + Produced;
      begin
         Athena.Logging.Log
           (Colony.Owner.Name
            & ": colony on "
            & Colony.Star.Name
            & ": ordered "
            & Image (Order.Quantity)
            & " industry"
            & "; available construct "
            & Image (Colony.Construct)
            & " cash "
            & Athena.Money.Show (Colony.Owner.Cash)
            & " material "
            & Image (Colony.Material)
            & "; produced "
            & Image (Produced)
            & "; new industry level "
            & Image (New_Industry));

         Colony.Set_Industry (New_Industry);
         Colony.Set_Construct (Real'Max (New_Construct, 0.0));
         Colony.Set_Material (Real'Max (New_Material, 0.0));

         Order.Complete := True;

         Athena.Empires.Pay (Colony.Owner, To_Money (Produced),
                             "industry construction");
      end;
   end Execute;

end Athena.Handles.Order.Colony;
