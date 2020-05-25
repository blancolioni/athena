with Athena.Money;

with Athena.Colonies;
with Athena.Empires;

package body Athena.Handles.Colony.Actions is

   type Build_Industry_Action_Record is
     new Root_Colony_Action with
      record
         Quantity : Non_Negative_Real;
      end record;

   overriding function Execute
     (Action   : Build_Industry_Action_Record;
      Colony   : Colony_Handle'Class)
      return Boolean;

   ---------------------------
   -- Build_Industry_Action --
   ---------------------------

   function Build_Industry_Action
     (Quantity : Non_Negative_Real)
      return Root_Colony_Action'Class
   is
   begin
      return Build_Industry_Action_Record'
        (Quantity => Quantity);
   end Build_Industry_Action;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Action   : Build_Industry_Action_Record;
      Colony   : Colony_Handle'Class)
      return Boolean
   is
      use Athena.Money;
      Max    : Real :=
                 Action.Quantity - Colony.Industry;
   begin
      Max := Real'Min (Max, Colony.Construct / 4.0);
      Max := Real'Min (Max, To_Real (Colony.Owner.Cash));

      if Colony.Industry >= Action.Quantity then
         return True;
      end if;

      if Max <= 0.0 then
         return False;
      end if;

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

         Colony.Log
           (": ordered "
            & Image (Action.Quantity - Colony.Industry)
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

         Athena.Empires.Pay (Colony.Owner, To_Money (Produced),
                             "industry construction");

         return False;

      end;
   end Execute;

end Athena.Handles.Colony.Actions;
