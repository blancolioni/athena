package body Athena.Colonies.Updates is

   function Build_Industry
     (Colony : Minerva.Colony.Colony_Class;
      Target : Non_Negative_Real)
      return Non_Negative_Real;

   --------------------
   -- Build_Industry --
   --------------------

   function Build_Industry
     (Colony : Minerva.Colony.Colony_Class;
      Target : Non_Negative_Real)
      return Non_Negative_Real
   is
      Material       : Non_Negative_Real := Target;
      Construct      : Non_Negative_Real := Target * 5.0;
      Used_Material  : Non_Negative_Real;
      Used_Construct : Non_Negative_Real;
   begin
      Request_Material (Colony, Material);
      Request_Construct (Colony, Construct);

      if Material > 0.0 and then Construct > 0.0 then
         Used_Material := Real'Min (Material, Construct / 5.0);
         Used_Construct := Real'Min (Construct, Material * 5.0);
         Log_Colony (Colony, "using " & Image (Material)
                     & " material and "
                     & Image (Construct)
                     & " construct to build industry");
         Colony.Update_Colony
           .Set_Industry (Colony.Industry + Material)
           .Set_Material (Colony.Material + Material - Used_Material)
           .Set_Construct (Colony.Construct + Construct - Used_Construct)
           .Done;
         return Material;
      else
         Colony.Update_Colony
           .Set_Material (Colony.Material + Material)
           .Set_Construct (Colony.Construct + Construct)
           .Done;
         return 0.0;
      end if;
   end Build_Industry;

   --------------------------
   -- Execute_Colony_Order --
   --------------------------

   procedure Execute_Colony_Order
     (Order : Minerva.Colony_Order.Colony_Order_Class)
   is
   begin
      case Order.Category is
         when Minerva.Db.Build_Industry =>
            declare
               Built : constant Non_Negative_Real :=
                         Build_Industry (Order.Colony, Order.Value);
            begin
               Log_Colony (Order.Colony,
                           "built " & Image (Built) & " industry");
               Order.Update_Colony_Order
                 .Set_Value (Order.Value - Built)
                 .Done;
            end;
         when Minerva.Db.Produce_Material =>
            null;
      end case;
   end Execute_Colony_Order;

end Athena.Colonies.Updates;
