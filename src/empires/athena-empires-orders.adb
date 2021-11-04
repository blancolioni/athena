with Athena.Logging;
with Athena.Real_Images;

with Athena.Colonies;
with Athena.Empires;
with Athena.Ships.Create;
with Athena.Ships.Designs;

package body Athena.Empires.Orders is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   --------------------------
   -- Apply_Research_Order --
   --------------------------

   procedure Apply_Research_Order
     (Order : Minerva.Research_Order.Research_Order_Class)
   is
      Required   : constant Non_Negative_Real := Order.Construct;
      Available  : Non_Negative_Real := 0.0;
      Investment : Non_Negative_Real := 0.0;

      procedure Add_Construct (Colony : Minerva.Colony.Colony_Class);

      procedure Spend_Construct (Colony : Minerva.Colony.Colony_Class);

      -------------------
      -- Add_Construct --
      -------------------

      procedure Add_Construct (Colony : Minerva.Colony.Colony_Class) is
      begin
         Available := Available + Colony.Construct;
      end Add_Construct;

      ---------------------
      -- Spend_Construct --
      ---------------------

      procedure Spend_Construct
        (Colony : Minerva.Colony.Colony_Class)
      is
      begin
         Athena.Colonies.Use_Assets
           (Colony      => Colony,
            Description => Order.Technology.Tag & " research",
            Construct   => Colony.Construct * Investment / Available,
            Material    => 0.0);
      end Spend_Construct;

   begin

      for Colony of
        Minerva.Colony.Select_By_Empire
          (Order.Empire)
      loop
         Add_Construct (Colony);
      end loop;

      if Available > 0.0 then
         Investment := Real'Min (Required, Available);

         declare

         begin
            for Colony of
              Minerva.Colony.Select_By_Empire
                (Order.Empire)
            loop
               Spend_Construct (Colony);
            end loop;

            Athena.Empires.Add_Investment
              (Order.Empire, Order.Technology, Investment);

         end;
      end if;

   end Apply_Research_Order;

   ----------------------------
   -- Apply_Ship_Build_Order --
   ----------------------------

   procedure Apply_Ship_Build_Order
     (Order : Minerva.Ship_Build_Order.Ship_Build_Order_Class)
   is
      Mass           : constant Non_Negative_Real :=
                         Athena.Ships.Designs.Dry_Mass (Order.Ship_Design);
      Material_Cost  : constant Non_Negative_Real := Mass;
      Construct_Cost : constant Non_Negative_Real := Mass * 10.0;

      function Immediately_Available
        (Colony : Minerva.Colony.Colony_Class)
         return Boolean
      is (Colony.Material >= Material_Cost
          and then Colony.Construct >= Construct_Cost);

      function Potentially_Available
        (Colony : Minerva.Colony.Colony_Class)
         return Boolean
      is (Athena.Colonies.Can_Provide
          (Colony    => Colony,
           Construct => Construct_Cost,
           Material  => Material_Cost));

      procedure Execute (Colony : Minerva.Colony.Colony_Class);

      -------------
      -- Execute --
      -------------

      procedure Execute (Colony : Minerva.Colony.Colony_Class) is
      begin
         if Colony.Material < Material_Cost then
            Athena.Colonies.Produce_Material
              (Colony, Material_Cost - Colony.Material);
         end if;

         pragma Assert (Colony.Construct >= Construct_Cost);
         pragma Assert (Colony.Material >= Material_Cost);

         Athena.Ships.Create.Create_Ship
           (Empire => Order.Empire,
            Star   => Colony.Star,
            Design => Order.Ship_Design,
            Fleet  => Order.Fleet,
            Manager => Order.Manager,
            Name   => Order.Ship_Design.Name,
            Destination => Order.Send_To);

         Athena.Colonies.Use_Assets
           (Colony      => Colony,
            Construct   => Construct_Cost,
            Material    => Material_Cost,
            Description =>
              "building class " & Order.Ship_Design.Name & " ship");

      end Execute;

      Immediate_Colony   : constant Minerva.Colony.Colony_Class :=
                             Athena.Colonies.Find_Colony
                               (Order.Empire, Immediately_Available'Access);
   begin

      if Immediate_Colony.Has_Element then
         Execute (Immediate_Colony);
      else
         declare
            Potential_Colony : constant Minerva.Colony.Colony_Class :=
                                 Athena.Colonies.Find_Colony
                                   (Order.Empire,
                                    Potentially_Available'Access);
         begin
            if Potential_Colony.Has_Element then
               Execute (Potential_Colony);
            else
               Athena.Logging.Log
                 (Order.Empire.Name & ": skipping " & Order.Ship_Design.Name
                  & " build due to insufficient resources"
                  & " (required " & Image (Material_Cost)
                  & " material and " & Image (Construct_Cost)
                  & " construct)");
            end if;
         end;
      end if;

   end Apply_Ship_Build_Order;

end Athena.Empires.Orders;
