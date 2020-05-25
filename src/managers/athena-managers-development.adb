with Athena.Orders;

with Athena.Handles.Colony;
with Athena.Handles.Empire;

package body Athena.Managers.Development is

   type Development_Manager is
     new Root_Manager_Type with null record;

   overriding function Identifier
     (Manager : Development_Manager)
      return String
   is ("development");

   overriding procedure Create_Orders
     (Manager : in out Development_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : in out Development_Manager)
   is
      procedure Check_Colony
        (Reference : Athena.Handles.Colony_Reference);

      ------------------
      -- Check_Colony --
      ------------------

      procedure Check_Colony
        (Reference : Athena.Handles.Colony_Reference)
      is
         Colony : constant Athena.Handles.Colony.Colony_Handle :=
                    Athena.Handles.Colony.Get (Reference);
      begin
         if Colony.Industry < Colony.Population then
            declare
               Required : constant Non_Negative_Real :=
                            Colony.Population - Colony.Industry;
            begin
               Manager.Log
                 ("ordering " & Image (Required)
                  & " industry for colony on " & Colony.Star.Name);

               Athena.Orders.Order_Industry
                 (Colony   => Reference,
                  Quantity => Colony.Population - Colony.Industry,
                  Priority => Manager.Priority);
            end;
         end if;
      end Check_Colony;

   begin
      Athena.Handles.Empire.Get (Manager.Empire)
        .Iterate_Colonies (Check_Colony'Access);
   end Create_Orders;

   ---------------------------------
   -- Default_Development_Manager --
   ---------------------------------

   function Default_Development_Manager
     return Root_Manager_Type'Class
   is
   begin
      return Manager : constant Development_Manager :=
        (Name     => +"develop",
         Priority => 1080,
         Empire   => Athena.Handles.Null_Empire_Reference,
         Messages => <>);
   end Default_Development_Manager;

end Athena.Managers.Development;
