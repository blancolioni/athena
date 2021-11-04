with Athena.Money;

with Athena.Technology;
with Athena.Turns;

with Minerva.Colony;
with Minerva.Research_Order;
with Minerva.Technology;

package body Athena.Managers.Research is

   type Research_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Research_Manager)
      return String
   is ("research");

   overriding procedure Create_Orders
     (Manager : Research_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Research_Manager)
   is
      use Athena.Money;

      Allocation : constant Unit_Real :=
                     (if Manager.Empire.Cash < To_Money (1_000.0)
                      then 0.1
                      elsif Manager.Empire.Cash < To_Money (10_000.0)
                      then 0.2
                      else 0.4);

      Total_Investment : Non_Negative_Real := 0.0;

      procedure Research
        (Technology : Minerva.Technology.Technology_Class;
         Construct  : Non_Negative_Real);

      --------------
      -- Research --
      --------------

      procedure Research
        (Technology : Minerva.Technology.Technology_Class;
         Construct  : Non_Negative_Real)
      is
      begin
         Manager.Log
           ("investing "
            & Image (Construct)
            & " construct into "
            & Technology.Tag
            & " research");

         Minerva.Research_Order.Create
           (Turn       => Athena.Turns.Current_Turn,
            Empire     => Manager.Empire,
            Priority   => Manager.Priority,
            Technology => Technology,
            Construct  => Construct);
      end Research;

   begin

      for Colony of
        Minerva.Colony.Select_By_Empire (Manager.Empire)
      loop
         Total_Investment := Total_Investment + Colony.Industry * Allocation;
      end loop;

      declare
         Drive_Investment : constant Non_Negative_Real :=
                              Total_Investment / 2.0;
      begin
         Research (Athena.Technology.Get ("drive"), Drive_Investment);
         Total_Investment := Total_Investment - Drive_Investment;
      end;

      declare
         Weapon_Investment : constant Non_Negative_Real :=
                               Total_Investment / 2.0;
      begin
         Research (Athena.Technology.Get ("weapon"), Weapon_Investment);
         Total_Investment := Total_Investment - Weapon_Investment;
      end;

      declare
         Shield_Investment : constant Non_Negative_Real :=
                              Total_Investment / 1.5;
      begin
         Research (Athena.Technology.Get ("shield"), Shield_Investment);
         Total_Investment := Total_Investment - Shield_Investment;
      end;

      Research (Athena.Technology.Get ("cargo"), Total_Investment);

   end Create_Orders;

   ------------------------------
   -- Default_Research_Manager --
   ------------------------------

   function Default_Research_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Research_Manager'
        (Name     => +"transport",
         Empire   => <>,
         Manager  => <>,
         Priority => 1050);
   end Default_Research_Manager;

end Athena.Managers.Research;
