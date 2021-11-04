with Athena.Colonies;

with Minerva.Colony;

package body Athena.Managers.Development is

   type Development_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Development_Manager)
      return String
   is ("development");

   overriding procedure Create_Orders
     (Manager : Development_Manager);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Development_Manager)
   is
      procedure Check_Colony
        (Colony : Minerva.Colony.Colony_Class);

      ------------------
      -- Check_Colony --
      ------------------

      procedure Check_Colony
        (Colony : Minerva.Colony.Colony_Class)
      is
      begin
         if Colony.Industry < Colony.Population then
            declare
               Wanted : constant Non_Negative_Real :=
                          Colony.Population - Colony.Industry;
               Max_Build : constant Non_Negative_Real :=
                             (Colony.Star.Resource * Colony.Construct
                              + Colony.Material)
                             / (5.0 * Colony.Star.Resource + 1.0);
               Build     : constant Non_Negative_Real :=
                             Real'Min (Wanted, Max_Build);
            begin
               Manager.Log
                 (Colony.Star.Name
                  & ": industry: want " & Image (Wanted)
                  & "; material " & Image (Colony.Material)
                  & "; construct " & Image (Colony.Construct)
                  & "; resource " & Image (Colony.Star.Resource * 100.0)
                  & "%"
                  & "; max " & Image (Max_Build));

               if Build > 0.0 then
                  Athena.Colonies.Build_Industry
                    (Colony   => Colony,
                     Priority => Manager.Priority,
                     Quantity => Build);
               end if;
            end;
         end if;
      end Check_Colony;

   begin
      for Colony of
        Minerva.Colony.Select_By_Empire
          (Manager.Empire)
      loop
         Check_Colony (Colony);
      end loop;
   end Create_Orders;

   ---------------------------------
   -- Default_Development_Manager --
   ---------------------------------

   function Default_Development_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Manager : constant Development_Manager :=
        (Name     => +"develop",
         Empire   => Minerva.Empire.Empty_Handle,
         Manager  => Minerva.Empire_Manager.Empty_Handle,
         Priority => 1080);
   end Default_Development_Manager;

end Athena.Managers.Development;
