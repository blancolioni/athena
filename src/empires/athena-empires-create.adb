with Athena.Managers;
with Athena.Money;

with Athena.Colonies;
with Athena.Ships.Create;

with Athena.Handles.Colony;
with Athena.Handles.Commodity;
with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.Facility;
with Athena.Handles.Fleet;
with Athena.Handles.Installation;

package body Athena.Empires.Create is

   ----------------
   -- New_Empire --
   ----------------

   procedure New_Empire
     (World     : Athena.Handles.World.World_Handle;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : Athena.Color.Athena_Color;
      Template  : Tropos.Configuration)
   is
      Init : constant Tropos.Configuration :=
        Template.Child ("init");

      function Get (Name : String) return Real
      is (Real (Long_Float'(Init.Get (Name, 0.0))));

      Empire         : constant Handles.Empire.Empire_Handle :=
                         Athena.Handles.Empire.Create_Empire
                           (Name       => Name,
                            Plural     => Plural,
                            Adjective  => Adjective,
                            World      => World.Reference,
                            Cash       =>
                              Athena.Money.To_Money (Get ("cash")),
                            Debt       =>
                              Athena.Money.To_Money (Get ("debt")),
                            Color      => Color);

   begin

      World.Set_Name (Capital);
      World.Set_Owner (Empire.Reference);

      declare
         Colony : constant Athena.Handles.Colony.Colony_Handle :=
                    Athena.Colonies.New_Colony
                      (World      => World,
                       Owner      => Empire,
                       Pop        => Get ("pop"),
                       Industry   => Get ("ind"),
                       Material   => 100.0);
      begin
         Empire.Set_Capital (Colony.Reference);

         for Installation_Config of Init.Child ("installations") loop
            declare
               Facility : constant Athena.Handles.Facility.Facility_Handle :=
                 Athena.Handles.Facility.Get_By_Tag
                   (Installation_Config.Config_Name);
               Size     : constant Non_Negative_Real :=
                 Real'Value (Installation_Config.Attribute ("size"));
            begin
               Colony.Add_Installation
                 (Athena.Handles.Installation.Create (Facility, Size));
            end;
         end loop;

         for Stock_Config of Init.Child ("stock") loop
            Colony.Set_Stock
              (Commodity =>
                 Athena.Handles.Commodity.Get_By_Tag
                   (Stock_Config.Config_Name),
               Quantity  =>
                 Real (Long_Float'(Stock_Config.Value)));
         end loop;

      end;

      for Manager in Athena.Handles.Manager_Class loop
         declare
            Name : constant String := Manager'Image;
         begin
            if Athena.Managers.Exists (Name) then
               Empire.Set_Manager
                 (Manager,
                  Athena.Managers.Get_Manager
                    (Manager'Image, Empire.Reference));
            end if;
         end;
      end loop;

      for Standard_Design_Config of
        Template.Child ("standard-designs")
      loop
         Empire.Set_Standard_Design
           (Class  =>
              Athena.Handles.Empire.Standard_Empire_Design'Value
                (Standard_Design_Config.Config_Name),
            Design =>
              Athena.Handles.Design.Get_By_Name
                (Standard_Design_Config.Value)
            .Reference);
      end loop;

      declare
         use Athena.Handles.Design;
      begin

         for Ship of Init.Child ("ships") loop
            declare
               Design : constant Design_Handle :=
                          Athena.Handles.Design.Get_By_Name
                            (Ship.Config_Name);
               Count  : constant Natural :=
                          (if Ship.Child_Count = 0 then 1 else Ship.Value);
            begin
               for I in 1 .. Count loop
                  Athena.Ships.Create.Create_Ship
                    (Empire  => Empire,
                     Star    => Star,
                     Fleet   => Athena.Handles.Fleet.Empty_Handle,
                     Manager => Design.Default_Manager,
                     Design  => Design,
                     Name    => Athena.Ships.New_Name (Empire, Design.Name));
               end loop;
            end;
         end loop;

      end;

   end New_Empire;

end Athena.Empires.Create;
