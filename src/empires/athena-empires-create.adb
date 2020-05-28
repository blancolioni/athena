with Athena.Managers;
with Athena.Money;

with Athena.Colonies;
with Athena.Ships.Create;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Fleet;

with Athena.Handles.Design;

package body Athena.Empires.Create is

   ----------------
   -- New_Empire --
   ----------------

   procedure New_Empire
     (Star      : Athena.Handles.Star.Star_Handle;
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
                            Star       => Star.Reference,
                            Cash       =>
                              Athena.Money.To_Money (Get ("cash")),
                            Debt       =>
                              Athena.Money.To_Money (Get ("debt")),
                            Color      => Color);

   begin

      Star.Set_Name (Capital);
      Star.Set_Owner (Empire.Reference);

      declare
         Colony : constant Athena.Handles.Colony.Colony_Handle :=
                    Athena.Colonies.New_Colony
                      (At_Star    => Star,
                       Owner      => Empire,
                       Pop        => Get ("pop"),
                       Industry   => Get ("ind"),
                       Material   => 100.0);
      begin
         Empire.Set_Capital (Colony.Reference);
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

         function Design (Name : String) return Design_Handle
                          renames Athena.Handles.Design.Get_By_Name;

         Scout_Design     : constant Design_Handle := Design ("scout");
         Transport_Design : constant Design_Handle := Design ("transport");
         Freighter_Design : constant Design_Handle := Design ("freighter");
         Defender_Design  : constant Design_Handle := Design ("defender");
         Destroyer_Design : constant Design_Handle := Design ("destroyer");
      begin

         Athena.Ships.Create.Create_Ship
           (Empire  => Empire,
            Star    => Star,
            Fleet   => Athena.Handles.Fleet.Empty_Handle,
            Manager => Athena.Handles.Exploration_Manager,
            Design  => Scout_Design,
            Name    => "Scout I");

         Athena.Ships.Create.Create_Ship
           (Empire  => Empire,
            Star    => Star,
            Fleet   => Athena.Handles.Fleet.Empty_Handle,
            Manager => Athena.Handles.Exploration_Manager,
            Design  => Scout_Design,
            Name    => "Scout II");

         Athena.Ships.Create.Create_Ship
           (Empire  => Empire,
            Star    => Star,
            Fleet   => Athena.Handles.Fleet.Empty_Handle,
            Manager => Athena.Handles.Defense_Manager,
            Design  => Defender_Design,
            Name    => "Defender I");

         Athena.Ships.Create.Create_Ship
           (Empire  => Empire,
            Star    => Star,
            Fleet   => Athena.Handles.Fleet.Empty_Handle,
            Manager => Athena.Handles.Transport_Manager,
            Design  => Transport_Design,
            Name    => "Transport I");

         Athena.Ships.Create.Create_Ship
           (Empire  => Empire,
            Star    => Star,
            Fleet   => Athena.Handles.Fleet.Empty_Handle,
            Manager => Athena.Handles.Transport_Manager,
            Design  => Freighter_Design,
            Name    => "Freighter I");

         Athena.Ships.Create.Create_Ship
           (Empire  => Empire,
            Star    => Star,
            Fleet   => Athena.Handles.Fleet.Empty_Handle,
            Manager => Athena.Handles.Attack_Manager,
            Design  => Destroyer_Design,
            Name    => "Destroyer I");

      end;

   end New_Empire;

end Athena.Empires.Create;
