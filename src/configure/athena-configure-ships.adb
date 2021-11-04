with Ada.Text_IO;

with WL.String_Maps;

with Athena.Identifiers;
with Athena.Technology;

with Minerva.Manager;
with Minerva.Ship_Design;

with Minerva.Cargo_Component;
with Minerva.Drive_Component;
with Minerva.Laser_Component;
with Minerva.Missile_Component;
with Minerva.Repair_Component;
with Minerva.Shield_Component;

package body Athena.Configure.Ships is

   procedure Configure_Design (Design_Config : Tropos.Configuration);

   procedure Configure_Component
     (Design           : Minerva.Ship_Design.Ship_Design_Class;
      Component_Config : Tropos.Configuration);

   type Component_Config_Procedure is access
     procedure (Design           : Minerva.Ship_Design.Ship_Design_Class;
                Component_Config : Tropos.Configuration);

   package Component_Config_Maps is
     new WL.String_Maps (Component_Config_Procedure);

   Component_Config_Map : Component_Config_Maps.Map;

   procedure Configure_Cargo
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration);

   procedure Configure_Drive
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration);

   procedure Configure_Laser
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration);

   procedure Configure_Missile
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration);

   procedure Configure_Repair
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration);

   procedure Configure_Shield
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration);

   ---------------------
   -- Configure_Cargo --
   ---------------------

   procedure Configure_Cargo
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration)
   is
   begin
      Minerva.Cargo_Component.Create
        (Ship_Design => Design,
         Mass        => Config.Value,
         Technology  => Athena.Technology.Get ("cargo"),
         Identifier  => Athena.Identifiers.Next_Identifier);
   end Configure_Cargo;

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Design           : Minerva.Ship_Design.Ship_Design_Class;
      Component_Config : Tropos.Configuration)
   is
      Tag : constant String := Component_Config.Config_Name;
   begin
      if Component_Config_Map.Contains (Tag) then
         Component_Config_Map.Element (Tag) (Design, Component_Config);
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Tag & ": no such component type");
      end if;
   end Configure_Component;

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design (Design_Config : Tropos.Configuration) is
      Design : constant Minerva.Ship_Design.Ship_Design_Handle :=
                 Minerva.Ship_Design.Create
                   (Name           => Design_Config.Config_Name,
                    Identifier     => Athena.Identifiers.Next_Identifier,
                    Default_Manager =>
                      Minerva.Manager.Get_By_Tag
                        (Design_Config.Get ("default-manager", "")),
                    Default_Script =>
                      Design_Config.Get ("default-script", "escape"),
                    Default_Rank   =>
                      Design_Config.Get ("default-rank", 9));
   begin
      for Component_Config of Design_Config.Child ("design") loop
         Configure_Component (Design, Component_Config);
      end loop;
   end Configure_Design;

   -----------------------
   -- Configure_Designs --
   -----------------------

   procedure Configure_Designs (Designs_Config : Tropos.Configuration) is
   begin
      for Design_Config of Designs_Config loop
         Configure_Design (Design_Config);
      end loop;
   end Configure_Designs;

   ---------------------
   -- Configure_Drive --
   ---------------------

   procedure Configure_Drive
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration)
   is
   begin
      Minerva.Drive_Component.Create
        (Ship_Design => Design,
         Technology  => Athena.Technology.Get ("drive"),
         Mass        => Config.Value,
         Identifier  => Athena.Identifiers.Next_Identifier);
   end Configure_Drive;

   ---------------------
   -- Configure_Laser --
   ---------------------

   procedure Configure_Laser
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration)
   is
      Antimissile : constant Boolean := Config.Get ("antimissile");
      Power       : constant Non_Negative_Real := Config.Get ("power");
      Mass        : constant Non_Negative_Real := Power;
   begin
      Minerva.Laser_Component.Create
        (Ship_Design  => Design,
         Technology   => Athena.Technology.Get ("weapon"),
         Mass         => Mass,
         Identifier   => Athena.Identifiers.Next_Identifier,
         Anti_Missile => Antimissile,
         Power        => Power);
   end Configure_Laser;

   -----------------------
   -- Configure_Missile --
   -----------------------

   procedure Configure_Missile
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration)
   is
      Antimissile : constant Boolean := Config.Get ("antimissile");
      Power       : constant Non_Negative_Real := Config.Get ("power");
      Rails       : constant Positive := Config.Get ("rails");
      Launches    : constant Positive := Config.Get ("launches");
      Warhead     : constant Unit_Real := Config.Get ("warhead");
      Mass        : constant Non_Negative_Real :=
                      Power * (1.0 + Real (Rails - 1) / 2.0)
                      * (1.0 + Real (Launches - 1) / 2.0);
   begin
      Minerva.Missile_Component.Create
        (Ship_Design  => Design,
         Technology   => Athena.Technology.Get ("weapon"),
         Mass         => Mass,
         Identifier   => Athena.Identifiers.Next_Identifier,
         Anti_Missile => Antimissile,
         Power        => Power,
         Rails        => Rails,
         Launches     => Launches,
         Warhead      => Warhead);
   end Configure_Missile;

   ----------------------
   -- Configure_Repair --
   ----------------------

   procedure Configure_Repair
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration)
   is
   begin
      Minerva.Repair_Component.Create
        (Ship_Design => Design,
         Technology  => Athena.Technology.Get ("drive"),
         Mass        => Config.Value,
         Identifier  => Athena.Identifiers.Next_Identifier);
   end Configure_Repair;

   ----------------------
   -- Configure_Shield --
   ----------------------

   procedure Configure_Shield
     (Design : Minerva.Ship_Design.Ship_Design_Class;
      Config : Tropos.Configuration)
   is
   begin
      Minerva.Shield_Component.Create
        (Ship_Design => Design,
         Technology  => Athena.Technology.Get ("shield"),
         Mass        => Config.Value,
         Identifier  => Athena.Identifiers.Next_Identifier);
   end Configure_Shield;

begin
   Component_Config_Map.Insert ("cargo", Configure_Cargo'Access);
   Component_Config_Map.Insert ("drive", Configure_Drive'Access);
   Component_Config_Map.Insert ("laser", Configure_Laser'Access);
   Component_Config_Map.Insert ("missile", Configure_Missile'Access);
   Component_Config_Map.Insert ("repair", Configure_Repair'Access);
   Component_Config_Map.Insert ("shield", Configure_Shield'Access);
end Athena.Configure.Ships;
