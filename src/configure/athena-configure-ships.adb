--  with WL.String_Maps;

with Tropos.Reader;

with Athena.Money;

with Athena.Handles.Component.Bridge;
with Athena.Handles.Component.Computer;
with Athena.Handles.Component.Jump_Drive;
with Athena.Handles.Component.Maneuver;
with Athena.Handles.Component.Power;
with Athena.Handles.Component.Quarters;
with Athena.Handles.Component.Weapon;
with Athena.Handles.Component.Weapon_Mount;

with Athena.Handles.Design;
with Athena.Handles.Design_Module;
with Athena.Handles.Empire;
with Athena.Handles.Hull;
with Athena.Handles.Hull_Armor;

with Athena.Handles.Commodity;

with Athena.Paths;

package body Athena.Configure.Ships is

   --  Basic_Power_Per_Ton : constant := 0.2;

   function Get_Fraction
     (Config : Tropos.Configuration;
      Name   : String)
      return Real
   is (Real (Long_Float'(Config.Get (Name, 1.0))));

   function Get_Value
     (Config : Tropos.Configuration;
      Name   : String)
      return Real
   is (Real (Long_Float'(Config.Get (Name, 0.0))));

   procedure Configure_Design
     (Config : Tropos.Configuration);

   procedure Configure_Armor
     (Config : Tropos.Configuration);

   procedure Configure_Bridge
     (Config : Tropos.Configuration);

   procedure Configure_Computer
     (Config : Tropos.Configuration);

   --  procedure Configure_Generator
   --    (Config : Tropos.Configuration);

   procedure Configure_Hull
     (Config : Tropos.Configuration);

   procedure Configure_Jump_Drive
     (Config : Tropos.Configuration);

   procedure Configure_Maneuver_Drive
     (Config : Tropos.Configuration);

   procedure Configure_Power
     (Config : Tropos.Configuration);

   --  procedure Configure_Jump_Drive
   --    (Config : Tropos.Configuration);

   procedure Configure_Quarters
     (Config : Tropos.Configuration);

   --  procedure Configure_Sensor
   --    (Config : Tropos.Configuration);

   procedure Configure_Weapon
     (Config : Tropos.Configuration);

   procedure Configure_Weapon_Mount
     (Config : Tropos.Configuration);

   --  type Configure_Option is access
   --    procedure (Config : Tropos.Configuration);

   --  package Configure_Maps is
   --    new WL.String_Maps (Configure_Option);
   --
   --  Option_Config : Configure_Maps.Map;

   --  procedure Configure_Fuel_Processor
   --    (Config : Tropos.Configuration);
   --
   --  procedure Configure_Fuel_Scoop
   --    (Config : Tropos.Configuration);

   ---------------------
   -- Configure_Armor --
   ---------------------

   procedure Configure_Armor
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Athena.Handles.Hull_Armor.Create
        (Tag              => Config.Config_Name,
         Mass             => Get ("mass"),
         Tonnage_Fraction => Get ("tonnage"),
         Price_Fraction   => Get ("cost"));
   end Configure_Armor;

   ----------------------
   -- Configure_Bridge --
   ----------------------

   procedure Configure_Bridge
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Tonnage : constant Non_Negative_Real := Get ("tonnage");
      Ship_Tons : constant Tropos.Configuration :=
                    Config.Child ("ship_tonnage");
   begin
      Athena.Handles.Component.Bridge.Create
        (Tag               => Config.Config_Name,
         Tonnage           => Tonnage,
         Mass              => Get ("mass"),
         Power             => Get ("power"),
         Price             => Athena.Money.To_Price (Get ("price")),
         Ship_Tonnage_Low  => Real (Long_Float'(Ship_Tons.Get (1))),
         Ship_Tonnage_High => Real (Long_Float'(Ship_Tons.Get (2))));
   end Configure_Bridge;

   ------------------------
   -- Configure_Computer --
   ------------------------

   procedure Configure_Computer
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));
   begin
      Athena.Handles.Component.Computer.Create
        (Tag      => Config.Config_Name,
         Tonnage  => Get ("tonnage"),
         Mass     => Get ("mass"),
         Power    => Get ("power"),
         Price    => Athena.Money.To_Price (Get ("price")),
         Capacity => Config.Get ("capacity"));
   end Configure_Computer;

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Hull : constant Athena.Handles.Hull.Hull_Handle :=
               Athena.Handles.Hull.Get_By_Tag
                 (Config.Child ("hull").Get ("type", "standard"));
      Mat  : constant Athena.Handles.Commodity.Commodity_Handle :=
                  Athena.Handles.Commodity.Get_By_Tag
                    (Config.Child ("hull").Get ("material"));
      Tonnage : constant Non_Negative_Real := Get ("tonnage");
      Hull_Points : constant Non_Negative_Real :=
                      Tonnage / 2.5
                        + (if Tonnage > 250_000.0
                           then (Tonnage - 250_000.0) / 4.0 else 0.0)
                        + (if Tonnage > 1_000_000.0
                           then (Tonnage - 1_000_000.0) / 2.0 else 0.0);

      Fuel_Tank : constant Non_Negative_Real :=
                    Get_Value (Config, "fuel_tank");

      Firm_Points : constant Natural :=
                      (if Tonnage < 500.0 then 1
                       elsif Tonnage < 1000.0 then 2
                       elsif Tonnage < 2000.0 then 3
                       else 0);
      Hard_Points : constant Natural :=
                      Natural (Real'Truncation (Tonnage / 2000.0));

      function Armor return Athena.Handles.Hull_Armor.Hull_Armor_Handle
      is (if Config.Contains ("armor")
          then Athena.Handles.Hull_Armor.Get_By_Tag
            (Config.Child ("armor").Get ("type"))
          else Athena.Handles.Hull_Armor.Empty_Handle);

      function Armor_Points return Natural
      is (if Config.Contains ("armor")
          then Config.Child ("armor").Get ("points")
          else 0);

      function Parts
        (Config : Tropos.Configuration)
        return Athena.Handles.Component.Component_Array;

      -----------
      -- Parts --
      -----------

      function Parts
        (Config : Tropos.Configuration)
         return Athena.Handles.Component.Component_Array
      is
         Count : constant Natural := Config.Child_Count;
         Index : Natural := 0;
      begin
         return Arr : Athena.Handles.Component.Component_Array (1 .. Count) do
            for Child of Config loop
               Index := Index + 1;
               Arr (Index) :=
                 Athena.Handles.Component.Get_By_Tag
                   (Child.Config_Name);
            end loop;
         end return;
      end Parts;

      Design : constant Athena.Handles.Design.Design_Handle :=
                 Athena.Handles.Design.Create
                   (Name           => Config.Get ("name", Config.Config_Name),
                    Owner          => Athena.Handles.Empire.Empty_Handle,
                    Hull           => Hull,
                    Material       => Mat,
                    Armor          => Armor,
                    Armor_Points   => Armor_Points,
                    Tonnage        => Tonnage,
                    Hull_Points    => Hull_Points,
                    Fuel_Tank      => Fuel_Tank,
                    Firm_Points    => Firm_Points,
                    Hard_Points    => Hard_Points,
                    Default_Script =>
                      Config.Get ("default-script", "escape"),
                    Default_Rank   =>
                      Config.Get ("default-rank", 5),
                    Default_Manager =>
                      Athena.Handles.Manager_Class'Value
                        (Config.Get ("default-manager", "defense")
                        & "_Manager"));

   begin

      for Component_Config of Config.Child ("components") loop
         declare
            use Athena.Handles.Component;
            Component : constant Component_Handle'Class :=
                          Get_By_Tag (Component_Config.Config_Name);
            pragma Assert (Component.Has_Element,
                           "no such component: "
                           & Component_Config.Config_Name);
         begin
            for I in 1 .. Component_Config.Get ("count", 1) loop
               Design.Add_Design_Module
                 (Athena.Handles.Design_Module.Create
                    (Component, Parts (Component_Config.Child ("parts"))));
            end loop;
         end;
      end loop;

   end Configure_Design;

   ------------------------------
   -- Configure_Fuel_Processor --
   ------------------------------

   --  procedure Configure_Fuel_Processor
   --    (Config : Tropos.Configuration)
   --  is
   --     function Get (Name : String) return Real
   --     is (Get_Fraction (Config, Name));
   --  begin
   --     Athena.Handles.Fuel_Processor.Create
   --       (Power_Per_Ton   => Get ("power"),
   --        Minimum_Tonnage => 1.0,
   --        Price_Per_Ton   => Athena.Money.To_Price (Get ("cost")),
   --        Tag             => Config.Config_Name,
   --        Conversion      => Get ("conversion"));
   --  end Configure_Fuel_Processor;
   --
   --------------------------
   -- Configure_Fuel_Scoop --
   --------------------------

   --  procedure Configure_Fuel_Scoop
   --    (Config : Tropos.Configuration)
   --  is
   --     function Get (Name : String) return Real
   --     is (Get_Fraction (Config, Name));
   --  begin
   --     Athena.Handles.Fuel_Scoop.Create
   --       (Minimum_Tonnage => 1.0,
   --        Price_Per_Ton   => Athena.Money.To_Price (Get ("cost")),
   --        Tag             => Config.Config_Name);
   --  end Configure_Fuel_Scoop;
   --

   --------------------
   -- Configure_Hull --
   --------------------

   procedure Configure_Hull
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Athena.Handles.Hull.Create
        (Tag           => Config.Config_Name,
         Streamlined   => Config.Get ("streamlined"),
         Mass_Fraction => Get ("mass"),
         Hull_Points   => Get ("hull_points"),
         Cost          => Get ("cost"),
         Comfort       => Get ("comfort"),
         Armor_Tonnage => Get ("armor_tonnage"));
   end Configure_Hull;

   --------------------------
   -- Configure_Jump_Drive --
   --------------------------

   procedure Configure_Jump_Drive
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Athena.Handles.Component.Jump_Drive.Create_Jump_Drive
        (Tag             => Config.Config_Name,
         Tonnage         => Get ("tonnage"),
         Mass            => Get ("mass"),
         Price           => Athena.Money.To_Price (Get ("price")),
         Idle_Power      => Get ("power"),
         Jump_Power      => Get ("active-power"),
         Fuel            => Get ("fuel"),
         Jump            => Get ("jump"));
   end Configure_Jump_Drive;

   --------------------------
   -- Configure_Jump_Drive --
   --------------------------

   --  procedure Configure_Jump_Drive
   --    (Config : Tropos.Configuration)
   --  is
   --
   --     function Get (Name : String) return Real
   --     is (Get_Fraction (Config, Name));
   --
   --  begin
   --     Athena.Db.Jump_Drive.Create
   --       (Minimum_Tonnage => Get_Value (Config, "minimum_size"),
   --        Power_Per_Ton   => Get ("power_per_ton"),
   --        Price_Per_Ton   => Athena.Money.To_Price (Get ("cost")),
   --        Tag             => Config.Config_Name,
   --        Hull_Fraction   => Get ("hull"),
   --        Jump            => Get ("jump"));
   --  end Configure_Jump_Drive;

   ------------------------------
   -- Configure_Maneuver_Drive --
   ------------------------------

   procedure Configure_Maneuver_Drive
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Athena.Handles.Component.Maneuver.Create_Maneuver_Drive
        (Tag             => Config.Config_Name,
         Tonnage         => Get ("tonnage"),
         Mass            => Get ("mass"),
         Price           => Athena.Money.To_Price (Get ("price")),
         Idle_Power      => Get ("power"),
         Active_Power    => Get ("full-power"),
         Fuel            => Get ("fuel"),
         Impulse         => Get ("impulse"));
   end Configure_Maneuver_Drive;

   ---------------------
   -- Configure_Power --
   ---------------------

   procedure Configure_Power
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

   begin
      Athena.Handles.Component.Power.Create
        (Tag               => Config.Config_Name,
         Tonnage           => Get ("tonnage"),
         Mass              => Get ("mass"),
         Price             => Athena.Money.To_Price (Get ("price")),
         Fuel_Per_Day      => Get ("fuel"),
         Power_Output      => Get ("power-output"));
   end Configure_Power;

   ------------------------
   -- Configure_Quarters --
   ------------------------

   procedure Configure_Quarters
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Tonnage   : constant Non_Negative_Real := Get ("tonnage");
   begin
      Athena.Handles.Component.Quarters.Create
        (Tag               => Config.Config_Name,
         Tonnage           => Tonnage,
         Mass              => Get ("mass"),
         Power             => Get ("power"),
         Price             => Athena.Money.To_Price (Get ("price")),
         Comfort           => Get ("comfort"),
         Occupancy         => Get ("occupancy"),
         Max_Occupancy     => Get ("max-occupancy"));
   end Configure_Quarters;

   ----------------------
   -- Configure_Sensor --
   ----------------------

   --  procedure Configure_Sensor
   --    (Config : Tropos.Configuration)
   --  is
   --     function Get (Name : String) return Real
   --     is (Get_Value (Config, Name));
   --
   --     Tonnage : constant Non_Negative_Real := Get ("tonnage");
   --  begin
   --     Athena.Db.Sensor.Create
   --       (Minimum_Tonnage => Get_Value (Config, "minimum_size"),
   --        Power_Per_Ton   => Get ("power") / Real'Max (Tonnage, 1.0),
   --        Price_Per_Ton   => Athena.Money.To_Price (Get ("price")
   --          / Real'Max (Tonnage, 1.0)),
   --        Tag             => Config.Config_Name,
   --        Tonnage         => Tonnage,
   --        Modifier        => Config.Get ("modifier"));
   --  end Configure_Sensor;

   ---------------------
   -- Configure_Ships --
   ---------------------

   procedure Configure_Ships is

      procedure Configure
        (Category_Name : String;
         Extension     : String;
         Process       : not null access
           procedure (Config : Tropos.Configuration));

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Category_Name : String;
         Extension     : String;
         Process       : not null access
           procedure (Config : Tropos.Configuration))
      is
      begin
         Tropos.Reader.Read_Config
           (Path      =>
              Athena.Paths.Config_File ("ships/" & Category_Name),
            Extension => Extension,
            Configure => Process);
      end Configure;

   begin
      Configure ("hulls", "hull", Configure_Hull'Access);
      Configure ("armor", "armor", Configure_Armor'Access);
      Configure ("engines", "engine", Configure_Maneuver_Drive'Access);
      Configure ("jump-drives", "jump", Configure_Jump_Drive'Access);
      Configure ("power", "power", Configure_Power'Access);
      Configure ("bridges", "bridge", Configure_Bridge'Access);
      Configure ("computers", "computer", Configure_Computer'Access);
      Configure ("quarters", "quarters", Configure_Quarters'Access);
      --  Configure ("sensors", "sensor", Configure_Sensor'Access);
      Configure ("weapons", "weapon", Configure_Weapon'Access);
      Configure ("weapon-mounts", "mount", Configure_Weapon_Mount'Access);

      Configure ("designs", "design", Configure_Design'Access);

   end Configure_Ships;

   ----------------------
   -- Configure_Weapon --
   ----------------------

   procedure Configure_Weapon
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Tonnage   : constant Non_Negative_Real := Get ("tonnage");

   begin
      if Config.Get ("cannon") then
         Athena.Handles.Component.Weapon.Create_Cannon
           (Tag               => Config.Config_Name,
            Tonnage           => Tonnage,
            Mass              => Get ("mass"),
            Power             => Get ("power"),
            Price             => Athena.Money.To_Price (Get ("price")),
            Max_Range         => Get ("max-range"),
            Damage            => Get ("damage"));
      end if;
   end Configure_Weapon;

   ----------------------------
   -- Configure_Weapon_Mount --
   ----------------------------

   procedure Configure_Weapon_Mount
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Tonnage   : constant Non_Negative_Real := Get ("tonnage");

      Concealment : constant Tropos.Configuration :=
                      Config.Child ("concealable");
   begin
      Athena.Handles.Component.Weapon_Mount.Create
        (Tag               => Config.Config_Name,
         Tonnage           => Tonnage,
         Mass              => Get ("mass"),
         Power             => Get ("power"),
         Price             => Athena.Money.To_Price (Get ("price")),
         Concealed_Tonnage => Get_Value (Concealment, "tonnage"),
         Concealed_Mass    => Get_Value (Concealment, "mass"),
         Hard_Points       => Config.Get ("hardpoints", 1),
         Firm_Points       => Config.Get ("firmpoints", 1),
         Weapon_Count      => Config.Get ("weapons"));
   end Configure_Weapon_Mount;

end Athena.Configure.Ships;
