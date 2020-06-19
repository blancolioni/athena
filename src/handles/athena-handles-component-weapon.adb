package body Athena.Handles.Component.Weapon is

   type Weapon_Component_Record is
     new Root_Component_Record with
      record
         Category : Weapon_Category;
         Max_Range : Non_Negative_Real;
         Damage    : Non_Negative_Real;
      end record;

   overriding function Is_Weapon
     (Component : Weapon_Component_Record)
      return Boolean
   is (True);

   -----------------
   -- Create_Beam --
   -----------------

   procedure Create_Beam
     (Tag       : String;
      Tonnage   : Non_Negative_Real;
      Mass      : Non_Negative_Real;
      Power     : Non_Negative_Real;
      Price     : Athena.Money.Price_Type;
      Max_Range : Non_Negative_Real;
      Damage    : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Weapon_Component_Record'
           (Identifier        => Next_Identifier,
            Tag               => +Tag,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => 0.0,
            Idle_Power        => Power,
            Active_Power      => Power,
            Berths            => 0.0,
            Category          => Beam,
            Max_Range         => Max_Range,
            Damage            => Damage));
   end Create_Beam;

   -------------------
   -- Create_Cannon --
   -------------------

   procedure Create_Cannon
     (Tag       : String;
      Tonnage   : Non_Negative_Real;
      Mass      : Non_Negative_Real;
      Power     : Non_Negative_Real;
      Price     : Athena.Money.Price_Type;
      Max_Range : Non_Negative_Real;
      Damage    : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Weapon_Component_Record'
           (Identifier        => Next_Identifier,
            Tag               => +Tag,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => 0.0,
            Idle_Power        => Power,
            Active_Power      => Power,
            Berths            => 0.0,
            Category          => Cannon,
            Max_Range         => Max_Range,
            Damage            => Damage));
   end Create_Cannon;

end Athena.Handles.Component.Weapon;
