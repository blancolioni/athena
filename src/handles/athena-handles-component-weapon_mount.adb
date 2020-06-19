package body Athena.Handles.Component.Weapon_Mount is

   type Weapon_Mount_Component_Record is
     new Root_Component_Record with
      record
         Concealed_Tonnage : Non_Negative_Real;
         Concealed_Mass    : Non_Negative_Real;
         Hard_Points       : Positive;
         Firm_Points       : Positive;
         Weapon_Count      : Positive;
      end record;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag               : String;
      Tonnage           : Non_Negative_Real;
      Mass              : Non_Negative_Real;
      Power             : Non_Negative_Real;
      Price             : Athena.Money.Price_Type;
      Concealed_Tonnage : Non_Negative_Real;
      Concealed_Mass    : Non_Negative_Real;
      Hard_Points       : Positive;
      Firm_Points       : Positive;
      Weapon_Count      : Positive)
   is
   begin
      Add_Component
        (Component => Weapon_Mount_Component_Record'
           (Identifier        => Next_Identifier,
            Tag               => +Tag,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => 0.0,
            Idle_Power        => Power,
            Active_Power      => Power,
            Berths            => 0.0,
            Concealed_Tonnage => Concealed_Tonnage,
            Concealed_Mass    => Concealed_Mass,
            Hard_Points       => Hard_Points,
            Firm_Points       => Firm_Points,
            Weapon_Count      => Weapon_Count));
   end Create;

end Athena.Handles.Component.Weapon_Mount;
