package body Athena.Handles.Component.Jump_Drive is

   type Jump_Drive_Component_Record is
     new Root_Component_Record with
      record
         Jump : Non_Negative_Real;
      end record;

   overriding function Jump
     (Rec : Jump_Drive_Component_Record)
      return Non_Negative_Real
   is (Rec.Jump);

   -----------------------
   -- Create_Jump_Drive --
   -----------------------

   procedure Create_Jump_Drive
     (Tag        : String;
      Tonnage    : Non_Negative_Real;
      Mass       : Non_Negative_Real;
      Price      : Athena.Money.Price_Type;
      Idle_Power : Non_Negative_Real;
      Jump_Power : Non_Negative_Real;
      Fuel       : Non_Negative_Real;
      Jump       : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Jump_Drive_Component_Record'
           (Tag               => +Tag,
            Identifier        => Next_Identifier,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => Fuel,
            Idle_Power        => Idle_Power,
            Active_Power      => Jump_Power,
            Berths            => 0.0,
            Jump              => Jump));
   end Create_Jump_Drive;

end Athena.Handles.Component.Jump_Drive;
