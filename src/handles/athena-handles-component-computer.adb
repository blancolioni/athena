package body Athena.Handles.Component.Computer is

   type Computer_Component_Record is
     new Root_Component_Record with
      record
         Capacity : Positive;
      end record;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag      : String;
      Tonnage  : Non_Negative_Real;
      Price    : Athena.Money.Price_Type;
      Capacity : Positive)
   is
   begin
      Add_Component
        (Component => Computer_Component_Record'
           (Tag               => +Tag,
            Identifier        => Next_Identifier,
            Tonnage           => Tonnage,
            Price             => Price,
            Fuel_Consumption  => 0.0,
            Power_Consumption => 0.0,
            Capacity          => Capacity));
   end Create;

end Athena.Handles.Component.Computer;
