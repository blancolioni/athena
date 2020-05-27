package body Athena.Handles.Component.Quarters is

   type Quarters_Component_Record is
     new Root_Component_Record with
      record
         Comfort       : Non_Negative_Real;
         Occupancy     : Non_Negative_Real;
         Max_Occupancy : Non_Negative_Real;
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
      Comfort           : Non_Negative_Real;
      Occupancy         : Non_Negative_Real;
      Max_Occupancy     : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Quarters_Component_Record'
           (Identifier        => Next_Identifier,
            Tag               => +Tag,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => 0.0,
            Idle_Power        => Power,
            Active_Power      => Power,
            Comfort           => Comfort,
            Occupancy         => Occupancy,
            Berths            => Occupancy,
            Max_Occupancy     => Max_Occupancy));
   end Create;

end Athena.Handles.Component.Quarters;
