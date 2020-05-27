package body Athena.Handles.Component.Power is

   type Power_Component_Record is
     new Root_Component_Record with
      record
         Power_Output : Non_Negative_Real;
      end record;

   overriding function Power_Output
     (Component : Power_Component_Record)
      return Non_Negative_Real
   is (Component.Power_Output);

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag               : String;
      Tonnage           : Non_Negative_Real;
      Mass              : Non_Negative_Real;
      Price             : Athena.Money.Price_Type;
      Fuel_Per_Day      : Non_Negative_Real;
      Power_Output      : Non_Negative_Real)
   is
   begin
      Add_Component
        (Power_Component_Record'
           (Identifier        => Next_Identifier,
            Tag               => +Tag,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => Fuel_Per_Day,
            Idle_Power        => 0.0,
            Active_Power      => 0.0,
            Power_Output      => Power_Output));
   end Create;

end Athena.Handles.Component.Power;
