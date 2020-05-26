package body Athena.Handles.Component.Bridge is

   type Bridge_Component_Record is
     new Root_Component_Record with
      record
         Ship_Tonnage_Low : Non_Negative_Real;
         Ship_Tonnage_High : Non_Negative_Real;
      end record;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag               : String;
      Tonnage           : Non_Negative_Real;
      Price             : Athena.Money.Price_Type;
      Ship_Tonnage_Low  : Non_Negative_Real;
      Ship_Tonnage_High : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Bridge_Component_Record'
           (Identifier        => Next_Identifier,
            Tag               => +Tag,
            Tonnage           => Tonnage,
            Price             => Price,
            Fuel_Consumption  => 0.0,
            Power_Consumption => 0.0,
            Ship_Tonnage_Low  => Ship_Tonnage_Low,
            Ship_Tonnage_High => Ship_Tonnage_High));
   end Create;

end Athena.Handles.Component.Bridge;
