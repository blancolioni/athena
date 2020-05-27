package body Athena.Handles.Component.Maneuver is

   type Maneuver_Drive_Component_Record is
     new Root_Component_Record with
      record
         Impulse : Non_Negative_Real;
      end record;

   overriding function Impulse
     (Component : Maneuver_Drive_Component_Record)
      return Non_Negative_Real
   is (Component.Impulse);

   ---------------------------
   -- Create_Maneuver_Drive --
   ---------------------------

   procedure Create_Maneuver_Drive
     (Tag          : String;
      Tonnage      : Non_Negative_Real;
      Mass         : Non_Negative_Real;
      Price        : Athena.Money.Price_Type;
      Idle_Power   : Non_Negative_Real;
      Active_Power : Non_Negative_Real;
      Fuel         : Non_Negative_Real;
      Impulse      : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Maneuver_Drive_Component_Record'
           (Tag               => +Tag,
            Identifier        => Next_Identifier,
            Tonnage           => Tonnage,
            Mass              => Mass,
            Price             => Price,
            Fuel_Consumption  => Fuel,
            Idle_Power        => Idle_Power,
            Active_Power      => Active_Power,
            Impulse           => Impulse));
   end Create_Maneuver_Drive;

end Athena.Handles.Component.Maneuver;
