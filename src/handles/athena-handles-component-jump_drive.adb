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
     (Tag     : String;
      Tonnage : Non_Negative_Real;
      Price   : Athena.Money.Price_Type;
      Power   : Non_Negative_Real;
      Fuel    : Non_Negative_Real;
      Jump    : Non_Negative_Real)
   is
   begin
      Add_Component
        (Component => Jump_Drive_Component_Record'
           (Tag               => +Tag,
            Identifier        => Next_Identifier,
            Tonnage           => Tonnage,
            Price             => Price,
            Fuel_Consumption  => Fuel,
            Power_Consumption => Power,
            Jump              => Jump));
   end Create_Jump_Drive;

end Athena.Handles.Component.Jump_Drive;
