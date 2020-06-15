package Athena.Handles.Component.Weapon_Mount is

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
      Weapon_Count      : Positive);

end Athena.Handles.Component.Weapon_Mount;
