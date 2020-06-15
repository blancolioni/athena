package Athena.Handles.Component.Weapon is

   type Weapon_Category is (Cannon, Beam, Missile);

   procedure Create_Cannon
     (Tag       : String;
      Tonnage   : Non_Negative_Real;
      Mass      : Non_Negative_Real;
      Power     : Non_Negative_Real;
      Price     : Athena.Money.Price_Type;
      Max_Range : Non_Negative_Real;
      Damage    : Non_Negative_Real);

   procedure Create_Beam
     (Tag       : String;
      Tonnage   : Non_Negative_Real;
      Mass      : Non_Negative_Real;
      Power     : Non_Negative_Real;
      Price     : Athena.Money.Price_Type;
      Max_Range : Non_Negative_Real;
      Damage    : Non_Negative_Real);

end Athena.Handles.Component.Weapon;
