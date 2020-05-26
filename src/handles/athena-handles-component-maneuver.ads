with Athena.Money;

package Athena.Handles.Component.Maneuver is

   procedure Create_Maneuver_Drive
     (Tag     : String;
      Tonnage : Non_Negative_Real;
      Price   : Athena.Money.Price_Type;
      Power   : Non_Negative_Real;
      Fuel    : Non_Negative_Real;
      Impulse : Non_Negative_Real);

end Athena.Handles.Component.Maneuver;
