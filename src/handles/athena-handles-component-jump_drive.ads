with Athena.Money;

package Athena.Handles.Component.Jump_Drive is

   procedure Create_Jump_Drive
     (Tag        : String;
      Tonnage    : Non_Negative_Real;
      Mass       : Non_Negative_Real;
      Price      : Athena.Money.Price_Type;
      Idle_Power : Non_Negative_Real;
      Jump_Power : Non_Negative_Real;
      Fuel       : Non_Negative_Real;
      Jump       : Non_Negative_Real);

end Athena.Handles.Component.Jump_Drive;
