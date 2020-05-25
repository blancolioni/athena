with Athena.Money;

package Athena.Handles.Component.Power is

   procedure Create
     (Tag               : String;
      Tonnage           : Non_Negative_Real;
      Price             : Athena.Money.Price_Type;
      Fuel_Per_Day      : Non_Negative_Real;
      Power_Output      : Non_Negative_Real);

end Athena.Handles.Component.Power;
