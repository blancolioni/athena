with Athena.Money;

package Athena.Handles.Component.Computer is

   procedure Create
     (Tag      : String;
      Tonnage  : Non_Negative_Real;
      Price    : Athena.Money.Price_Type;
      Capacity : Positive);

end Athena.Handles.Component.Computer;
