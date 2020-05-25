with Athena.Money;

package Athena.Handles.Component.Computer is

   type Computer_Handle is
     new Component_Handle with private;

   procedure Create
     (Tag      : String;
      Tonnage  : Non_Negative_Real;
      Price    : Athena.Money.Price_Type;
      Capacity : Positive);

private

   type Computer_Handle is
     new Component_Handle with null record;

end Athena.Handles.Component.Computer;
