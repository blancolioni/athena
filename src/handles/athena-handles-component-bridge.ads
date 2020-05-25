with Athena.Money;

package Athena.Handles.Component.Bridge is

   type Bridge_Handle is
     new Component_Handle with private;

   procedure Create
     (Tag               : String;
      Tonnage           : Non_Negative_Real;
      Price             : Athena.Money.Price_Type;
      Ship_Tonnage_Low  : Non_Negative_Real;
      Ship_Tonnage_High : Non_Negative_Real);

private

   type Bridge_Handle is
     new Component_Handle with null record;

end Athena.Handles.Component.Bridge;
