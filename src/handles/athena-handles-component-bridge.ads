with Athena.Money;

package Athena.Handles.Component.Bridge is

   procedure Create
     (Tag               : String;
      Tonnage           : Non_Negative_Real;
      Mass              : Non_Negative_Real;
      Power             : Non_Negative_Real;
      Price             : Athena.Money.Price_Type;
      Ship_Tonnage_Low  : Non_Negative_Real;
      Ship_Tonnage_High : Non_Negative_Real);

end Athena.Handles.Component.Bridge;
