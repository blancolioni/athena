package Athena.Handles.Component.Quarters is

   procedure Create
     (Tag           : String;
      Tonnage       : Non_Negative_Real;
      Mass          : Non_Negative_Real;
      Power         : Non_Negative_Real;
      Price         : Athena.Money.Price_Type;
      Comfort       : Non_Negative_Real;
      Occupancy     : Non_Negative_Real;
      Max_Occupancy : Non_Negative_Real);

end Athena.Handles.Component.Quarters;
