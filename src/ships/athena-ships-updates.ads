package Athena.Ships.Updates is

   procedure Update_Fleets
     (Turn_Delta : Unit_Real);

   procedure Update_Ships
     (Turn_Delta : Unit_Real);

   procedure Update_Experience
     (Ship : Ship_Class;
      XP   : Non_Negative_Real);

end Athena.Ships.Updates;
