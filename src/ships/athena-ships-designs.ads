with Minerva.Ship_Design;

package Athena.Ships.Designs is

   subtype Ship_Design_Class is Minerva.Ship_Design.Ship_Design_Class;

   function Dry_Mass
     (Design : Ship_Design_Class)
      return Non_Negative_Real;

   function Maximum_Speed
     (Design     : Ship_Design_Class;
      Tec_Level  : Non_Negative_Real)
      return Non_Negative_Real;

   function Cargo_Space
     (Design     : Ship_Design_Class;
      Tec_Level  : Non_Negative_Real)
      return Non_Negative_Real;

end Athena.Ships.Designs;
