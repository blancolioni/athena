with Athena.Money;

with Minerva.Colony;
with Minerva.Empire;
with Minerva.Manager;
with Minerva.Ship_Design;
with Minerva.Star;
with Minerva.Technology;

package Athena.Empires is

   function Current_Tec_Level
     (Empire     : Minerva.Empire.Empire_Class;
      Technology : Minerva.Technology.Technology_Class)
      return Non_Negative_Real;

   procedure Add_Investment
     (Empire     : Minerva.Empire.Empire_Class;
      Technology : Minerva.Technology.Technology_Class;
      Construct  : Non_Negative_Real);

   procedure Pay
     (Empire      : Minerva.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String);

   procedure Earn
     (Empire      : Minerva.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String);

   function Capital
     (Of_Empire : Minerva.Empire.Empire_Class)
      return Minerva.Star.Star_Class;

   function Capital
     (Of_Empire : Minerva.Empire.Empire_Class)
      return Minerva.Colony.Colony_Handle;

   procedure Enable_Manager
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class);

   procedure Disable_Manager
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class);

   procedure Set_Manager_Script
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class;
      Script  : String);

   procedure Visit
     (Empire : Minerva.Empire.Empire_Class;
      Star   : Minerva.Star.Star_Class);

   function Visited
     (Empire : Minerva.Empire.Empire_Class;
      Star   : Minerva.Star.Star_Class)
      return Boolean;

   function Standard_Battleship_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Carrier_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Cruiser_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Defender_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Destroyer_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Recon_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Scout_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

   function Standard_Transport_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class;

end Athena.Empires;
