with Athena.Money;

with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.Ship;
with Athena.Handles.World;

package Athena.Ships is

   subtype Ship_Handle_Class is Athena.Handles.Ship.Ship_Handle'Class;
   subtype Fleet_Handle_Class is Athena.Handles.Fleet.Fleet_Handle'Class;

   function Tonnage
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Mass
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Idle_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Drive_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Jump_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Available_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Is_Armed
     (Ship : Ship_Handle_Class)
      return Boolean;

   function Get_Maintenance_Cost
     (Ship : Ship_Handle_Class)
      return Athena.Money.Money_Type;

   function Get_Jump_Speed
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   function Get_Impulse_Speed
     (Fleet : Fleet_Handle_Class)
      return Non_Negative_Real;

   function Get_Jump_Speed
     (Fleet : Fleet_Handle_Class)
      return Non_Negative_Real;

   function Get_Impulse_Speed
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   procedure On_Arrival
     (Ship : Ship_Handle_Class);

   procedure Add_Refuel_Action
     (Ship : Ship_Handle_Class);

   function New_Name
     (Empire    : Athena.Handles.Empire.Empire_Handle;
      Base_Name : String)
      return String;

end Athena.Ships;
