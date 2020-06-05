with Athena.Money;

with Athena.Handles.Ship;

package Athena.Ships is

   subtype Ship_Handle_Class is Athena.Handles.Ship.Ship_Handle'Class;

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
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real;

   procedure On_Arrival
     (Ship : Ship_Handle_Class);

   procedure Add_Refuel_Action
     (Ship : Ship_Handle_Class);

   --  function Get_Design
   --    (Name : String)
   --     return Athena.Handles.Design.Design_Handle;

end Athena.Ships;
