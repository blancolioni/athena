with Minerva.Empire;
with Minerva.Has_Movement;
with Minerva.Fleet;
with Minerva.Ship;
with Minerva.Star;

with Minerva.Db;

package Athena.Ships is

   subtype Ship_Class is Minerva.Ship.Ship_Class;
   subtype Fleet_Class is Minerva.Fleet.Fleet_Class;

   function Dry_Mass (Ship : Ship_Class) return Non_Negative_Real;
   function Current_Mass (Ship : Ship_Class) return Non_Negative_Real;
   function Weapon_Mass (Ship : Ship_Class) return Non_Negative_Real;
   function Drive_Mass (Ship : Ship_Class) return Non_Negative_Real;

   function Total_Cargo_Space (Ship : Ship_Class) return Non_Negative_Real;
   function Available_Cargo_Space (Ship : Ship_Class) return Non_Negative_Real;
   function Current_Cargo
     (Ship : Ship_Class;
      Cargo : Minerva.Db.Cargo_Type)
      return Non_Negative_Real;

   procedure Add_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
     with Pre => Quantity <= Available_Cargo_Space (Ship);

   procedure Remove_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
     with Pre => Quantity <= Current_Cargo (Ship, Cargo);

   function Maximum_Speed (Ship : Ship_Class) return Non_Negative_Real;
   function Maximum_Speed (Fleet : Fleet_Class) return Non_Negative_Real;

   function Maximum_Shields (Ship : Ship_Class) return Non_Negative_Real;
   function Is_Idle (Ship : Ship_Class) return Boolean;
   function Has_Orders (Ship : Ship_Class) return Boolean;

   function Has_Star_Location
     (Located : Minerva.Has_Movement.Has_Movement_Class)
      return Boolean;

   function Has_Star_Location
     (Located : Minerva.Has_Movement.Has_Movement_Class;
      Star : Minerva.Star.Star_Class)
      return Boolean;

   function Has_Destination
     (Located : Minerva.Has_Movement.Has_Movement_Class)
      return Boolean;

   function Has_Destination
     (Located : Minerva.Has_Movement.Has_Movement_Class;
      Star    : Minerva.Star.Star_Class)
      return Boolean;

   function Is_Armed (Ship : Ship_Class) return Boolean;

   procedure On_Arrival (Ship : Ship_Class);
   procedure Next_Order (Ship : Ship_Class);

   procedure On_Arrival (Fleet : Fleet_Class);

   procedure Log_Ship
     (Ship    : Ship_Class;
      Message : String);

   procedure Log_Fleet
     (Fleet   : Fleet_Class;
      Message : String);

   function New_Ship_Name
     (Empire : Minerva.Empire.Empire_Class;
      Base   : String)
      return String;

   function New_Fleet_Name
     (Empire : Minerva.Empire.Empire_Class;
      Base   : String)
      return String;

end Athena.Ships;
