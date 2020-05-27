with Athena.Handles.Design_Module;
with Athena.Handles.Module;

package body Athena.Ships is

   ---------------------------
   -- Available_Cargo_Space --
   ---------------------------

   function Available_Cargo_Space
     (Ship  : Ship_Handle_Class;
      Cargo : Athena.Handles.Cargo_Class)
      return Non_Negative_Real
   is
   begin
      return Cargo_Space (Ship, Cargo) - Ship.Current_Cargo (Cargo);
   end Available_Cargo_Space;

   ---------------------
   -- Available_Power --
   ---------------------

   function Available_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
      Power : Non_Negative_Real := 0.0;

      procedure Add_Power (Module : Athena.Handles.Module.Module_Handle);

      ---------------
      -- Add_Power --
      ---------------

      procedure Add_Power (Module : Athena.Handles.Module.Module_Handle) is
      begin
         Power := Power +
           Module.Component.Power_Output * Module.Condition;
      end Add_Power;

   begin
      Ship.Iterate_Power_Modules (Add_Power'Access);
      return Power;
   end Available_Power;

   -----------------
   -- Cargo_Space --
   -----------------

   function Cargo_Space
     (Ship  : Ship_Handle_Class;
      Cargo : Athena.Handles.Cargo_Class)
      return Non_Negative_Real
   is
   begin
      case Cargo is
         when Athena.Handles.Colonists =>
            return Ship.Design.Passenger_Berths;
         when Athena.Handles.Material | Athena.Handles.Industry =>
            return Ship.Design.Free_Space;
      end case;
   end Cargo_Space;

   -------------------
   -- Current_Cargo --
   -------------------

   function Current_Cargo
     (Ship  : Ship_Handle_Class;
      Cargo : Athena.Handles.Cargo_Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Current_Cargo (Cargo);
   end Current_Cargo;

   -----------------
   -- Drive_Power --
   -----------------

   function Drive_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
      Total : Non_Negative_Real := 0.0;

      procedure Add_Power (Drive : Athena.Handles.Module.Module_Handle);

      ---------------
      -- Add_Power --
      ---------------

      procedure Add_Power (Drive : Athena.Handles.Module.Module_Handle) is
      begin
         if Drive.Condition > 0.0 then
            Total := Total + Drive.Component.Active_Power_Consumption;
         end if;
      end Add_Power;

   begin
      Ship.Iterate_Maneuver_Drives (Add_Power'Access);
      return Total;
   end Drive_Power;

   -----------------------
   -- Get_Impulse_Speed --
   -----------------------

   function Get_Impulse_Speed
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
      Total_Impulse : Non_Negative_Real := 0.0;

      procedure Add_Impulse
        (Drive : Athena.Handles.Module.Module_Handle);

      -----------------
      -- Add_Impulse --
      -----------------

      procedure Add_Impulse
        (Drive : Athena.Handles.Module.Module_Handle)
      is
      begin
         Total_Impulse :=
           Total_Impulse + Drive.Component.Impulse * Drive.Condition;
      end Add_Impulse;

   begin
      Ship.Iterate_Maneuver_Drives (Add_Impulse'Access);

      return Total_Impulse
        / Ship.Current_Mass
        * 1.0e4;
   end Get_Impulse_Speed;

   --------------------
   -- Get_Jump_Speed --
   --------------------

   function Get_Jump_Speed
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      if Ship.Jump_Drive.Has_Element then
         return Ship.Jump_Drive.Component.Jump
           * Ship.Jump_Drive.Condition
           / Ship.Current_Mass
           * 1.0e4;
      else
         return 0.0;
      end if;
   end Get_Jump_Speed;

   --------------------------
   -- Get_Maintenance_Cost --
   --------------------------

   function Get_Maintenance_Cost
     (Ship : Ship_Handle_Class)
      return Athena.Money.Money_Type
   is
      use Athena.Money;

      Total : Money_Type := Zero;

      procedure Add_Maintenance
        (Module : Athena.Handles.Design_Module.Design_Module_Handle);

      ---------------------
      -- Add_Maintenance --
      ---------------------

      procedure Add_Maintenance
        (Module : Athena.Handles.Design_Module.Design_Module_Handle)
      is
      begin
         Total := Total
           + To_Money (To_Real (Adjust_Price (Module.Component.Price, 0.1)));
      end Add_Maintenance;

   begin
      Ship.Design.Iterate_Design_Modules
        (Add_Maintenance'Access);
      return Total + To_Money (Ship.Design.Tonnage / 20.0);
   end Get_Maintenance_Cost;

   ----------------
   -- Idle_Power --
   ----------------

   function Idle_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
      Power : Non_Negative_Real := 0.0;

      procedure Add_Power
        (Module : Athena.Handles.Design_Module.Design_Module_Handle);

      ---------------
      -- Add_Power --
      ---------------

      procedure Add_Power
        (Module : Athena.Handles.Design_Module.Design_Module_Handle)
      is
      begin
         Power := Power + Module.Component.Idle_Power_Consumption;
      end Add_Power;

   begin
      Ship.Design.Iterate_Design_Modules
        (Add_Power'Access);
      return Power;
   end Idle_Power;

   --------------
   -- Is_Armed --
   --------------

   function Is_Armed
     (Ship : Ship_Handle_Class)
      return Boolean
   is
      pragma Unreferenced (Ship);
   begin
      return False;
   end Is_Armed;

   ----------------
   -- Jump_Power --
   ----------------

   function Jump_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      if Ship.Jump_Drive.Has_Element
        and then Ship.Jump_Drive.Condition > 0.0
      then
         return Ship.Jump_Drive.Component.Active_Power_Consumption;
      else
         return 0.0;
      end if;
   end Jump_Power;

   ----------------
   -- Load_Cargo --
   ----------------

   procedure Load_Cargo
     (Ship     : Ship_Handle_Class;
      Cargo    : Athena.Handles.Cargo_Class;
      Quantity : Non_Negative_Real)
   is
   begin
      Ship.Set_Current_Cargo
        (Cargo, Ship.Current_Cargo (Cargo) + Quantity);
   end Load_Cargo;

   ----------
   -- Mass --
   ----------

   function Mass
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Current_Mass;
   end Mass;

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival
     (Ship : Ship_Handle_Class)
   is
   begin
      Ship.Owner.Knowledge.Visit (Ship.Star_Location);
   end On_Arrival;

   -------------
   -- Tonnage --
   -------------

   function Tonnage
     (Ship : Ship_Handle_Class) return Non_Negative_Real
   is
   begin
      return Athena.Handles.Ship.Design (Ship).Tonnage;
   end Tonnage;

   ------------------
   -- Unload_Cargo --
   ------------------

   procedure Unload_Cargo
     (Ship     : Ship_Handle_Class;
      Cargo    : Athena.Handles.Cargo_Class;
      Quantity : Non_Negative_Real)
   is
   begin
      pragma Assert (Quantity <= Ship.Current_Cargo (Cargo));
      Ship.Set_Current_Cargo
        (Cargo, Ship.Current_Cargo (Cargo) - Quantity);
   end Unload_Cargo;

end Athena.Ships;
