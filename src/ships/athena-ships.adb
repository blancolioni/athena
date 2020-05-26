with Athena.Handles.Module;

package body Athena.Ships is

   ---------------------------
   -- Available_Cargo_Space --
   ---------------------------

   function Available_Cargo_Space
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      return Total_Cargo_Space (Ship);
   end Available_Cargo_Space;

   ---------------------
   -- Available_Power --
   ---------------------

   function Available_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      if Ship.Power_Module.Has_Element then
         return Ship.Power_Module.Component.Power_Output
           * Ship.Power_Module.Condition;
      else
         return 0.0;
      end if;
   end Available_Power;

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
            Total := Total + Drive.Component.Power_Consumption;
         end if;
      end Add_Power;

   begin
      Ship.Iterate_Maneuver_Drives (Add_Power'Access);
      return Total;
   end Drive_Power;

   --------------------
   -- Get_Jump_Speed --
   --------------------

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
        / Tonnage (Ship)
        * 100.0;
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
           / Tonnage (Ship)
           * 400.0;
      else
         return 0.0;
      end if;
   end Get_Jump_Speed;

   ----------------
   -- Idle_Power --
   ----------------

   function Idle_Power
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Design.Tonnage * 0.2;
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
         return Ship.Jump_Drive.Component.Power_Consumption;
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

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival
     (Ship : Ship_Handle_Class)
   is
   begin
      Ship.Owner.Knowledge.Visit (Ship.Star_Location);
      if Ship.Has_Manager then
         Ship.Owner.Send_Signal (Ship.Manager);
      end if;
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

   -----------------------
   -- Total_Cargo_Space --
   -----------------------

   function Total_Cargo_Space
     (Ship : Ship_Handle_Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Design.Cargo_Space;
   end Total_Cargo_Space;

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
