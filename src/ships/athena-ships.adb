with Ada.Characters.Handling;

with WL.Numerics.Roman;

with Athena.Elementary_Functions;
with Athena.Logging;

with Athena.Ships.Designs;

with Minerva.Design_Component;
with Minerva.Ship_Module;
with Minerva.Weapon_Component;

package body Athena.Ships is

   function Nice_Name
     (Base  : String;
      Valid : not null access
        function (S : String) return Boolean)
      return String;

   ---------------
   -- Add_Cargo --
   ---------------

   procedure Add_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
   is
      use all type Minerva.Db.Cargo_Type;
      New_Col : constant Non_Negative_Real :=
                  (if Cargo = Colonists
                   then Ship.Colonists + Quantity
                   else Ship.Colonists);
      New_Ind : constant Non_Negative_Real :=
                  (if Cargo = Industry
                   then Ship.Industry + Quantity
                   else Ship.Industry);
      New_Mat : constant Non_Negative_Real :=
                  (if Cargo = Material
                   then Ship.Material + Quantity
                   else Ship.Material);
   begin
      Ship.Update_Ship
        .Set_Colonists (New_Col)
        .Set_Industry (New_Ind)
        .Set_Material (New_Mat)
        .Done;
   end Add_Cargo;

   ---------------------------
   -- Available_Cargo_Space --
   ---------------------------

   function Available_Cargo_Space
     (Ship : Ship_Class)
      return Non_Negative_Real
   is
   begin
      return Total_Cargo_Space (Ship)
        - Ship.Colonists - Ship.Industry - Ship.Material;
   end Available_Cargo_Space;

   -------------------
   -- Current_Cargo --
   -------------------

   function Current_Cargo
     (Ship  : Ship_Class;
      Cargo : Minerva.Db.Cargo_Type)
      return Non_Negative_Real
   is
      use all type Minerva.Db.Cargo_Type;
   begin
      case Cargo is
         when Colonists =>
            return Ship.Colonists;
         when Industry =>
            return Ship.Industry;
         when Material =>
            return Ship.Material;
      end case;
   end Current_Cargo;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass (Ship : Ship_Class) return Non_Negative_Real is
   begin
      return Dry_Mass (Ship) + Ship.Colonists + Ship.Industry + Ship.Material;
   end Current_Mass;

   ----------------
   -- Drive_Mass --
   ----------------

   function Drive_Mass (Ship : Ship_Class) return Non_Negative_Real is
      use type Minerva.Db.Record_Type;
      Mass  : Non_Negative_Real := 0.0;
   begin
      for Module of
        Minerva.Ship_Module.Select_By_Ship (Ship)
      loop
         declare
            use Minerva.Design_Component;
            Component : constant Design_Component_Class :=
                          Module.Design_Component;
         begin
            if Component.Top_Record = Minerva.Db.R_Drive_Component then
               Mass := Mass + Component.Mass;
            end if;
         end;
      end loop;
      return Mass;
   end Drive_Mass;

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass (Ship : Ship_Class) return Non_Negative_Real is
   begin
      return Athena.Ships.Designs.Dry_Mass (Ship.Ship_Design);
   end Dry_Mass;

   ---------------------
   -- Has_Destination --
   ---------------------

   function Has_Destination
     (Located : Minerva.Has_Movement.Has_Movement_Class)
      return Boolean
   is
   begin
      return Located.Destination.Has_Element;
   end Has_Destination;

   ---------------------
   -- Has_Destination --
   ---------------------

   function Has_Destination
     (Located : Minerva.Has_Movement.Has_Movement_Class;
      Star    : Minerva.Star.Star_Class)
      return Boolean
   is
   begin
      return Has_Destination (Located)
        and then Located.Destination.Identifier = Star.Identifier;
   end Has_Destination;

   ----------------
   -- Has_Orders --
   ----------------

   function Has_Orders (Ship : Ship_Class) return Boolean is
   begin
      return Ship.Last_Order >= Ship.First_Order;
   end Has_Orders;

   -----------------------
   -- Has_Star_Location --
   -----------------------

   function Has_Star_Location
     (Located : Minerva.Has_Movement.Has_Movement_Class)
      return Boolean is
   begin
      return Located.Star.Has_Element
        and then (not Located.Destination.Has_Element
                  or else Located.Progress = 0.0);
   end Has_Star_Location;

   -----------------------
   -- Has_Star_Location --
   -----------------------

   function Has_Star_Location
     (Located : Minerva.Has_Movement.Has_Movement_Class;
      Star : Minerva.Star.Star_Class)
      return Boolean
   is
   begin
      return Has_Star_Location (Located)
        and then Located.Star.Identifier = Star.Identifier;
   end Has_Star_Location;

   --------------
   -- Is_Armed --
   --------------

   function Is_Armed (Ship : Ship_Class) return Boolean is
   begin
      return Minerva.Weapon_Component.First_By_Ship_Design
        (Ship.Ship_Design)
        .Has_Element;
   end Is_Armed;

   -------------
   -- Is_Idle --
   -------------

   function Is_Idle (Ship : Ship_Class) return Boolean is
   begin
      return not Has_Destination (Ship) and then not Has_Orders (Ship);
   end Is_Idle;

   --------------
   -- Log_Ship --
   --------------

   procedure Log_Fleet
     (Fleet : Fleet_Class;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Fleet.Empire.Name
         & "/" & Fleet.Identifier & " " & Fleet.Name
         & ": " & Message);
   end Log_Fleet;

   --------------
   -- Log_Ship --
   --------------

   procedure Log_Ship
     (Ship    : Ship_Class;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Ship.Empire.Name
         & "/" & Ship.Identifier & " " & Ship.Name
         & ": " & Message);
   end Log_Ship;

   ---------------------
   -- Maximum_Shields --
   ---------------------

   function Maximum_Shields (Ship : Ship_Class) return Non_Negative_Real is
      use Athena.Elementary_Functions;
      Max : Non_Negative_Real := 0.0;
      Mass : constant Non_Negative_Real := Current_Mass (Ship);
      M    : constant Non_Negative_Real :=
               Mass ** (1.0 / 3.0);
   begin
      for Module of
        Minerva.Ship_Module.Select_By_Ship (Ship)
      loop
         declare
            use type Minerva.Db.Record_Type;
            use Minerva.Design_Component;
            Component : constant Design_Component_Class :=
                          Module.Design_Component;
         begin
            if Component.Top_Record = Minerva.Db.R_Shield_Component then
               declare
                  This_Shield : constant Non_Negative_Real :=
                                  Component.Mass * Module.Condition
                                    * Module.Tec_Level / M * 20.0;
               begin
                  Max := Max + This_Shield;
               end;
            end if;
         end;
      end loop;
      return Max;
   end Maximum_Shields;

   -------------------
   -- Maximum_Speed --
   -------------------

   function Maximum_Speed (Ship : Ship_Class) return Non_Negative_Real is
      use type Minerva.Db.Record_Type;
      Mass  : constant Non_Negative_Real := Current_Mass (Ship);
      Speed : Non_Negative_Real := 0.0;
   begin
      pragma Assert (Mass > 0.0);

      for Module of
        Minerva.Ship_Module.Select_By_Ship (Ship)
      loop
         declare
            use Minerva.Design_Component;
            Component : constant Design_Component_Class :=
                          Module.Design_Component;
         begin
            if Component.Top_Record = Minerva.Db.R_Drive_Component then
               Speed := Speed
                 + 20.0 * Module.Condition * Module.Tec_Level * Component.Mass;
            end if;
         end;
      end loop;

      return Speed / Mass;
   end Maximum_Speed;

   -------------------
   -- Maximum_Speed --
   -------------------

   function Maximum_Speed (Fleet : Fleet_Class) return Non_Negative_Real is
   begin
      return Result : Non_Negative_Real := Non_Negative_Real'Last do
         for Ship of Minerva.Ship.Select_By_Fleet (Fleet) loop
            declare
               This_Speed : constant Non_Negative_Real :=
                              Maximum_Speed (Ship);
            begin
               if This_Speed < Result then
                  Result := This_Speed;
               end if;
            end;
         end loop;
      end return;
   end Maximum_Speed;

   --------------------
   -- New_Fleet_Name --
   --------------------

   function New_Fleet_Name
     (Empire : Minerva.Empire.Empire_Class;
      Base   : String)
      return String
   is
      function Valid (S : String) return Boolean
      is (not Minerva.Fleet.First_By_Asset_Empire_Name
          (Empire, S)
          .Has_Element);
   begin
      return Nice_Name (Base, Valid'Access);
   end New_Fleet_Name;

   -------------------
   -- New_Ship_Name --
   -------------------

   function New_Ship_Name
     (Empire : Minerva.Empire.Empire_Class;
      Base   : String)
      return String
   is
      function Valid (S : String) return Boolean
      is (not Minerva.Ship.First_By_Asset_Empire_Name
          (Empire, S)
          .Has_Element);
   begin
      return Nice_Name (Base, Valid'Access);
   end New_Ship_Name;

   ----------------
   -- Next_Order --
   ----------------

   procedure Next_Order (Ship : Ship_Class) is
   begin
      if Ship.First_Order = Ship.Last_Order then
         Ship.Update_Ship
           .Set_First_Order (1)
           .Set_Last_Order (0)
           .Done;
      else
         Ship.Update_Ship
           .Set_First_Order (Ship.First_Order + 1)
           .Done;
      end if;
   end Next_Order;

   ---------------
   -- Nice_Name --
   ---------------

   function Nice_Name
     (Base : String;
      Valid : not null access
        function (S : String) return Boolean)
      return String
   is
      Index : Positive := 1;
      Nice  : String := Base;
      Capitalize : Boolean := True;
   begin
      for Ch of Nice loop
         if Capitalize then
            Ch := Ada.Characters.Handling.To_Upper (Ch);
            Capitalize := False;
         elsif Ch in ' ' | '_' | '-' then
            Ch := ' ';
            Capitalize := True;
         end if;
      end loop;

      loop
         declare
            Name : constant String := Nice & ' '
                     & WL.Numerics.Roman.Roman_Image (Index);
         begin
            if Valid (Name) then
               return Name;
            end if;
         end;
         Index := Index + 1;
      end loop;

   end Nice_Name;

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival (Ship : Ship_Class) is
   begin
      Log_Ship (Ship, "arrives at " & Ship.Destination.Name);
      Ship.Update_Ship
        .Set_Star (Ship.Destination)
        .Set_Destination (Minerva.Star.Empty_Handle)
        .Set_Progress (0.0)
        .Done;
   end On_Arrival;

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival (Fleet : Fleet_Class) is
      Star : constant Minerva.Star.Star_Class := Fleet.Destination;
   begin
      Log_Fleet (Fleet, "arrives at " & Star.Name);
      for Ship of Minerva.Ship.Select_By_Fleet (Fleet) loop
         declare
            Update : constant Minerva.Ship.Ship_Update_Handle'Class :=
                       Ship.Update_Ship;
         begin
            Minerva.Ship.Done (Update.Set_Star (Star));
         end;

         --  Ship.Update_Ship
         --    .Set_Star (Star)
         --    .Done;
      end loop;
      Fleet.Update_Fleet
        .Set_Star (Star)
        .Set_Destination (Minerva.Star.Empty_Handle)
        .Set_Progress (0.0)
        .Done;
   end On_Arrival;

   ------------------
   -- Remove_Cargo --
   ------------------

   procedure Remove_Cargo
     (Ship     : Ship_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
   is
      use all type Minerva.Db.Cargo_Type;
      New_Col : constant Non_Negative_Real :=
                  (if Cargo = Colonists
                   then Ship.Colonists - Quantity
                   else Ship.Colonists);
      New_Ind : constant Non_Negative_Real :=
                  (if Cargo = Industry
                   then Ship.Industry - Quantity
                   else Ship.Industry);
      New_Mat : constant Non_Negative_Real :=
                  (if Cargo = Material
                   then Ship.Material - Quantity
                   else Ship.Material);
   begin
      Ship.Update_Ship
        .Set_Colonists (New_Col)
        .Set_Industry (New_Ind)
        .Set_Material (New_Mat)
        .Done;
   end Remove_Cargo;

   -----------------------
   -- Total_Cargo_Space --
   -----------------------

   function Total_Cargo_Space (Ship : Ship_Class) return Non_Negative_Real is
      use type Minerva.Db.Record_Type;
      Space : Non_Negative_Real := 0.0;
   begin
      for Module of
        Minerva.Ship_Module.Select_By_Ship (Ship)
      loop
         declare
            use Minerva.Design_Component;
            Component : constant Design_Component_Class :=
                          Module.Design_Component;
         begin
            if Component.Top_Record = Minerva.Db.R_Cargo_Component then
               Space := Space + Module.Condition * Module.Tec_Level
                 * (Component.Mass + (Component.Mass ** 2) / 20.0);
            end if;
         end;
      end loop;
      return Space;
   end Total_Cargo_Space;

   -----------------
   -- Weapon_Mass --
   -----------------

   function Weapon_Mass (Ship : Ship_Class) return Non_Negative_Real is
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for Weapon of
           Minerva.Weapon_Component.Select_By_Ship_Design
             (Ship.Ship_Design)
         loop
            Mass := Mass + Weapon.Mass;
         end loop;
      end return;
   end Weapon_Mass;

end Athena.Ships;
