with Minerva.Cargo_Component;
with Minerva.Design_Component;

with Minerva.Db;

package body Athena.Ships.Designs is

   -----------------
   -- Cargo_Space --
   -----------------

   function Cargo_Space
     (Design    : Ship_Design_Class;
      Tec_Level : Non_Negative_Real)
      return Non_Negative_Real
   is
      Space : Non_Negative_Real := 0.0;
   begin
      for Component of
        Minerva.Cargo_Component.Select_By_Ship_Design
          (Design)
      loop
         Space := Space + 1.0 * Tec_Level
           * (Component.Mass + (Component.Mass ** 2) / 20.0);
      end loop;
      return Space;
   end Cargo_Space;

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass (Design : Ship_Design_Class) return Non_Negative_Real is
   begin
      return Mass : Non_Negative_Real := 0.0 do
         for Component of
           Minerva.Design_Component.Select_By_Ship_Design
             (Design)
         loop
            Mass := Mass + Component.Mass;
         end loop;
      end return;
   end Dry_Mass;

   -------------------
   -- Maximum_Speed --
   -------------------

   function Maximum_Speed
     (Design    : Ship_Design_Class;
      Tec_Level : Non_Negative_Real)
      return Non_Negative_Real
   is
      use type Minerva.Db.Record_Type;
      Mass  : constant Non_Negative_Real := Dry_Mass (Design);
      Speed : Non_Negative_Real := 0.0;
   begin
      for Component of
        Minerva.Design_Component.Select_By_Ship_Design
          (Design)
      loop
         if Component.Top_Record = Minerva.Db.R_Drive_Component then
            Speed := Speed + 20.0 * 1.0 * Component.Mass * Tec_Level;
         end if;
      end loop;
      return Speed / Mass;
   end Maximum_Speed;

end Athena.Ships.Designs;
