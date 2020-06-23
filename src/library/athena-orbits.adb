with Ada.Numerics;

with Athena.Elementary_Functions;

with Athena.Constants;

package body Athena.Orbits is

   -----------------------------
   -- Current_Global_Position --
   -----------------------------

   function Current_Global_Position
     (Item : Orbiting_Interface'Class) return Orbital_Point
   is
   begin
      return Item.Global_Position (Athena.Calendar.Clock);
   end Current_Global_Position;

   ----------------------------
   -- Current_Local_Position --
   ----------------------------

   function Current_Local_Position
     (Item : Orbiting_Interface'Class) return Orbital_Point
   is
   begin
      return Item.Local_Position (Athena.Calendar.Clock);
   end Current_Local_Position;

   ---------------------
   -- Global_Position --
   ---------------------

   function Global_Position
     (Item  : Orbiting_Interface'Class;
      Clock : Athena.Calendar.Time)
      return Orbital_Point
   is
      use type Athena.Real_Arrays.Real_Vector;
      Primary : constant Massive_Interface'Class := Item.Primary;
   begin
      if Primary in Orbiting_Interface'Class then
         return Orbiting_Interface'Class (Primary).Global_Position (Clock)
           + Item.Local_Position (Clock);
      else
         return Item.Local_Position (Clock);
      end if;
   end Global_Position;

   --------------------
   -- Local_Position --
   --------------------

   function Local_Position
     (Item  : Orbiting_Interface'Class;
      Clock : Athena.Calendar.Time)
      return Orbital_Point
   is
      use Athena.Trigonometry;
      use type Athena.Calendar.Time;
      D : constant Duration := Clock - Athena.Calendar.Start;
      A : constant Angle :=
            Item.Zero_Time_Angle
              + From_Degrees (360.0 * Real (D) / Item.Period);
      R : constant Non_Negative_Real := Item.Semimajor_Axis;
   begin
      return (1 => R * Cos (A),
              2 => R * Sin (A),
              3 => 0.0);
   end Local_Position;

   ------------
   -- Period --
   ------------

   function Period
     (Item : Orbiting_Interface'Class)
      return Non_Negative_Real
   is
      use Athena.Elementary_Functions;
      Mu : constant Non_Negative_Real :=
             Athena.Constants.Gravitational_Constant * Item.Primary.Mass;
   begin
      return 2.0 * Ada.Numerics.Pi * Sqrt (Item.Semimajor_Axis ** 3 / Mu);
   end Period;

end Athena.Orbits;
