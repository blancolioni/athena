private with Ada.Strings.Unbounded;
private with Athena.Real_Images;

with Athena.Trigonometry;

package Athena.Encounters is

   type Team_Type is (Attacker, Defender);

   type Encounter_Point is
      record
         X, Y : Real;
      end record;

   function Translate
     (Pt   : Encounter_Point;
      X, Y : Real := 0.0)
      return Encounter_Point
   is (Pt.X + X, Pt.Y + Y);

   function Rotate
     (Pt     : Encounter_Point;
      Around : Encounter_Point;
      Angle  : Athena.Trigonometry.Angle)
      return Encounter_Point;

   type Shape_Type is
     (Small_Military_Ship, Large_Military_Ship,
      Small_Civilian_Ship, Large_Civilian_Ship,
      Beam_Weapon, Missile_Weapon, Fighter_Craft,
      Explosion);

   subtype Ship_Shape_Type is
     Shape_Type range Small_Military_Ship .. Large_Civilian_Ship;

   subtype Military_Shape_Type is
     Shape_Type range Small_Military_Ship .. Large_Military_Ship;

   subtype Civilian_Shape_Type is
     Shape_Type range Small_Civilian_Ship .. Large_Civilian_Ship;

   subtype Weapon_Shape_Type is
     Shape_Type range Beam_Weapon .. Fighter_Craft;

   type Encounter_Tick_Count is new Natural;
   subtype Encounter_Tick is
     Encounter_Tick_Count range 1 .. Encounter_Tick_Count'Last;

private

   function "+" (X : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (X : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   Log_Encounter : constant Boolean := True;

end Athena.Encounters;
