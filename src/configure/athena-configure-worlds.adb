with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.Numerics.Roman;
with WL.Random;

with Athena.Elementary_Functions;
with Athena.Random;
with Athena.Real_Images;
with Athena.Trigonometry;

with Athena.Solar_System;

with Athena.Handles.World;             use Athena.Handles.World;

package body Athena.Configure.Worlds is

   Log_Generation : constant Boolean := False;

   subtype Rocky_World is World_Composition range Ice .. Rock_Iron;

   subtype Planetary_Zone is Stellar_Orbit_Zone range Yellow .. Black;

   type World_Terrain is
     (Ocean, Desert, Ice, Tundra, Mountains);
--     , Grassland, Wetland,
--        Jungle, Forest);

   type Atmospheric_Component is
      record
         Gas : Atmospheric_Gas;
         Partial : Unit_Real;
      end record;

   package Atmospheric_Component_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Atmospheric_Component);

   function More (Left, Right : Atmospheric_Component) return Boolean
   is (Left.Partial > Right.Partial);

   package Atmospheric_Sorting is
     new Atmospheric_Component_Lists.Generic_Sorting (More);

   type Atmosphere is
      record
         List : Atmospheric_Component_Lists.List;
      end record;

   procedure Add_Component
     (Atm     : in out Atmosphere;
      Gas     : Atmospheric_Gas;
      Partial : Unit_Real);

   function Partial
     (Atm : Atmosphere;
      Gas : Atmospheric_Gas)
      return Unit_Real;

   function Get_Zone
     (Star : Athena.Handles.Star.Star_Handle;
      AUs  : Non_Negative_Real)
      return Stellar_Orbit_Zone;

   function D6 return Non_Negative_Real
   is (Athena.Random.Unit_Random * 5.0 + 1.0);

   function D (Count : Positive) return Non_Negative_Real;

   subtype Die_Roll is Integer range 1 .. 6;
   subtype Double_Roll is Integer range 2 .. 12;
   subtype Triple_Roll is Integer range 3 .. 18;

   function D6 return Die_Roll
   is (WL.Random.Random_Number (1, 6));

   function DR return Double_Roll
   is (D6 + D6);

   function TDR return Triple_Roll
   is (D6 + DR);

   procedure Put (Width : Positive;
                  Value : String);

   procedure Put (Width : Positive;
                  Value : Real);

   procedure Put (Width : Positive;
                  Value : Integer);

   procedure Generate_World
     (Star  : Athena.Handles.Star.Star_Handle;
      Index : Positive;
      Zone  : Planetary_Zone;
      Orbit : Non_Negative_Real);

   package Planet_Tables is

      function Random_Planet_Mass
        (Zone : Planetary_Zone)
         return Non_Negative_Real;

      function Composition
        (Mass : Non_Negative_Real;
         Zone : Stellar_Orbit_Zone)
         return World_Composition;

      function Random_Planet_Density
        (Composition : World_Composition;
         Mass        : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Planet_Rotation
        (Mass  : Non_Negative_Real;
         Orbit : Non_Negative_Real;
         Year  : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Planet_Tilt
         return Real;

      function Random_Atmospheric_Class
        (Zone : Stellar_Orbit_Zone;
         Mass : Non_Negative_Real)
         return Atmosphere_Class;

      function Random_Surface_Pressure
        (Class   : Atmosphere_Class;
         Gravity : Non_Negative_Real)
         return Non_Negative_Real;

      function Random_Atmosphere
        (Zone        : Planetary_Zone;
         Composition : Rocky_World)
         return Atmosphere;

   end Planet_Tables;

   -------------------
   -- Planet_Tables --
   -------------------

   package body Planet_Tables is

      type Mass_Parameters is
         record
            Base   : Non_Negative_Real;
            Random : Natural;
         end record;

      function Z return Mass_Parameters is (0.0, 0);
      function K (N : Non_Negative_Real) return Mass_Parameters is (N, 0);
      function R (N : Positive) return Mass_Parameters is (0.0, N);

      type Mass_Table is array (2 .. 12, Planetary_Zone) of Mass_Parameters;
      Planet_Mass : constant Mass_Table :=
                      (2  => (others => Z),
                       3  => (Black => Z, others => K (0.1)),
                       4  => (K (0.1), K (0.2), K (0.2), K (0.1)),
                       5  => (K (0.2), K (0.5), K (0.5), K (0.2)),
                       6  => (K (0.3), K (0.8), K (0.8), K (0.1)),
                       7  => (K (0.5), K (1.0), R (5), K (0.4)),
                       8  => (K (0.8), K (1.2), R (5), K (0.5)),
                       9  => (K (1.0), K (1.5), R (10), R (5)),
                       10 => (K (1.5), K (2.0), R (50), R (10)),
                       11 => (R (1), R (50), R (50), R (50)),
                       12 => (R (50), R (100), R (100), R (100)));

      type Zone_Composition is
        array (Planetary_Zone) of World_Composition;

      type Composition_Parameters is
         record
            Mass        : Non_Negative_Real;
            Composition : Zone_Composition;
         end record;

      Composition_Table : constant array (Positive range <>)
        of Composition_Parameters :=
          ((0.1, (Rock_Iron, Rock, Rock_Ice, Ice)),
           (0.4, (Rock_Iron, Rock_Iron, Rock, Rock_Ice)),
           (0.6, (Rock_Iron, Rock_Iron, Rock_Iron, Rock_Ice)),
           (1.1, (Rock_Iron, Rock_Iron, Rock_Iron, Gaseous)),
           (10.0, (Rock_Iron, Rock_Iron, Gaseous, Gaseous)),
           (50.0, (Gaseous, Gaseous, Gaseous, Gaseous)),
           (100.0, (Gaseous, Gaseous, Hydrogen, Hydrogen)),
           (1000.0, (others => Hydrogen)));

      Density_Table : constant array
        (World_Composition, 1 .. 3, 1 .. 2) of Real :=
                        (Hydrogen => (others => (0.19, 0.21)),
                         Gaseous  => (others => (0.2, 0.3)),
                         Ice      => (others => (0.1, 0.2)),
                         Rock     => (others => (0.6, 0.7)),
                         Rock_Ice => (others => (0.3, 0.5)),
                         Rock_Iron => ((0.5, 0.6), (1.0, 1.6), (1.0, 2.5)));

      -----------------
      -- Composition --
      -----------------

      function Composition
        (Mass : Non_Negative_Real;
         Zone : Stellar_Orbit_Zone)
         return World_Composition
      is
      begin
         for Item of Composition_Table loop
            if Mass < Item.Mass then
               return Item.Composition (Zone);
            end if;
         end loop;
         return Composition_Table (Composition_Table'Last).Composition (Zone);
      end Composition;

      -----------------------
      -- Random_Atmosphere --
      -----------------------

      function Random_Atmosphere
        (Zone        : Planetary_Zone;
         Composition : Rocky_World)
         return Atmosphere
      is
         Atm : Atmosphere;
         type Component_Array is array (Positive range <>) of Atmospheric_Gas;
         Main : constant Component_Array :=
                  (case Composition is
                      when Rock | Rock_Iron =>
                     (case Zone is
                         when Yellow       => (CO2, N2, SO2),
                         when Green | Blue => (CO2, N2, CH4),
                         when Black        => (1 => H2)),
                      when Ice | Rock_Ice   =>
                     (case Zone is
                         when Yellow | Green   =>
                        (raise Constraint_Error with
                           "ice world in yellow or green zone"),
                         when Blue             => (CO2, CH4),
                         when Black            => (1 => H2)));

         Trace : constant Component_Array :=
                   (case Composition is
                       when Rock | Rock_Iron =>
                      (case Zone is
                          when Yellow       => (Ar, Cl2, F2),
                          when Green | Blue => (Ar, NH3, SO2, Cl2, F2),
                          when Black        => (1 => He)),
                       when Ice | Rock_Ice   =>
                      (case Zone is
                          when Yellow | Green   =>
                         (raise Constraint_Error with
                            "ice world in yellow or green zone"),
                          when Blue             => (Ar, N2, NH3),
                          when Black            => (1 => He)));

         Total : Unit_Real := 0.0;

         procedure Add (Components : Component_Array;
                        Count      : Positive;
                        Scale      : Unit_Real);

         procedure Add (Components : Component_Array;
                        Count      : Positive;
                        Scale      : Unit_Real)
         is
         begin
            for Gas of Components loop
               declare
                  Partial : constant Unit_Real :=
                              Real'Min (D (Count) * Scale, 1.0 - Total);
               begin
                  if Partial > 0.0 then
                     Atm.List.Append ((Gas, Partial));
                     Total := Total + Partial;
                  end if;
               end;
            end loop;
         end Add;

      begin
         Add (Main, 1, 0.1);
         Add (Trace, 2, 0.01);

         declare
            Item : Atmospheric_Component renames
                     Atm.List (Atm.List.First);
         begin
            Item.Partial := Item.Partial + 1.0 - Total;
         end;

         Atmospheric_Sorting.Sort (Atm.List);

         return Atm;
      end Random_Atmosphere;

      ------------------------------
      -- Random_Atmospheric_Class --
      ------------------------------

      function Random_Atmospheric_Class
        (Zone : Stellar_Orbit_Zone;
         Mass : Non_Negative_Real)
         return Atmosphere_Class
      is
         Close      : constant Boolean := Zone in Yellow | Green;
         Base_Class : constant Atmosphere_Class :=
                        (if Mass <= 0.3
                         then (if Close then None else Trace)
                         elsif Mass <= 0.5
                         then (if Close then Trace else Thin)
                         elsif Mass <= 0.7 then Thin
                         elsif Mass <= 0.9 then Average
                         elsif Mass <= 1.3
                         then (if Close then Average else Dense)
                         else Dense);
         Step_Roll  : constant Unit_Real := Athena.Random.Unit_Random;
         Class      : constant Atmosphere_Class :=
                        (if Step_Roll <= 0.05 and then Base_Class > None
                         then Atmosphere_Class'Pred (Base_Class)
                         elsif Step_Roll >= 0.95 and then Base_Class < Dense
                         then Atmosphere_Class'Succ (Base_Class)
                         else Base_Class);
      begin
         return Class;
      end Random_Atmospheric_Class;

      ---------------------------
      -- Random_Planet_Density --
      ---------------------------

      function Random_Planet_Density
        (Composition : World_Composition;
         Mass        : Non_Negative_Real)
         return Non_Negative_Real
      is
         Index : constant Positive :=
                   (if Mass < 1.0 then 1
                    elsif Mass < 2.0 then 2
                    else 3);
         Low   : constant Non_Negative_Real :=
                   Density_Table (Composition, Index, 1);
         High  : constant Non_Negative_Real :=
                   Density_Table (Composition, Index, 2);
      begin
         return Athena.Random.Unit_Random * (High - Low) + Low;
      end Random_Planet_Density;

      ------------------------
      -- Random_Planet_Mass --
      ------------------------

      function Random_Planet_Mass
        (Zone : Planetary_Zone)
         return Non_Negative_Real
      is
         Roll : constant Positive := DR;
         Parameters : constant Mass_Parameters :=
                        Planet_Mass (Roll, Zone);
         Mass : constant Non_Negative_Real :=
                        (Parameters.Base
                         + Real (Parameters.Random) * D6)
                        * (1.0 - Real (D (2)) / 100.0);
      begin
         if Mass >= 300.0
           and then Athena.Random.Unit_Random < 0.5
         then
            return Mass * Real (D (2));
         else
            return Mass;
         end if;
      end Random_Planet_Mass;

      ----------------------------
      -- Random_Planet_Rotation --
      ----------------------------

      function Random_Planet_Rotation
        (Mass  : Non_Negative_Real;
         Orbit : Non_Negative_Real;
         Year  : Non_Negative_Real)
         return Non_Negative_Real
      is
         N : constant Positive :=
               (if Mass <= 0.5 then 6
                elsif Mass < 5.0 then 5
                elsif Mass < 50.0 then 4
                else 3);
         Base : constant Non_Negative_Real :=
                  Real (D (N)) * (0.8 + Athena.Random.Unit_Random * 0.4);
      begin
         if Mass < 10.0 then
            if Orbit < 0.3 then
               return Year * Athena.Solar_System.Earth_Sidereal_Year;
            elsif Orbit < 0.4 then
               return Base * D6 * 10.0;
            elsif Orbit < 0.5 then
               return Base * D6;
            else
               return Base;
            end if;
         else
            return Base;
         end if;
      end Random_Planet_Rotation;

      ------------------------
      -- Random_Planet_Tilt --
      ------------------------

      function Random_Planet_Tilt
        return Real
      is
      begin
         case D6 is
            when 1 =>
               return D6;
            when 2 | 3 =>
               return 10.0 + D (2);
            when 4 | 5 =>
               return 20.0 + D (2);
            when 6 =>
               declare
                  Tilt : constant Real := D (2) * 10.0;
               begin
                  if Tilt > 90.0 then
                     return 90.0 - Tilt;
                  else
                     return Tilt;
                  end if;
               end;
         end case;
      end Random_Planet_Tilt;

      -----------------------------
      -- Random_Surface_Pressure --
      -----------------------------

      function Random_Surface_Pressure
        (Class   : Atmosphere_Class;
         Gravity : Non_Negative_Real)
         return Non_Negative_Real
      is
      begin
         return (case Class is
                    when None => 0.0,
                    when Trace =>
                      Gravity * D (2) * 0.01,
                    when Thin  =>
                      Gravity * D6 * 0.1,
                    when Average =>
                      Gravity * D (3) * 0.2,
                    when Dense    =>
                      Gravity * D (2) * 10.0);
      end Random_Surface_Pressure;

   end Planet_Tables;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (Atm     : in out Atmosphere;
      Gas     : Atmospheric_Gas;
      Partial : Unit_Real)
   is
      Current : Non_Negative_Real := 0.0;
      New_List : Atmospheric_Component_Lists.List;
   begin
      for Item of Atm.List loop
         if Item.Partial > 0.0 then
            New_List.Append (Item);
            Current := Current + Item.Partial;
         end if;
      end loop;

      for Item of New_List loop
         Item.Partial := Item.Partial / Current;
      end loop;

      declare
         Ratio : constant Unit_Real := 1.0 - Partial;
      begin
         for Item of New_List loop
            Item.Partial := Item.Partial * Ratio;
         end loop;
      end;

      New_List.Append ((Gas, Partial));
      Atmospheric_Sorting.Sort (New_List);
      Atm.List := New_List;
   end Add_Component;

   -------
   -- D --
   -------

   function D (Count : Positive) return Non_Negative_Real is
      Value : Non_Negative_Real := 0.0;
   begin
      for I in 1 .. Count loop
         Value := Value + D6;
      end loop;
      return Value;
   end D;

   --------------------
   -- Generate_World --
   --------------------

   procedure Generate_World
     (Star  : Athena.Handles.Star.Star_Handle;
      Index : Positive;
      Zone  : Planetary_Zone;
      Orbit : Non_Negative_Real)
   is
      use Athena.Elementary_Functions;
      Name : constant String :=
               Star.Name & " "
               & WL.Numerics.Roman.Roman_Image (Index);
      Year : constant Non_Negative_Real :=
               Sqrt (Orbit ** 3 /
                     (Star.Mass / Athena.Solar_System.Solar_Mass));
      Mass : constant Non_Negative_Real :=
               Planet_Tables.Random_Planet_Mass (Zone);
      Composition : constant World_Composition :=
        Planet_Tables.Composition (Mass, Zone);
      Gas_Giant   : constant Boolean :=
        Composition in Gaseous | Hydrogen;
      Density     : constant Non_Negative_Real :=
                      Planet_Tables.Random_Planet_Density (Composition, Mass);
      Radius      : constant Non_Negative_Real :=
                      (Mass / Density) ** (1.0 / 3.0);
      Gravity     : constant Non_Negative_Real := Radius * Density;
      Smoothness  : constant Natural :=
        Natural (Gravity * 4.0);
      Elevation_Range : constant Natural :=
        Natural
          ((Athena.Random.Normal_Random (0.1) + 2.0) * 25.0);
      Day         : constant Non_Negative_Real :=
                      Planet_Tables.Random_Planet_Rotation
                        (Mass  => Mass,
                         Orbit => Orbit,
                         Year  => Year);
      Tilt        : constant Real :=
        Planet_Tables.Random_Planet_Tilt;

      Atmospheric_Class : constant Atmosphere_Class :=
                            Planet_Tables.Random_Atmospheric_Class
                              (Zone, Mass);
      Primordial_Pressure : constant Non_Negative_Real :=
                              Planet_Tables.Random_Surface_Pressure
                                (Atmospheric_Class, Gravity);
      Primordial_Atm      : constant Atmosphere :=
                              (if Composition in Rocky_World
                               and then Atmospheric_Class /= None
                               then Planet_Tables.Random_Atmosphere
                                 (Zone, Composition)
                               else (List => <>));
      Base_Temperature    : constant Non_Negative_Real :=
                              (Star.Luminosity ** 0.25) * 280.0
                              / Sqrt (Orbit);
      Initial_Temp        : constant Non_Negative_Real :=
        Real'Max
          (1.0,
           Base_Temperature
           - (case Atmospheric_Class is
                when None | Trace    => 0.0,
                when Thin | Average  => 5.0,
                when Dense           => 20.0));

      Primordial_Temp : constant Non_Negative_Real :=
        Initial_Temp + Partial (Primordial_Atm, CO2) * 100.0;

      Life_Bearing : constant Boolean :=
                       (Primordial_Temp in 253.0 .. 323.0
                        and then Atmospheric_Class >= Thin);

      Life_Complexity : constant Life_Complexity_Type :=
                          (if Star.Age <= 1.0e9
                           then Prebiotic
                           elsif Star.Age <= 2.0e9
                           then Single_Celled
                           elsif Star.Age <= 3.0e9
                           then Plants
                           else Multicellular);

      Current_Atm      : Atmosphere := Primordial_Atm;
      Current_Pressure : Non_Negative_Real := Primordial_Pressure;
      Current_Temperature : Non_Negative_Real := Primordial_Temp;

      Terrain             : array (World_Terrain) of Unit_Real :=
        (others => 0.0);

      Hydrosphere         : Unit_Real renames Terrain (Ocean);
      Climate             : World_Climate;
      Habitability        : Unit_Real;
   begin

      if Life_Bearing
        and then Life_Complexity >= Plants
      then
         for Item of Current_Atm.List loop
            if Item.Gas = CO2 then
               Item.Partial := Item.Partial / 100.0;
            elsif Item.Gas = CH4 then
               Item.Partial := 0.0;
            end if;
         end loop;
         Add_Component (Current_Atm, O2, D6 * 5.0 / 100.0);
         Atmospheric_Sorting.Sort (Current_Atm.List);
         Current_Pressure := Current_Pressure / 2.0;
      end if;

      for Item of Current_Atm.List loop
         if Item.Gas = CO2 then
            Current_Temperature := Current_Temperature + Item.Partial * 100.0;
         end if;
      end loop;

      declare
         Base_Hydrosphere : constant Real := D (2)
           + (if Mass > 1.25 then 1.0 else 0.0)
           - (if Mass < 0.75 then 1.0 else 0.0)
           + (if Current_Temperature in 290.0 .. 320.0
              then 1.0 else 0.0)
           - (if Current_Temperature > 250.0
              and then Current_Temperature <= 270.0
              then 1.0 else 0.0)
           - (if Current_Temperature in 220.0 .. 250.0
              then 2.0 else 0.0);
      begin
         Hydrosphere :=
           (if Current_Temperature < 220.0
            or else Current_Temperature > 370.0
            then 0.0
            else Unit_Clamp (Base_Hydrosphere / 12.0))
             * (if Current_Temperature > 320.0 then 0.5 else 1.0);
      end;

      if not Gas_Giant then
         declare
            Land : constant Unit_Real := 1.0 - Hydrosphere;
         begin
            Terrain (Desert) := Land ** 2;

            if Current_Temperature > 320.0 then
               Terrain (Ice) := 0.0;
               Terrain (Tundra) := 0.0;
            elsif Current_Temperature < 220.0 then
               Terrain (Ice) := Land;
            else
               Terrain (Ice) :=
                 Real'Min (Land,
                           (320.0 - Current_Temperature) / 300.0);
               Terrain (Tundra) :=
                 Real'Min (Land - Terrain (Ice),
                           (320.0 - Current_Temperature) / 500.0);
            end if;

            Terrain (Mountains) :=
              Real'Min (Land, Mass * 0.05);
         end;
      end if;

      Climate :=
        (if Gas_Giant
         then Jovian
         elsif Current_Pressure = 0.0
         then Airless
         elsif Terrain (Ice) > 0.9
         then Iceball
         elsif Hydrosphere > 0.9
         then Water
         elsif Hydrosphere < 0.1
         then Desert
         elsif Atmospheric_Class = Dense
         then Venusian
         elsif Atmospheric_Class = Thin
         then Martian
         else Temperate);

      declare
         use all type World_Climate;
         Ideal_Tmp  : constant := 285.0;
         Std_Dev_Tmp : constant := 25.0;
         Tmp_Factor : constant Unit_Real :=
           Unit_Clamp
             (Exp
                (-(Current_Temperature - Ideal_Tmp) ** 2
                 / (2.0 * Std_Dev_Tmp ** 2)));
         Std_Dev_Oxygen : constant := 0.2;
         function Oxygen_Factor (Partial : Unit_Real) return Unit_Real
         is (Unit_Clamp
             (Exp
              (-(Partial * Current_Pressure - 0.2) ** 2
               / (2.0 * Std_Dev_Oxygen ** 2))));

         Atm_Factor : Unit_Real := 1.0;
      begin

         for Item of Current_Atm.List loop
            declare
               This_Factor : constant Unit_Real :=
                 (case Item.Gas is
                     when O2 =>
                       Oxygen_Factor (Item.Partial),
                     when Cl2 | F2 | NH3 | SO2 => 0.0,
                     when CH4                  =>
                       Unit_Clamp (1.0 - Item.Partial * 10.0),
                     when CO2                  =>
                       Unit_Clamp (1.0 - Item.Partial * 10.0),
                     when H2 | He | N2 | Ar    =>
                       1.0);
            begin
               Atm_Factor := Real'Min (Atm_Factor, This_Factor);
            end;
         end loop;

         Habitability :=
           Atm_Factor *
           (case Climate is
               when Airless => 0.0,
               when Desert  =>
                 Tmp_Factor * (Hydrosphere + 0.5),
               when Iceball => 0.0,
               when Martian => 0.0,
               when Temperate => Tmp_Factor,
               when Venusian     => 0.0,
               when Water        => Tmp_Factor,
               when Jovian       => 0.0);
      end;

      if Log_Generation then
         Ada.Text_IO.Put ("  " & Name);
         Ada.Text_IO.Set_Col (16);
         Ada.Text_IO.Put
           (case Zone is
               when Yellow => "Yellow",
               when Green  => "Green",
               when Blue   => "Blue",
               when Black  => "Black");
         Ada.Text_IO.Set_Col (24);
         Ada.Text_IO.Put
           (case Composition is
               when Hydrogen  => "Hydrogen",
               when Gaseous   => "Gas",
               when Ice       => "Ice",
               when Rock      => "Rock",
               when Rock_Ice  => "Rock-Ice",
               when Rock_Iron => "Rock-Iron");

         Ada.Text_IO.Set_Col (36);
         Put (8, Orbit);
         Put (8, Mass);
         Put (8, Density);
         Put (8, Radius);
         Put (8, Gravity);
         Put (8, Year);
         Put (8, Day);

         if Composition not in Hydrogen | Gaseous then
            if Hydrosphere = 0.0 then
               Put (8, " -");
            else
               Put (8, Hydrosphere * 100.0);
            end if;
         else
            Put (8, " -");
         end if;

         Put (8, Current_Temperature - 273.0);

         if Composition not in Hydrogen | Gaseous
           and then Life_Bearing
         then
            Put (16,
                 (case Life_Complexity is
                     when No_Life => "no life",
                     when Prebiotic     => "prebiotic",
                     when Single_Celled => "single-celled",
                     when Plants        => "plants",
                     when Multicellular => "multi-cellular"));
         else
            Put (16, "none");
         end if;

         if Composition not in Hydrogen | Gaseous then
            Put (8,
                 (case Atmospheric_Class is
                     when None     => "None",
                     when Trace    => "Trace",
                     when Thin     => "Thin",
                     when Average  => "Ave",
                     when Dense    => "Dense"));
         else
            Put (8, " - ");
         end if;

         Put (8, Habitability * 100.0);

         Put (8, Current_Pressure);

         if not Gas_Giant then
            for Item of Current_Atm.List loop
               Ada.Text_IO.Put (" " & Item.Gas'Image);
               Ada.Text_IO.Put
                 (Natural'Image (Natural (Item.Partial * 100.0)));
            end loop;

            for T in Terrain'Range loop
               if Terrain (T) > 0.0 then
                  Ada.Text_IO.Put (" " & T'Image);
                  Ada.Text_IO.Put
                    (Natural'Image (Natural (Terrain (T) * 100.0)));
               end if;
            end loop;
         end if;
         Ada.Text_IO.New_Line;
      end if;

      declare
         use Athena.Solar_System;
         World : constant Athena.Handles.World.World_Handle :=
           Athena.Handles.World.Create
             (Star        => Star,
              Radius              => Radius * Earth_Radius,
              Density             => Density * Earth_Density,
              Rotation_Period     => Day * 3600.0,
              Tilt                =>
                Athena.Trigonometry.From_Degrees (Tilt),
              --  Surface_Gravity     => Gravity * Earth_Gravity,
              Name                => Name,
              Seed                => WL.Random.Random_Number (1, Integer'Last),
              Semimajor_Axis      => Orbit * Earth_Orbit,
              Zero_Time_Angle     =>
                Athena.Trigonometry.From_Degrees
                  (Athena.Random.Unit_Random * 360.0),
              Eccentricity        => 0.0,
              Inclination         => Athena.Trigonometry.From_Radians (0.0),
              Period              => Year * Earth_Sidereal_Year,
              Mass                => Mass * Earth_Mass,
              Composition         => Composition,
              Climate             => Climate,
              Orbit_Zone          => Zone,
              Gas_Giant           => Gas_Giant,
              Age                 => Star.Age,
              Habitability        => Habitability,
              Resource            => Athena.Random.Unit_Random,
              Surface_Pressure    => Current_Pressure * Earth_Surface_Pressure,
              Average_Temperature => Current_Temperature,
              Hydrosphere         => Hydrosphere,
              Life                => (if Life_Bearing
                                      then Life_Complexity
                                      else No_Life),
              Smoothness          => Smoothness,
              Elevation_Range     => Elevation_Range,
              Sea_Level           =>
                Natural (Real (Elevation_Range) * Hydrosphere));
      begin
         if Current_Pressure > 0.0 then
            for Item of Current_Atm.List loop
               World.Add_Gas (Item.Gas, Item.Partial);
            end loop;
         end if;
      end;
   end Generate_World;

   procedure Generate_Worlds
     (Star : Athena.Handles.Star.Star_Handle)
   is
      Planet_Count : constant Positive := TDR;
      Ds           : array (1 .. Planet_Count) of Non_Negative_Real;
   begin
      Ds (Ds'First) := D6 / 10.0;
      for I in Ds'First + 1 .. Ds'Last loop
         Ds (I) := Ds (I - 1) * (Real (D (2)) / 10.0 + 1.0);
      end loop;

      if Log_Generation then
         Put (16, Star.Name);
         Ada.Text_IO.Put (Star.Spectral_Class);
         Ada.Text_IO.Set_Col (24);
         Put (8, Star.Age / 1.0e9);
         Put (8, Planet_Count);

         for D of Ds loop
            declare
               Zone : constant Stellar_Orbit_Zone :=
                        Get_Zone (Star, D);
            begin
               Ada.Text_IO.Put
                 (case Zone is
                     when Red    => 'R',
                     when Yellow => 'Y',
                     when Green  => 'G',
                     when Blue   => 'B',
                     when Black  => 'x');
            end;
         end loop;
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put ("  World");
         Ada.Text_IO.Set_Col (16);
         Put (8, "Zone");
         Put (12, "Type");
         Put (8, "Orbit");
         Put (8, "Mass");
         Put (8, "Density");
         Put (8, "Radius");
         Put (8, "Gravity");
         Put (8, "Year");
         Put (8, "Day");
         Put (8, "Ocean%");
         Put (8, "Temp");
         Put (16, "Life");
         Put (8, "Atm");
         Put (8, "Hab");
         Put (8, "Pressure");
         Ada.Text_IO.New_Line;
      end if;

      declare
         Count : Natural := 0;
      begin
         for D of Ds loop
            if Get_Zone (Star, D) > Red then
               Count := Count + 1;
               Generate_World (Star, Count, Get_Zone (Star, D), D);
            end if;
         end loop;
      end;

      if Log_Generation then
         Ada.Text_IO.New_Line;
      end if;

   end Generate_Worlds;

   --------------
   -- Get_Zone --
   --------------

   function Get_Zone
     (Star : Athena.Handles.Star.Star_Handle;
      AUs  : Non_Negative_Real)
      return Stellar_Orbit_Zone
   is
      Lum : constant Non_Negative_Real :=
              Athena.Elementary_Functions.Sqrt (Star.Luminosity);
   begin
      if AUs < Lum * 0.25 then
         return Red;
      elsif AUs < Lum * 0.75 then
         return Yellow;
      elsif AUs < Lum * 1.5 then
         return Green;
      elsif AUs < Lum * 20.0 then
         return Blue;
      else
         return Black;
      end if;
   end Get_Zone;

   -------------
   -- Partial --
   -------------

   function Partial
     (Atm : Atmosphere;
      Gas : Atmospheric_Gas)
      return Unit_Real
   is
   begin
      for Item of Atm.List loop
         if Item.Gas = Gas then
            return Item.Partial;
         end if;
      end loop;
      return 0.0;
   end Partial;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : String)
   is
      use Ada.Text_IO;
      Target : constant Count := Col + Count (Width);
   begin
      Put (Value);
      Set_Col (Target);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : Real)
   is
   begin
      Put (Width, Athena.Real_Images.Approximate_Image (Value));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Width : Positive;
                  Value : Integer)
   is
   begin
      Put (Width, Value'Image);
   end Put;

end Athena.Configure.Worlds;
