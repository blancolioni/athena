package Athena.Solar_System is

   Solar_Mass                 : constant := 1.98892e30;
   Earth_Mass                 : constant := 5.9722e24;
   Moon_Mass                  : constant := 7.34767309e22;
   Solar_Mass_In_Earth_Masses : constant :=
                                  Solar_Mass / Earth_Mass;
   Solar_Radius               : constant := 695_700_000.0;
   Earth_Radius               : constant := 6_371_000.0;
   Earth_Orbit                : constant := 149_598_261_000.0;

   Solar_Surface_Temperature  : constant := 5772.0;
   Earth_Exospheric_Temp      : constant := 1273.0;
   Earth_Effective_Temp       : constant := 250.0;
   Earth_Average_Kelvin       : constant := 287.15;
   Earth_Gravity              : constant := 9.81;
   Earth_Density              : constant := 5.52e-3;
   Earth_Surface_Pressure     : constant := 1013.25;
   Earth_Convection_Factor    : constant := 0.43;
   Earth_Water_Mass_Per_Area  : constant := 3.83e12;
   Change_In_Earth_Angular_V  : constant := -1.3E-16;
   Earth_Sidereal_Year        : constant := 365.256363004;

   Cloud_Coverage_Factor      : constant := 1.839e-8;

   Ice_Albedo                 : constant := 0.7;
   Cloud_Albedo               : constant := 0.52;
   Gas_Giant_Albedo           : constant := 0.5;
   Airless_Ice_Albedo         : constant := 0.5;
   Earth_Albedo               : constant := 0.3;
   Greenhouse_Trigger_Albedo  : constant := 0.2;
   Rocky_Albedo               : constant := 0.15;
   Rocky_Airless_Albedo       : constant := 0.07;
   Water_Albedo               : constant := 0.04;

end Athena.Solar_System;
