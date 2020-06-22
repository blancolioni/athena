with Athena.Calendar;
with Athena.Real_Arrays;
with Athena.Trigonometry;

package Athena.Orbits is

   subtype Orbital_Point is Athena.Real_Arrays.Real_Vector (1 .. 3);

   type Massive_Interface is interface;

   function Mass (Item : Massive_Interface) return Non_Negative_Real
                  is abstract;

   type Orbiting_Interface is interface and Massive_Interface;

   function Primary
     (Item : Orbiting_Interface) return Massive_Interface'Class
      is abstract;

   function Semimajor_Axis
     (Item : Orbiting_Interface)
      return Non_Negative_Real
      is abstract;

   function Epoch
     (Item : Orbiting_Interface)
      return Athena.Calendar.Time
      is abstract;

   function Eccentricity
     (Item : Orbiting_Interface)
      return Unit_Real
      is abstract;

   function Inclination
     (Item : Orbiting_Interface)
      return Athena.Trigonometry.Angle
      is abstract;

   function Period
     (Item : Orbiting_Interface'Class)
      return Duration;

   function Local_Position
     (Item : Orbiting_Interface'Class;
      Clock : Athena.Calendar.Time)
      return Orbital_Point;

   function Global_Position
     (Item  : Orbiting_Interface'Class;
      Clock : Athena.Calendar.Time)
      return Orbital_Point;

   function Current_Local_Position
     (Item  : Orbiting_Interface'Class)
      return Orbital_Point;

   function Current_Global_Position
     (Item  : Orbiting_Interface'Class)
      return Orbital_Point;

   --  function Current_Angle
   --    (Item  : Orbiting_Interface'Class)
   --     return Athena.Trigonometry.Angle;

end Athena.Orbits;
