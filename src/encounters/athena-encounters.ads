with Minerva.Drivers;

package Athena.Encounters is

   type Actor_Interface is interface;

   function Self_Driver
     (Actor : Actor_Interface)
      return Minerva.Drivers.Minerva_Driver
      is abstract;

   function Ally_Driver
     (Actor : Actor_Interface)
      return Minerva.Drivers.Minerva_Driver
      is abstract;

   function Hostile_Driver
     (Actor : Actor_Interface)
      return Minerva.Drivers.Minerva_Driver
      is abstract;

end Athena.Encounters;
