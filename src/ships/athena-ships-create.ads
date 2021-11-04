with Minerva.Empire;
with Minerva.Empire_Manager;
with Minerva.Fleet;
with Minerva.Ship_Design;
with Minerva.Star;

package Athena.Ships.Create is

   procedure Create_Ship
     (Empire      : Minerva.Empire.Empire_Class;
      Star        : Minerva.Star.Star_Class;
      Design      : Minerva.Ship_Design.Ship_Design_Class;
      Fleet       : Minerva.Fleet.Fleet_Class;
      Manager     : Minerva.Empire_Manager.Empire_Manager_Class;
      Name        : String;
      Destination : Minerva.Star.Star_Class)
     with Pre => Design.Has_Element
     and then Star.Has_Element;

end Athena.Ships.Create;
