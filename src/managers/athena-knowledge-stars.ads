private with WL.String_Maps;

with Ada.Containers.Doubly_Linked_Lists;

with Minerva.Colony;
with Minerva.Empire;
with Minerva.Star;
with Minerva.Turn;

package Athena.Knowledge.Stars is

   type Known_Ship_Record is
      record
         Mass : Non_Negative_Real;
         Weapon_Mass : Non_Negative_Real;
      end record;

   package Known_Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Known_Ship_Record);

   type Star_Knowledge is tagged private;

   procedure Load
     (Knowledge  : in out Star_Knowledge;
      For_Empire : Minerva.Empire.Empire_Class);

   function Colonizing
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Boolean;

   function Visited
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Boolean;

   function Last_Visit
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Minerva.Turn.Turn_Class;

   function Turns_Since_Last_Visit
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Natural;

   function Get_Known_Ships
     (Knowledge : Star_Knowledge'Class;
      At_Star   : Minerva.Star.Star_Class)
      return Known_Ship_Lists.List;

   procedure Iterate_Neighbours
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Neighbour : Minerva.Star.Star_Class;
                   Nearest   : Minerva.Colony.Colony_Class;
                   Stop      : out Boolean));

   procedure Iterate_Threats
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Threat      : Minerva.Empire.Empire_Class;
                   Threat_Star : Minerva.Star.Star_Class;
                   Nearest     : Minerva.Colony.Colony_Class;
                   Stop        : out Boolean));

   procedure Iterate_Uncolonized
     (Knowledge : Star_Knowledge'Class;
      Process   : not null access
        procedure (Star      : Minerva.Star.Star_Class;
                   Stop      : out Boolean));

   procedure Set_Colonizing
     (Knowledge  : in out Star_Knowledge'Class;
      Star       : Minerva.Star.Star_Class;
      Colonizing : Boolean);

   procedure Clear_Colonizing
     (Empire     : Minerva.Empire.Empire_Class;
      Star       : Minerva.Star.Star_Class);

   procedure Visit
     (Empire : Minerva.Empire.Empire_Class;
      Star   : Minerva.Star.Star_Class);

   procedure Clear_Cache;

private

   type Neighbour_Record is
      record
         Neighbour : Minerva.Star.Star_Handle;
         Nearest   : Minerva.Colony.Colony_Handle;
         Distance  : Non_Negative_Real;
      end record;

   package Neighbour_Maps is
     new WL.String_Maps (Neighbour_Record);

   package Neighbour_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Neighbour_Record);

   package Star_Maps is
     new WL.String_Maps
       (Minerva.Star.Star_Class,
        Minerva.Star."=");

   type Star_Knowledge is tagged
      record
         Empire         : Minerva.Empire.Empire_Handle;
         Neighbour_Map  : Neighbour_Maps.Map;
         Neighbour_List : Neighbour_Lists.List;
         Threat_Map     : Neighbour_Maps.Map;
         Threat_List    : Neighbour_Lists.List;
         Uncolonized    : Star_Maps.Map;
         Visited        : Star_Maps.Map;
         Colonizing     : Star_Maps.Map;
      end record;

   function Visited
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Boolean
   is (Knowledge.Visited.Contains (Star.Identifier));

   function Colonizing
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Boolean
   is (Knowledge.Colonizing.Contains (Star.Identifier));

end Athena.Knowledge.Stars;
