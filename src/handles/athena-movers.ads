with Athena.Real_Arrays;

with Athena.Handles.Star;
with Athena.Handles.World;

package Athena.Movers is

   type Mover_Location_Type is
     (Nowhere, Deep_Space, System_Space, World_Orbit);

   type Mover_Interface is interface;

   function Location
     (Mover : Mover_Interface)
      return Mover_Location_Type
   is abstract;

   function Has_Destination
     (Mover : Mover_Interface)
      return Boolean
      is abstract;

   function Location_Star
     (Mover : Mover_Interface)
      return Athena.Handles.Star.Star_Handle
   is abstract
     with Pre'Class => Mover.Location in World_Orbit | System_Space;

   function Location_World
     (Mover : Mover_Interface)
      return Athena.Handles.World.World_Handle
   is abstract
     with Pre'Class => Mover.Location = World_Orbit;

   function System_Position
     (Mover : Mover_Interface)
      return Athena.Real_Arrays.Real_Vector
      is abstract
     with Pre'Class => Mover.Location = System_Space;

   function Origin_Star
     (Mover : Mover_Interface)
      return Athena.Handles.Star.Star_Handle
      is abstract
     with Pre'Class => Mover.Has_Destination;

   function Destination_Star
     (Mover : Mover_Interface)
      return Athena.Handles.Star.Star_Handle
      is abstract
     with Pre'Class => Mover.Has_Destination
     and then Mover.Location = Deep_Space;

   function Destination_World
     (Mover : Mover_Interface)
      return Athena.Handles.World.World_Handle
      is abstract
     with Pre'Class => Mover.Has_Destination
     and then Mover.Location = System_Space;

   function Progress
     (Mover : Mover_Interface)
      return Unit_Real
      is abstract
     with Pre'Class => Mover.Has_Destination;

   function Location_Name
     (Mover : Mover_Interface'Class)
      return String;

   function Destination_Name
     (Mover : Mover_Interface'Class)
      return String
     with Pre => Mover.Has_Destination;

   function Is_Closer
     (Mover : Mover_Interface'Class;
      Than  : Mover_Interface'Class;
      World : Athena.Handles.World.World_Handle)
      return Boolean;

   function Is_Closer
     (Mover : Mover_Interface'Class;
      Than  : Mover_Interface'Class;
      Star  : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function At_World
     (Mover : Mover_Interface'Class;
      World : Athena.Handles.World.World_Handle)
      return Boolean;

   function At_Star
     (Mover : Mover_Interface'Class;
      Star  : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function Travelling_To
     (Mover : Mover_Interface'Class;
      Star  : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function Travelling_To
     (Mover : Mover_Interface'Class;
      World : Athena.Handles.World.World_Handle)
      return Boolean;

end Athena.Movers;
