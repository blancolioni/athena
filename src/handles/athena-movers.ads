with Athena.Real_Arrays;

with Athena.Handles.Star;
with Athena.Handles.World;

package Athena.Movers is

   type Mover_Location_Type is
     (Nowhere, Deep_Space, System_Space, World_Orbit);

   subtype Real_Location is
     Mover_Location_Type range Deep_Space .. World_Orbit;

   type Mover_Location (Loc_Type : Mover_Location_Type := Nowhere) is
      record
         case Loc_Type is
            when Nowhere =>
               null;
            when Deep_Space =>
               Position : Athena.Real_Arrays.Real_Vector (1 .. 3);
            when System_Space =>
               Star            : Athena.Handles.Star.Star_Handle;
               System_Position : Athena.Real_Arrays.Real_Vector (1 .. 3);
            when World_Orbit =>
               World    : Athena.Handles.World.World_Handle;
         end case;
      end record;

   function Get_Star
     (Location : Mover_Location)
      return Athena.Handles.Star.Star_Handle;

   type Mover_Interface is interface;

   function Location
     (Mover : Mover_Interface)
      return Mover_Location
   is abstract;

   function Has_Destination
     (Mover : Mover_Interface)
      return Boolean
      is abstract;

   function Destination
     (Mover : Mover_Interface)
      return Mover_Location
      is abstract;

   function Progress
     (Mover : Mover_Interface)
      return Unit_Real
      is abstract
     with Pre'Class => Mover.Has_Destination;

   function Current_Location
     (Mover : Mover_Interface'Class)
      return Mover_Location;

   function Current_Journey_Length
     (Mover : Mover_Interface'Class)
      return Non_Negative_Real;

   function Nowhere (Mover : Mover_Interface'Class) return Boolean;
   function In_Deep_Space (Mover : Mover_Interface'Class) return Boolean;
   function In_System_Space (Mover : Mover_Interface'Class) return Boolean;
   function Orbiting_World (Mover : Mover_Interface'Class) return Boolean;

   function Location_Star
     (Mover : Mover_Interface'Class)
      return Athena.Handles.Star.Star_Handle
     with Pre => Mover.In_System_Space or else Mover.Orbiting_World;

   function Location_World
     (Mover : Mover_Interface'Class)
      return Athena.Handles.World.World_Handle
     with Pre => Mover.Orbiting_World;

   function System_Position
     (Mover : Mover_Interface'Class)
      return Athena.Real_Arrays.Real_Vector
     with Pre => In_System_Space (Mover);

   function Deep_Space_Position
     (Mover : Mover_Interface'Class)
      return Athena.Real_Arrays.Real_Vector
     with Pre => In_Deep_Space (Mover);

   --  function Origin_Star
   --    (Mover : Mover_Interface'Class)
   --     return Athena.Handles.Star.Star_Handle
   --    with Pre => Mover.Has_Destination;

   function Destination_Star
     (Mover : Mover_Interface'Class)
      return Athena.Handles.Star.Star_Handle
     with Pre => Mover.Has_Destination
     and then In_Deep_Space (Mover);

   function Destination_World
     (Mover : Mover_Interface'Class)
      return Athena.Handles.World.World_Handle
     with Pre => Mover.Has_Destination
     and then In_System_Space (Mover);

   function Location_Name
     (Mover : Mover_Interface'Class)
      return String;

   function Current_Location_Name
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

private

   function Nowhere (Mover : Mover_Interface'Class) return Boolean
   is (Mover.Location.Loc_Type = Nowhere);

   function In_Deep_Space (Mover : Mover_Interface'Class) return Boolean
   is (Mover.Location.Loc_Type = Deep_Space);

   function In_System_Space (Mover : Mover_Interface'Class) return Boolean
   is (Mover.Location.Loc_Type = System_Space);

   function Orbiting_World (Mover : Mover_Interface'Class) return Boolean
   is (Mover.Location.Loc_Type = World_Orbit);

   function System_Position
     (Mover : Mover_Interface'Class)
      return Athena.Real_Arrays.Real_Vector
   is (Mover.Location.System_Position);

   function Deep_Space_Position
     (Mover : Mover_Interface'Class)
      return Athena.Real_Arrays.Real_Vector
   is (Mover.Location.Position);

   function Destination_Star
     (Mover : Mover_Interface'Class)
      return Athena.Handles.Star.Star_Handle
   is (Get_Star (Mover.Destination));

   function Destination_World
     (Mover : Mover_Interface'Class)
      return Athena.Handles.World.World_Handle
   is (Mover.Destination.World);

end Athena.Movers;
