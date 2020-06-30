with Ada.Streams.Stream_IO;

with Athena.Movers;

with Athena.Handles.Ship;
with Athena.Handles.Star;
with Athena.Handles.Empire;
with Athena.Handles.World;

package Athena.Handles.Fleet is

   type Fleet_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface
     and Athena.Movers.Mover_Interface
   with private;

   function Reference (Fleet : Fleet_Handle) return Fleet_Reference;
   function Get (Fleet : Fleet_Reference) return Fleet_Handle;

   function Empty_Handle return Fleet_Handle;

   function Owner
     (Fleet : Fleet_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   overriding function Location
     (Fleet : Fleet_Handle)
      return Athena.Movers.Mover_Location;

   overriding function Has_Destination
     (Fleet : Fleet_Handle)
      return Boolean;

   overriding function Destination
     (Fleet : Fleet_Handle)
      return Athena.Movers.Mover_Location;

   overriding function Progress
     (Fleet : Fleet_Handle)
      return Unit_Real;

   procedure Move_To
     (Fleet       : Fleet_Handle;
      Destination : Athena.Handles.Star.Star_Handle)
     with Pre => not Fleet.Is_Empty;

   procedure Move_To
     (Fleet       : Fleet_Handle;
      Destination : Athena.Handles.World.World_Handle)
     with Post => not Fleet.Is_Empty or else Fleet.At_World (Destination);

   function Is_Empty
     (Fleet : Fleet_Handle)
      return Boolean;

   function Ship_Count
     (Fleet : Fleet_Handle)
      return Natural;

   function Create
     (Name  : String;
      World : Athena.Handles.World.World_Handle;
      Owner : Athena.Handles.Empire.Empire_Handle)
     return Fleet_Handle;

   procedure Add_Ship
     (Fleet : Fleet_Handle;
      Ship  : Ship_Reference);

   procedure Remove_Ship
     (Fleet : Fleet_Handle;
      Ship  : Ship_Reference);

   procedure Iterate_Ships
     (Fleet : Fleet_Handle;
      Process : not null access procedure
        (Ship : Ship_Reference));

   procedure Iterate_All
     (Process : not null access procedure
        (Fleet   : Fleet_Handle));

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Fleet_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface
     and Athena.Movers.Mover_Interface with
      record
         Reference : Fleet_Reference := 0;
      end record;

   overriding function Name
     (Fleet : Fleet_Handle)
      return String;

   overriding function Short_Name
     (Fleet : Fleet_Handle)
      return String;

   function Reference (Fleet : Fleet_Handle) return Fleet_Reference
   is (Fleet.Reference);

   function Get (Fleet : Fleet_Reference) return Fleet_Handle
   is (Fleet /= 0, Fleet);

   function Empty_Handle return Fleet_Handle
   is (False, 0);

end Athena.Handles.Fleet;
