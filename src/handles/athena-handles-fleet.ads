with Ada.Streams.Stream_IO;

with Athena.Handles.Star;
with Athena.Handles.Empire;

package Athena.Handles.Fleet is

   type Fleet_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface
   with private;

   function Reference (Fleet : Fleet_Handle) return Fleet_Reference;
   function Get (Fleet : Fleet_Reference) return Fleet_Handle;

   function Empty_Handle return Fleet_Handle;

   function Owner
     (Fleet : Fleet_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   function Location
     (Fleet : Fleet_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Destination
     (Fleet : Fleet_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Progress
     (Fleet : Fleet_Handle)
      return Unit_Real;

   procedure Set_Destination
     (Fleet       : Fleet_Handle;
      Destination : Athena.Handles.Star.Star_Handle);

   procedure Create
     (Name  : String;
      Star  : Athena.Handles.Star.Star_Handle;
      Owner : Athena.Handles.Empire.Empire_Handle);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Fleet_Handle is
     new Root_Athena_Handle
     and Has_Name_Interface with
      record
         Reference : Fleet_Reference := 0;
      end record;

   overriding function Name
     (Fleet : Fleet_Handle)
      return String;

   overriding procedure Set_Name
     (Fleet    : Fleet_Handle;
      New_Name : String);

   overriding function Short_Name
     (Fleet : Fleet_Handle)
      return String
   is (Fleet.Name);

   function Reference (Fleet : Fleet_Handle) return Fleet_Reference
   is (Fleet.Reference);

   function Get (Fleet : Fleet_Reference) return Fleet_Handle
   is (Fleet /= 0, Fleet);

   function Empty_Handle return Fleet_Handle
   is (False, 0);

end Athena.Handles.Fleet;
