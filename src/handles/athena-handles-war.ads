with Ada.Streams.Stream_IO;

with Athena.Handles.Empire;

package Athena.Handles.War is

   type War_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
   with private;

   function Empty_Handle return War_Handle;
   function Reference (War : War_Handle) return War_Reference;
   function Get (War : War_Reference) return War_Handle;

   function Attacker
     (War : War_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   function Defender
     (War : War_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   function Start
     (War : War_Handle)
      return Athena_Turn_Number;

   function Active
     (War : War_Handle)
      return Boolean;

   function Finish
     (War : War_Handle)
      return Athena_Turn_Number;

   procedure Create
     (Attacker : Athena.Handles.Empire.Empire_Handle;
      Defender : Athena.Handles.Empire.Empire_Handle);

   procedure Resolve
     (War : War_Handle;
      Victor : Athena.Handles.Empire.Empire_Handle);

   function Find_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
      return War_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type War_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface with
      record
         Reference : War_Reference := 0;
      end record;

   overriding function Short_Name
     (War : War_Handle)
      return String;

   overriding function Identifier
     (War : War_Handle)
      return Object_Identifier;

   function Empty_Handle return War_Handle
   is (False, Null_War_Reference);

   function Reference (War : War_Handle) return War_Reference
   is (War.Reference);

   function Get (War : War_Reference) return War_Handle
   is (War /= 0, War);

end Athena.Handles.War;
