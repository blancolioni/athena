with Ada.Streams.Stream_IO;

with Athena.Handles.Empire;

package Athena.Handles.Relationship is

   type Relationship_Handle is
     new Root_Athena_Handle
   with private;

   function Empty_Handle return Relationship_Handle;

   function Reference
     (Relationship : Relationship_Handle)
      return Relationship_Reference;

   function Get
     (Relationship : Relationship_Reference)
      return Relationship_Handle;

   function Opinion
     (Relationship : Relationship_Handle)
      return Integer;

   function War
     (Relationship : Relationship_Handle)
      return Boolean;

   procedure Set_Opinion
     (Relationship : Relationship_Handle;
      To_Value     : Integer);

   procedure Set_War
     (Relationship : Relationship_Handle;
      To_Value     : Boolean);

   function Find_Relationship
     (From, To : Athena.Handles.Empire.Empire_Handle)
      return Relationship_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Relationship_Handle is
     new Root_Athena_Handle with
      record
         Reference : Relationship_Reference := 0;
      end record;

   overriding function Short_Name
     (Relationship : Relationship_Handle)
      return String;

   function Empty_Handle return Relationship_Handle
   is (False, Null_Relationship_Reference);

   function Reference
     (Relationship : Relationship_Handle)
      return Relationship_Reference
   is (Relationship.Reference);

   function Get
     (Relationship : Relationship_Reference)
      return Relationship_Handle
   is (Relationship /= 0, Relationship);

end Athena.Handles.Relationship;
