with Ada.Streams.Stream_IO;

package Athena.Handles.Technology is

   type Technology_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference
     (Technology : Technology_Handle)
      return Technology_Reference;

   function Get
     (Technology : Technology_Reference)
      return Technology_Handle;

   function Get_By_Tag (Tag : String) return Technology_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Technology_Handle is
     new Root_Athena_Handle
     and Localised_Interface with
      record
         Reference : Technology_Reference := 0;
      end record;

   overriding function Tag
     (Technology : Technology_Handle)
      return String;

   overriding function Short_Name
     (Technology : Technology_Handle)
      return String
   is (Technology.Tag);

   function Reference
     (Technology : Technology_Handle)
      return Technology_Reference
   is (Technology.Reference);

   function Get (Technology : Technology_Reference) return Technology_Handle
   is (Technology /= 0, Technology);

end Athena.Handles.Technology;
