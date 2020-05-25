with Ada.Streams.Stream_IO;

package Athena.Handles.Hull is

   type Hull_Handle is
     new Root_Athena_Handle
     and Localised_Interface
   with private;

   function Reference (Handle : Hull_Handle) return Hull_Reference;
   function Get (Reference : Hull_Reference) return Hull_Handle;

   function Get_By_Tag
     (Tag : String)
      return Hull_Handle;

   procedure Create
     (Tag           : String;
      Streamlined   : Boolean;
      Hull_Points   : Non_Negative_Real;
      Cost          : Non_Negative_Real;
      Comfort       : Non_Negative_Real;
      Armor_Tonnage : Non_Negative_Real);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Hull_Handle is
     new Root_Athena_Handle
     and Localised_Interface with
      record
         Reference : Hull_Reference := 0;
      end record;

   overriding function Tag
     (Hull : Hull_Handle)
      return String;

   overriding function Short_Name
     (Hull : Hull_Handle)
      return String
   is (Hull.Tag);

   function Reference (Handle : Hull_Handle) return Hull_Reference
   is (Handle.Reference);

   function Get (Reference : Hull_Reference) return Hull_Handle
   is (Reference /= 0, Reference);

end Athena.Handles.Hull;
