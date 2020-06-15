with Ada.Streams.Stream_IO;

with Athena.Handles.Component;

package Athena.Handles.Design_Module is

   type Design_Module_Handle is
     new Root_Athena_Handle
   with private;

   function Reference
     (Module : Design_Module_Handle)
      return Design_Module_Reference;

   function Get
     (Module : Design_Module_Reference)
      return Design_Module_Handle;

   function Component
     (Handle : Design_Module_Handle)
      return Athena.Handles.Component.Component_Handle'Class;

   function Mass
     (Handle : Design_Module_Handle)
      return Non_Negative_Real;

   function Parts
     (Handle : Design_Module_Handle)
      return Athena.Handles.Component.Component_Array;

   function Create
     (From_Component : Athena.Handles.Component.Component_Handle'Class;
      Parts          : Athena.Handles.Component.Component_Array)
      return Design_Module_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Design_Module_Handle is
     new Root_Athena_Handle with
      record
         Reference : Design_Module_Reference := 0;
      end record;

   overriding function Short_Name
     (Design_Module : Design_Module_Handle)
      return String
   is (Design_Module.Component.Short_Name);

   function Reference
     (Module : Design_Module_Handle)
      return Design_Module_Reference
   is (Module.Reference);

   function Get
     (Module : Design_Module_Reference)
      return Design_Module_Handle
   is (Module /= 0, Module);

end Athena.Handles.Design_Module;
