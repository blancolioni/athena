with Ada.Streams.Stream_IO;

with Athena.Handles.Component;
with Athena.Handles.Design_Module;

package Athena.Handles.Module is

   type Module_Handle is
     new Root_Athena_Handle
   with private;

   function Reference (Module : Module_Handle) return Module_Reference;
   function Get (Module : Module_Reference) return Module_Handle;

   function Condition
     (Module : Module_Handle)
      return Unit_Real;

   function Tec_Level
     (Module : Module_Handle)
      return Non_Negative_Real;

   function Component
     (Module : Module_Handle)
      return Athena.Handles.Component.Component_Handle;

   function Design_Module
     (Module : Module_Handle)
      return Athena.Handles.Design_Module.Design_Module_Handle;

   function Create
     (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle)
      return Module_Handle;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Module_Handle is
     new Root_Athena_Handle with
      record
         Reference : Module_Reference := 0;
      end record;

   overriding function Short_Name
     (Module : Module_Handle)
      return String
   is (Module.Component.Short_Name);

   function Reference (Module : Module_Handle) return Module_Reference
   is (Module.Reference);

   function Get (Module : Module_Reference) return Module_Handle
   is (Module /= 0, Module);

end Athena.Handles.Module;
