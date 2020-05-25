with Ada.Containers.Vectors;

package body Athena.Handles.Module is

   type Module_Record is
      record
         Design_Module : Design_Module_Reference;
         Component     : Component_Reference;
         Condition     : Unit_Real;
         Tec_Level     : Non_Negative_Real;
      end record;

   package Module_Vectors is
     new Ada.Containers.Vectors
       (Real_Module_Reference, Module_Record);

   Vector : Module_Vectors.Vector;

   function Component
     (Module : Module_Handle)
      return Athena.Handles.Component.Component_Handle
   is (Athena.Handles.Component.Get (Vector (Module.Reference).Component));

   function Design_Module
     (Module : Module_Handle)
      return Athena.Handles.Design_Module.Design_Module_Handle
   is (Athena.Handles.Design_Module.Get (Vector (Module.Reference)
                                         .Design_Module));

   function Condition
     (Module : Module_Handle)
      return Unit_Real
   is (Vector (Module.Reference).Condition);

   function Tec_Level
     (Module : Module_Handle)
      return Non_Negative_Real
   is (Vector (Module.Reference).Tec_Level);

   ------------
   -- Create --
   ------------

   function Create
     (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle)
      return Module_Handle
   is
   begin
      Vector.Append
        (Module_Record'
           (Design_Module => Design_Module.Reference,
            Component     => Design_Module.Component.Reference,
            Condition     => 1.0,
            Tec_Level     => 1.0));
      return (True, Vector.Last_Index);
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Module_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Module_Vectors.Vector'Write (Stream, Vector);
   end Save;

end Athena.Handles.Module;
