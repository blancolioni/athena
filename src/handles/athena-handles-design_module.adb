with Ada.Containers.Vectors;

package body Athena.Handles.Design_Module is

   type Design_Module_Record is
      record
         Identifier : Object_Identifier;
         Component  : Component_Reference;
      end record;

   package Design_Module_Vectors is
     new Ada.Containers.Vectors
       (Real_Design_Module_Reference, Design_Module_Record);

   Vector : Design_Module_Vectors.Vector;

   function Component
     (Handle : Design_Module_Handle)
      return Athena.Handles.Component.Component_Handle'Class
   is (Athena.Handles.Component.Get
       (Vector (Handle.Reference).Component));

   ------------
   -- Create --
   ------------

   function Create
     (From_Component : Athena.Handles.Component.Component_Handle'Class)
      return Design_Module_Handle
   is
   begin
      Vector.Append
        (Design_Module_Record'
           (Identifier    => Next_Identifier,
            Component     => From_Component.Reference));
      return Get (Vector.Last_Index);
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Design_Module_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Design_Module_Vectors.Vector'Write (Stream, Vector);
   end Save;

end Athena.Handles.Design_Module;
