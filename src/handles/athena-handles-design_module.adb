with Ada.Containers.Vectors;

package body Athena.Handles.Design_Module is

   package Component_Vectors is
     new Ada.Containers.Vectors (Positive, Component_Reference);

   type Design_Module_Record is
      record
         Identifier : Object_Identifier;
         Component  : Component_Reference;
         Parts      : Component_Vectors.Vector;
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
     (From_Component : Athena.Handles.Component.Component_Handle'Class;
      Parts          : Athena.Handles.Component.Component_Array)
      return Design_Module_Handle
   is
      function Parts_Vector return Component_Vectors.Vector;

      ------------------
      -- Parts_Vector --
      ------------------

      function Parts_Vector return Component_Vectors.Vector is
      begin
         return V : Component_Vectors.Vector do
            for Part of Parts loop
               V.Append (Part.Reference);
            end loop;
         end return;
      end Parts_Vector;

   begin
      Vector.Append
        (Design_Module_Record'
           (Identifier    => Next_Identifier,
            Component     => From_Component.Reference,
            Parts         => Parts_Vector));
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
   -- Mass --
   ----------

   function Mass
     (Handle : Design_Module_Handle)
      return Non_Negative_Real
   is
   begin
      return M : Non_Negative_Real := Handle.Component.Empty_Mass do
         for Part of Handle.Parts loop
            M := M + Part.Empty_Mass;
         end loop;
      end return;
   end Mass;

   -----------
   -- Parts --
   -----------

   function Parts
     (Handle : Design_Module_Handle)
      return Athena.Handles.Component.Component_Array
   is
      Rec : Design_Module_Record renames Vector (Handle.Reference);
   begin
      return Arr : Athena.Handles.Component.Component_Array
        (1 .. Rec.Parts.Last_Index)
      do
         for I in Arr'Range loop
            Arr (I) := Athena.Handles.Component.Get (Rec.Parts (I));
         end loop;
      end return;
   end Parts;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Design_Module_Vectors.Vector'Write (Stream, Vector);
   end Save;

end Athena.Handles.Design_Module;
