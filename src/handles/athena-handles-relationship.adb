with Ada.Containers.Vectors;

package body Athena.Handles.Relationship is

   type Relationship_Record is
      record
         From        : Empire_Reference;
         To          : Empire_Reference;
         Opinion     : Integer;
         War         : Boolean;
         Hostile     : Boolean;
         Allied      : Boolean;
         Trade       : Boolean;
      end record;

   package Relationship_Vectors is
     new Ada.Containers.Vectors
       (Real_Relationship_Reference, Relationship_Record);

   Vector : Relationship_Vectors.Vector;

   function Opinion
     (Relationship : Relationship_Handle)
      return Integer
   is (Vector (Relationship.Reference).Opinion);

   function War
     (Relationship : Relationship_Handle)
      return Boolean
   is (Vector (Relationship.Reference).War);

   -----------------------
   -- Find_Relationship --
   -----------------------

   function Find_Relationship
     (From, To : Athena.Handles.Empire.Empire_Handle)
      return Relationship_Handle
   is
   begin
      for I in 1 .. Vector.Last_Index loop
         if Vector (I).From = From.Reference
           and then Vector (I).To = To.Reference
         then
            return Get (I);
         end if;
      end loop;

      Vector.Append
        (Relationship_Record'
           (From       => From.Reference,
            To         => To.Reference,
            Opinion    => 0,
            War        => False,
            Hostile    => False,
            Allied     => False,
            Trade      => False));
      return Get (Vector.Last_Index);
   end Find_Relationship;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Relationship_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Relationship_Vectors.Vector'Write (Stream, Vector);
   end Save;

   -----------------
   -- Set_Opinion --
   -----------------

   procedure Set_Opinion
     (Relationship : Relationship_Handle;
      To_Value     : Integer)
   is
   begin
      Vector (Relationship.Reference).Opinion := To_Value;
   end Set_Opinion;

   -------------
   -- Set_War --
   -------------

   procedure Set_War
     (Relationship : Relationship_Handle;
      To_Value     : Boolean)
   is
   begin
      Vector (Relationship.Reference).War := To_Value;
   end Set_War;

   ----------------
   -- Short_Name --
   ----------------

   overriding function Short_Name
     (Relationship : Relationship_Handle)
      return String
   is
      Rec : Relationship_Record renames Vector (Relationship.Reference);
   begin
      return Athena.Handles.Empire.Get (Rec.From).Name
        & "/" & Athena.Handles.Empire.Get (Rec.To).Name;
   end Short_Name;

end Athena.Handles.Relationship;
