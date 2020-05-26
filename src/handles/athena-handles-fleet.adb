with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body Athena.Handles.Fleet is

   type Fleet_Record is
      record
         Identifier  : Object_Identifier;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Star        : Star_Reference;
         Owner       : Empire_Reference;
         Destination : Star_Reference;
         Progress    : Unit_Real;
      end record;

   package Fleet_Vectors is
     new Ada.Containers.Vectors
       (Real_Fleet_Reference, Fleet_Record);

   Vector : Fleet_Vectors.Vector;

   overriding function Name
     (Fleet : Fleet_Handle)
      return String
   is (-(Vector (Fleet.Reference).Name));

   function Owner
     (Fleet : Fleet_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (Fleet.Reference).Owner));

   function Location
     (Fleet : Fleet_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Fleet.Reference).Star));

   function Destination
     (Fleet : Fleet_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Fleet.Reference).Destination));

   function Progress
     (Fleet : Fleet_Handle)
      return Unit_Real
   is (Vector (Fleet.Reference).Progress);

   ------------
   -- Create --
   ------------

   procedure Create
     (Name  : String;
      Star  : Athena.Handles.Star.Star_Handle;
      Owner : Athena.Handles.Empire.Empire_Handle)
   is
   begin
      Vector.Append
        (Fleet_Record'
           (Identifier    => Next_Identifier,
            Star          => Star.Reference,
            Owner         => Owner.Reference,
            Name          => +Name,
            Destination   => 0,
            Progress      => 0.0));
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Fleet_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Fleet_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Fleet       : Fleet_Handle;
      Destination : Athena.Handles.Star.Star_Handle)
   is
   begin
      Vector (Fleet.Reference).Destination := Destination.Reference;
   end Set_Destination;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Fleet    : Fleet_Handle;
      New_Name : String)
   is
      Rec : Fleet_Record renames Vector (Fleet.Reference);
   begin
      Rec.Name := +New_Name;
   end Set_Name;

end Athena.Handles.Fleet;
