with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Athena.Handles.Ship.Actions;

package body Athena.Handles.Fleet is

   package Ship_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type Fleet_Record is
      record
         Identifier  : Object_Identifier;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Star        : Star_Reference;
         Owner       : Empire_Reference;
         Destination : Star_Reference;
         Progress    : Unit_Real;
         Ships       : Ship_Reference_Lists.List;
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

   function Has_Destination
     (Fleet : Fleet_Handle)
      return Boolean
   is (Vector (Fleet.Reference).Destination /= Null_Star_Reference);

   function Destination
     (Fleet : Fleet_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Fleet.Reference).Destination));

   function Progress
     (Fleet : Fleet_Handle)
      return Unit_Real
   is (Vector (Fleet.Reference).Progress);

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (Fleet : Fleet_Handle;
      Ship  : Ship_Reference)
   is
   begin
      Vector (Fleet.Reference).Ships.Append (Ship);
      Athena.Handles.Ship.Get (Ship).Set_Fleet (Fleet.Reference);
   end Add_Ship;

   ------------
   -- Create --
   ------------

   function Create
     (Name  : String;
      Star  : Athena.Handles.Star.Star_Handle;
      Owner : Athena.Handles.Empire.Empire_Handle)
      return Fleet_Handle
   is
   begin
      Vector.Append
        (Fleet_Record'
           (Identifier    => Next_Identifier,
            Star          => Star.Reference,
            Owner         => Owner.Reference,
            Name          => +Name,
            Destination   => 0,
            Progress      => 0.0,
            Ships         => <>));
      Owner.Add_Fleet (Vector.Last_Index);
      return Get (Vector.Last_Index);
   end Create;

   -------------------
   -- Iterate_Ships --
   -------------------

   procedure Iterate_Ships
     (Fleet   : Fleet_Handle;
      Process : not null access procedure
        (Ship : Ship_Reference))
   is
   begin
      for Ship of Vector (Fleet.Reference).Ships loop
         Process (Ship);
      end loop;
   end Iterate_Ships;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Fleet_Vectors.Vector'Read (Stream, Vector);
   end Load;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Fleet  : Fleet_Handle;
      Ship   : Ship_Reference)
   is
      use Ship_Reference_Lists;
      Ships    : List renames Vector (Fleet.Reference).Ships;
      Position : Cursor := Ships.Find (Ship);
   begin
      pragma Assert (Has_Element (Position),
                     Fleet.Name & ": does not contain ship"
                     & Ship'Image);
      Ships.Delete (Position);
   end Remove_Ship;

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
      for Ship_Ref of Vector (Fleet.Reference).Ships loop
         Athena.Handles.Ship.Actions.Move_To
           (Ship => Athena.Handles.Ship.Get (Ship_Ref),
            Star => Destination);
      end loop;
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
