with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with Athena.Handles.Ship.Actions;

with Athena.Handles.Vectors;

package body Athena.Handles.Fleet is

   package Ship_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type Fleet_Record is
      record
         Identifier  : Object_Identifier;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         World       : World_Reference;
         Owner       : Empire_Reference;
         Ships       : Ship_Reference_Lists.List;
      end record;

   package Fleet_Vectors is
     new Athena.Handles.Vectors
       (Real_Fleet_Reference, Fleet_Record);

   Vector : Fleet_Vectors.Vector;

   overriding function Name
     (Fleet : Fleet_Handle)
      return String
   is (-(Vector (Fleet.Reference).Name));

   function Is_Empty
     (Fleet : Fleet_Handle)
      return Boolean
   is (Vector (Fleet.Reference).Ships.Is_Empty);

   function Ship_Count
     (Fleet : Fleet_Handle)
      return Natural
   is (Natural (Vector (Fleet.Reference).Ships.Length));

   function First_Ship
     (Fleet : Fleet_Handle)
      return Athena.Handles.Ship.Ship_Handle
   is (Athena.Handles.Ship.Get
       (Vector (Fleet.Reference).Ships.First_Element))
     with Pre => not Fleet.Is_Empty;

   function Owner
     (Fleet : Fleet_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (Fleet.Reference).Owner));

   overriding function Location
     (Fleet : Fleet_Handle)
      return Athena.Movers.Mover_Location
   is (if Fleet.Is_Empty
       then (Athena.Movers.World_Orbit,
             Athena.Handles.World.Get (Vector (Fleet.Reference).World))
       else Fleet.First_Ship.Location);

   overriding function Has_Destination
     (Fleet : Fleet_Handle)
      return Boolean
   is (not Fleet.Is_Empty and then Fleet.First_Ship.Has_Destination);

   overriding function Destination
     (Fleet : Fleet_Handle)
      return Athena.Movers.Mover_Location
   is (Fleet.First_Ship.Destination);

   overriding function Progress
     (Fleet : Fleet_Handle)
      return Unit_Real
   is (Fleet.First_Ship.Progress);

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
      World : Athena.Handles.World.World_Handle;
      Owner : Athena.Handles.Empire.Empire_Handle)
      return Fleet_Handle
   is
      Reference : Fleet_Reference;
   begin
      Vector.Append
        (Fleet_Record'
           (Identifier    => Next_Identifier,
            World         => World.Reference,
            Owner         => Owner.Reference,
            Name          => +Name,
            Ships         => <>),
         Index => Reference);
      Owner.Add_Fleet (Reference);
      return Get (Reference);
   end Create;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Process : not null access procedure
        (Fleet   : Fleet_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end Iterate_All;

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
      Vector.Read (Stream);
   end Load;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Fleet       : Fleet_Handle;
      Destination : Athena.Handles.Star.Star_Handle)
   is
   begin
      for Ship_Ref of Vector (Fleet.Reference).Ships loop
         Athena.Handles.Ship.Actions.Move_To_Star
           (Ship => Athena.Handles.Ship.Get (Ship_Ref),
            Star => Destination);
      end loop;
   end Move_To;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Fleet       : Fleet_Handle;
      Destination : Athena.Handles.World.World_Handle)
   is
   begin
      if Fleet.Is_Empty then
         Vector (Fleet.Reference).World := Destination.Reference;
      else
         for Ship_Ref of Vector (Fleet.Reference).Ships loop
            Athena.Handles.Ship.Actions.Move_To_World
              (Ship  => Athena.Handles.Ship.Get (Ship_Ref),
               World => Destination);
         end loop;
      end if;
   end Move_To;

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
      Vector.Write (Stream);
   end Save;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Fleet    : Fleet_Handle;
      New_Name : String)
   is
      procedure Update (Rec : in out Fleet_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Fleet_Record) is
      begin
         Rec.Name := +New_Name;
      end Update;

   begin
      Vector.Update (Fleet.Reference, Update'Access);
   end Set_Name;

   ----------------
   -- Short_Name --
   ----------------

   overriding function Short_Name
     (Fleet : Fleet_Handle)
      return String
   is
   begin
      return Fleet.Name & Fleet.Ship_Count'Image
        & " ships "
        & (if Fleet.Has_Destination
           then "travelling to " & Fleet.Destination_Name
           else Fleet.Current_Location_Name);
   end Short_Name;

end Athena.Handles.Fleet;
