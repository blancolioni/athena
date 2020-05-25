with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

package body Athena.Handles.Colony is

   type Colony_Record is
      record
         Identifier : Object_Identifier;
         Star       : Star_Reference;
         Owner      : Empire_Reference;
         Construct  : Non_Negative_Real := 0.0;
         Pop        : Non_Negative_Real := 0.0;
         Industry   : Non_Negative_Real := 0.0;
         Material   : Non_Negative_Real := 0.0;
      end record;

   package Colony_Vectors is
     new Ada.Containers.Vectors
       (Real_Colony_Reference, Colony_Record);

   package Colony_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Colony_Reference);

   package Star_Colony_Maps is
     new Ada.Containers.Ordered_Maps
       (Star_Reference, Colony_Lists.List, "<", Colony_Lists."=");

   package Empire_Colony_Maps is
     new Ada.Containers.Ordered_Maps
       (Empire_Reference, Colony_Lists.List, "<", Colony_Lists."=");

   Vector     : Colony_Vectors.Vector;
   Star_Map   : Star_Colony_Maps.Map;
   Empire_Map : Empire_Colony_Maps.Map;

   procedure Update_Maps
     (Colony : Colony_Reference);

   function Star
     (Colony : Colony_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Colony.Reference).Star));

   function Owner
     (Colony : Colony_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (Colony.Reference).Owner));

   function Construct
     (Colony : Colony_Handle)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Construct);

   function Population
     (Colony : Colony_Handle)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Pop);

   function Industry
     (Colony : Colony_Handle)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Industry);

   function Material
     (Colony : Colony_Handle)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Material);

   ------------
   -- Create --
   ------------

   function Create
     (Star      : Athena.Handles.Star.Star_Handle;
      Owner     : Athena.Handles.Empire.Empire_Handle;
      Pop       : Non_Negative_Real := 0.0;
      Industry  : Non_Negative_Real := 0.0;
      Material  : Non_Negative_Real := 0.0)
      return Colony_Handle
   is
   begin
      Vector.Append
        (Colony_Record'
           (Identifier => Next_Identifier,
            Star       => Star.Reference,
            Owner      => Owner.Reference,
            Construct  => 0.0,
            Pop        => Pop,
            Industry   => Industry,
            Material   => Material));
      Update_Maps (Vector.Last_Index);

      return (True, Vector.Last_Index);
   end Create;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Process : not null access
        procedure (Colony : Colony_Handle))
   is
   begin
      for I in 1 .. Vector.Last_Index loop
         Process (Get (I));
      end loop;
   end Iterate_All;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Colony_Vectors.Vector'Read (Stream, Vector);
      for Ref in 1 .. Vector.Last_Index loop
         Update_Maps (Ref);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Colony_Vectors.Vector'Write (Stream, Vector);
   end Save;

   -------------------
   -- Set_Construct --
   -------------------

   procedure Set_Construct
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real)
   is
   begin
      Vector (Colony.Reference).Construct := Quantity;
   end Set_Construct;

   ------------------
   -- Set_Industry --
   ------------------

   procedure Set_Industry
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real)
   is
   begin
      Vector (Colony.Reference).Industry := Quantity;
   end Set_Industry;

   ------------------
   -- Set_Material --
   ------------------

   procedure Set_Material
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real)
   is
   begin
      Vector (Colony.Reference).Material := Quantity;
   end Set_Material;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Colony    : Colony_Handle;
      New_Owner : Athena.Handles.Empire.Empire_Handle)
   is
   begin
      Vector (Colony.Reference).Owner := New_Owner.Reference;
   end Set_Owner;

   --------------------
   -- Set_Population --
   --------------------

   procedure Set_Population
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real)
   is
   begin
      Vector (Colony.Reference).Pop := Quantity;
   end Set_Population;

   -----------------
   -- Update_Maps --
   -----------------

   procedure Update_Maps
     (Colony : Colony_Reference)
   is
      Rec : Colony_Record renames Vector (Colony);
   begin
      if not Star_Map.Contains (Rec.Star) then
         Star_Map.Insert (Rec.Star, Colony_Lists.Empty_List);
      end if;
      if not Empire_Map.Contains (Rec.Owner) then
         Empire_Map.Insert (Rec.Owner, Colony_Lists.Empty_List);
      end if;

      Star_Map (Rec.Star).Append (Vector.Last_Index);
      Empire_Map (Rec.Owner).Append (Vector.Last_Index);

   end Update_Maps;

end Athena.Handles.Colony;
