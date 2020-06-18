with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with Athena.Calendar;

package body Athena.Handles.Encounter is

   package Empire_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Empire_Reference);

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type Encounter_Record is
      record
         Star        : Star_Reference;
         Active      : Boolean;
         Start       : Athena.Calendar.Time;
         Finish      : Athena.Calendar.Time;
         Antagonists : Empire_Lists.List;
         Actors      : Ship_Lists.List;
         Victor      : Empire_Reference;
      end record;

   package Encounter_Vectors is
     new Ada.Containers.Vectors
       (Real_Encounter_Reference, Encounter_Record);

   Vector : Encounter_Vectors.Vector;

   function Star
     (Handle : Encounter_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Handle.Reference).Star));

   ---------------
   -- Add_Actor --
   ---------------

   procedure Add_Actor
     (Handle : Encounter_Handle;
      Actor  : Athena.Handles.Ship.Ship_Handle'Class)
   is
   begin
      Handle.Log ("adding " & Actor.Short_Name);
      Vector (Handle.Reference).Antagonists.Append (Actor.Owner.Reference);
      Vector (Handle.Reference).Actors.Append (Actor.Reference);
   end Add_Actor;

   ------------
   -- Create --
   ------------

   function Create
     (Star : Athena.Handles.Star.Star_Handle)
      return Encounter_Handle
   is
   begin
      Vector.Append
        (Encounter_Record'
           (Star        => Star.Reference,
            Active      => True,
            Start       => Athena.Calendar.Clock,
            Finish      => Athena.Calendar.Start,
            Victor      => Null_Empire_Reference,
            Antagonists => Empire_Lists.Empty_List,
            Actors      => Ship_Lists.Empty_List));
      return Handle : constant Encounter_Handle := Get (Vector.Last_Index) do
         Handle.Log ("started");
      end return;
   end Create;

   ---------------------------
   -- Find_Active_Encounter --
   ---------------------------

   function Find_Active_Encounter
     (Star      : Athena.Handles.Star.Star_Handle;
      Involving : Athena.Handles.Empire.Empire_Handle)
      return Encounter_Handle
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         declare
            Rec : Encounter_Record renames Vector (Reference);
         begin
            if Rec.Active
              and then Rec.Star = Star.Reference
              and then Rec.Antagonists.Contains (Involving.Reference)
            then
               return Get (Reference);
            end if;
         end;
      end loop;
      return Empty_Handle;
   end Find_Active_Encounter;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Encounter_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Encounter_Vectors.Vector'Write (Stream, Vector);
   end Save;

end Athena.Handles.Encounter;
