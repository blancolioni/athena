with Ada.Containers.Vectors;

package body Athena.Handles.War is

   type War_Record is
      record
         Identifier  : Object_Identifier;
         Attacker    : Empire_Reference;
         Defender    : Empire_Reference;
         Victor      : Empire_Reference;
         Start       : Athena_Turn_Number;
         Finish      : Athena_Turn_Number;
      end record;

   package War_Vectors is
     new Ada.Containers.Vectors
       (Real_War_Reference, War_Record);

   Vector : War_Vectors.Vector;

   overriding function Identifier
     (War : War_Handle)
      return Object_Identifier
   is (Vector (War.Reference).Identifier);

   function Attacker
     (War : War_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (War.Reference).Attacker));

   function Defender
     (War : War_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (War.Reference).Defender));

   function Start
     (War : War_Handle)
      return Athena_Turn_Number
   is (Vector (War.Reference).Start);

   function Active
     (War : War_Handle)
      return Boolean
   is (Vector (War.Reference).Finish = 0);

   function Finish
     (War : War_Handle)
      return Athena_Turn_Number
   is (Vector (War.Reference).Finish);

   ------------
   -- Create --
   ------------

   procedure Create
     (Attacker : Athena.Handles.Empire.Empire_Handle;
      Defender : Athena.Handles.Empire.Empire_Handle)
   is
   begin
      Vector.Append
        (War_Record'
           (Identifier => Next_Identifier,
            Attacker   => Attacker.Reference,
            Defender   => Defender.Reference,
            Victor     => Null_Empire_Reference,
            Start      => Current_Turn,
            Finish     => 0));
   end Create;

   --------------
   -- Find_War --
   --------------

   function Find_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
      return War_Handle
   is
   begin
      for Reference in 1 ..  Vector.Last_Index loop
         declare
            Rec : War_Record renames Vector (Reference);
         begin
            if Rec.Finish = 0 then
               if (Rec.Attacker = E1.Reference
                   and then Rec.Defender = E2.Reference)
                 or else (Rec.Attacker = E2.Reference
                          and then Rec.Defender = E1.Reference)
               then
                  return Get (Reference);
               end if;
            end if;
         end;
      end loop;
      return Empty_Handle;
   end Find_War;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      War_Vectors.Vector'Read (Stream, Vector);
   end Load;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (War    : War_Handle;
      Victor : Athena.Handles.Empire.Empire_Handle)
   is
      Rec : War_Record renames Vector (War.Reference);
   begin
      Rec.Finish := Current_Turn;
      Rec.Victor := Victor.Reference;
   end Resolve;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      War_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ----------------
   -- Short_Name --
   ----------------

   overriding function Short_Name
     (War : War_Handle)
      return String
   is
      Rec : War_Record renames Vector (War.Reference);
   begin
      return Athena.Handles.Empire.Get (Rec.Attacker).Name
        & "/" & Athena.Handles.Empire.Get (Rec.Defender).Name
        & " war";
   end Short_Name;

end Athena.Handles.War;
