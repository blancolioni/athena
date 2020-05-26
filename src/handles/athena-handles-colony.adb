with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Athena.Calendar;
with Athena.Money;
with Athena.Random;

with Athena.Empires;

with Athena.Updates.Events;

package body Athena.Handles.Colony is

   package Colony_Action_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Colony_Action'Class);

   type Colony_Record is
      record
         Identifier  : Object_Identifier;
         Star        : Star_Reference;
         Owner       : Empire_Reference;
         Next_Update : Athena.Calendar.Time;
         Construct   : Non_Negative_Real := 0.0;
         Pop         : Non_Negative_Real := 0.0;
         Industry    : Non_Negative_Real := 0.0;
         Material    : Non_Negative_Real := 0.0;
         Actions     : Colony_Action_Lists.List;
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

   function Has_Actions
     (Colony : Colony_Handle)
      return Boolean
   is (not Vector (Colony.Reference).Actions.Is_Empty);

   function First_Action
     (Colony : Colony_Handle)
      return Root_Colony_Action'Class
   is (Vector (Colony.Reference).Actions.First_Element);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Colony : Colony_Handle)
   is
   begin
      if Colony.Construct > 0.0 then
         Athena.Empires.Earn
           (Colony.Owner, Athena.Money.To_Money (Colony.Construct),
            "unused production on " & Colony.Star.Name);
      end if;

      declare
         Construct : constant Non_Negative_Real :=
                       Real'Min (Colony.Industry, Colony.Population)
                       + (Real'Max (Colony.Industry, Colony.Population)
                          - Colony.Industry)
                       / 4.0;
         New_Pop   : constant Non_Negative_Real :=
                       Colony.Population
                         + 0.1 * Colony.Population * Colony.Star.Habitability
                       / 360.0;
         Max_Pop   : constant Non_Negative_Real :=
                       Non_Negative_Real (Colony.Star.Space);
      begin
         Colony.Set_Construct (Construct / 360.0);
         Colony.Set_Population (Real'Min (New_Pop, Max_Pop));
         Colony.Log
           ("update: construction = " & Image (Colony.Construct)
            & "; pop = " & Image (Colony.Population));
         Athena.Updates.Events.Update_With_Delay
           (Athena.Calendar.Days (1.0), Colony);
      end;

      if Colony.Has_Actions then
         declare
            Action : constant Root_Colony_Action'Class :=
                       Colony.First_Action;
         begin
            if Action.Execute (Colony) then
               Colony.Delete_First_Action;
            end if;
         end;
      end if;

   end Activate;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Colony   : Colony_Handle;
      Action   : Root_Colony_Action'Class)
   is
   begin
      Vector (Colony.Reference).Actions.Append (Action);
   end Add_Action;

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
      use type Athena.Calendar.Time;
   begin
      Vector.Append
        (Colony_Record'
           (Identifier  => Next_Identifier,
            Star        => Star.Reference,
            Owner       => Owner.Reference,
            Next_Update =>
              Athena.Calendar.Clock
            + Athena.Calendar.Days (Athena.Random.Unit_Random),
            Construct   => 0.0,
            Pop         => Pop,
            Industry    => Industry,
            Material    => Material,
            Actions     => Colony_Action_Lists.Empty_List));
      Update_Maps (Vector.Last_Index);

      return Handle : constant Colony_Handle := (True, Vector.Last_Index) do
         Athena.Updates.Events.Update_At
           (Vector.Last_Element.Next_Update,
            Handle);
      end return;
   end Create;

   -------------------------
   -- Delete_First_Action --
   -------------------------

   procedure Delete_First_Action (Colony : Colony_Handle) is
   begin
      Vector (Colony.Reference).Actions.Delete_First;
   end Delete_First_Action;

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
         Athena.Updates.Events.Update_At
           (Vector (Ref).Next_Update,
            Colony_Handle'(True, Ref));
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
