with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Athena.Money;
with Athena.Random;

with Athena.Server;

with Athena.Empires;

with Athena.Updates.Events;

package body Athena.Handles.Colony is

   Signal_Colony_Owner_Changed : constant String :=
                                   "signal-colony-owner-changed";

   type Colony_Owner_Changed_Data is
     new Athena.Signals.Signal_Data_Interface with
      record
         Old_Owner : Athena.Handles.Empire.Empire_Handle;
         New_Owner : Athena.Handles.Empire.Empire_Handle;
      end record;

   package Colony_Action_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Colony_Action'Class);

   package Stock_Vectors is
     new Ada.Containers.Vectors (Real_Commodity_Reference, Non_Negative_Real);

   type Production_Record is
      record
         Size       : Non_Negative_Real;
         Employment : Non_Negative_Real;
      end record;

   package Production_Vectors is
     new Ada.Containers.Vectors (Real_Production_Reference, Production_Record);

   type Colony_Record is
      record
         Identifier  : Object_Identifier;
         Star        : Star_Reference;
         Owner       : Empire_Reference;
         Founded     : Athena.Calendar.Time;
         Next_Update : Athena.Calendar.Time;
         Construct   : Non_Negative_Real := 0.0;
         Pop         : Non_Negative_Real := 0.0;
         Industry    : Non_Negative_Real := 0.0;
         Material    : Non_Negative_Real := 0.0;
         Actions     : Colony_Action_Lists.List;
         Stock       : Stock_Vectors.Vector;
         Production  : Production_Vectors.Vector;
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

   function Founded
     (Colony : Colony_Handle)
      return Athena.Calendar.Time
   is (Vector (Colony.Reference).Founded);

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

   overriding function Get_Stock
     (Colony    : Colony_Handle;
      Commodity : Athena.Handles.Commodity.Commodity_Handle'Class)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Stock (Commodity.Reference));

   function Production_Size
     (Colony     : Colony_Handle;
      Production : Athena.Handles.Production.Production_Handle)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Production (Production.Reference).Size);

   function Production_Employment
     (Colony     : Colony_Handle;
      Production : Athena.Handles.Production.Production_Handle)
      return Non_Negative_Real
   is (Vector (Colony.Reference).Production (Production.Reference).Employment);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Colony : Colony_Handle)
   is
      Food_Stock : constant Non_Negative_Real :=
                     Colony.Get_Stock (Athena.Handles.Commodity.Food);
      Water_Stock : constant Non_Negative_Real :=
                      Colony.Get_Stock (Athena.Handles.Commodity.Water);
      Environment_Water : constant Non_Negative_Real :=
                            Colony.Star.Habitability ** 2
                              * 1.0e10;

      function Calculate_New_Population return Real;

      ------------------------------
      -- Calculate_New_Population --
      ------------------------------

      function Calculate_New_Population return Real is
         Required_Water : constant Non_Negative_Real :=
                            Colony.Population / 500.0;
         Available_Water : constant Non_Negative_Real :=
                             Water_Stock + Environment_Water;
         Required_Food   : constant Non_Negative_Real :=
                             Colony.Population / 400.0;
         Available_Food  : constant Non_Negative_Real :=
                             Food_Stock;
         Base_Increase   : constant Real :=
                             (Colony.Star.Habitability - 0.2)
                             * 0.02 / 360.0;
      begin
         if Available_Water < Required_Water then
            Colony.Log ("insufficient water "
                        & Image (Available_Water)
                        & "t");
            Colony.Set_Stock (Athena.Handles.Commodity.Water, 0.0);
            return -(Required_Water - Available_Water);
         elsif Environment_Water < Required_Water then
            Colony.Log ("consuming water from stock "
                        & Image (Required_Water - Environment_Water)
                        & "t");
            Colony.Remove_Stock (Athena.Handles.Commodity.Water,
                                 Required_Water - Environment_Water);
            return 0.0;
         end if;

         if Available_Food < Required_Food then
            Colony.Log ("insufficient food "
                        & Image (Available_Food)
                        & "t");
            Colony.Remove_Stock
              (Athena.Handles.Commodity.Food, Available_Food / 4.0);
            return -(Colony.Population / 100.0);
         else
            Colony.Log ("consume food "
                        & Image (Required_Food)
                        & "t");
            Colony.Remove_Stock (Athena.Handles.Commodity.Food, Required_Food);
            return Colony.Population * Base_Increase;
         end if;
      end Calculate_New_Population;

   begin
      Colony.Log ("activating");
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
                       Colony.Population + Calculate_New_Population;
      begin
         Colony.Set_Construct (Construct / 100.0);
         Colony.Set_Population (New_Pop);
         Colony.Log
           ("update: construction = " & Image (Colony.Construct)
            & "; pop = " & Image (Colony.Population));
         Athena.Updates.Events.Update_With_Delay
           (Athena.Calendar.Days (1.0), Colony);
      end;

      declare
         Rec : Colony_Record renames Vector (Colony.Reference);
      begin
         for Ref in 1 .. Rec.Production.Last_Index loop
            declare
               Production : constant Handles.Production.Production_Handle :=
                              Athena.Handles.Production.Get (Ref);
            begin
               if Rec.Production (Ref).Size > 0.0 then
                  Production.Daily_Production
                    (Size  => Rec.Production (Ref).Size,
                     Star  => Colony.Star,
                     Stock => Colony);
               end if;
            end;
         end loop;
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

   --------------------------
   -- Colony_Owner_Changed --
   --------------------------

   function Colony_Owner_Changed return Athena.Signals.Signal_Type is
   begin
      return Athena.Signals.Signal (Signal_Colony_Owner_Changed);
   end Colony_Owner_Changed;

   --------------------------
   -- Colony_Owner_Changed --
   --------------------------

   procedure Colony_Owner_Changed
     (Colony    : Colony_Handle;
      Old_Owner : Athena.Handles.Empire.Empire_Handle;
      New_Owner : Athena.Handles.Empire.Empire_Handle)
   is
   begin
      Athena.Server.Emit
        (Source      => Colony,
         Signal      => Colony_Owner_Changed,
         Signal_Data =>
           Colony_Owner_Changed_Data'
             (Old_Owner => Old_Owner,
              New_Owner => New_Owner));
   end Colony_Owner_Changed;

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

      Stock : Stock_Vectors.Vector;
      Production : Production_Vectors.Vector;

   begin

      for Item of Athena.Handles.Commodity.All_Commodities loop
         Stock.Append (0.0);
      end loop;

      for Item of Athena.Handles.Production.All_Production loop
         Production.Append (Production_Record'
                              (Size       => 0.0,
                               Employment => 0.0));
      end loop;

      Vector.Append
        (Colony_Record'
           (Identifier  => Next_Identifier,
            Star        => Star.Reference,
            Owner       => Owner.Reference,
            Founded     => Athena.Calendar.Clock,
            Next_Update =>
              Athena.Calendar.Clock
            + Athena.Calendar.Days (Athena.Random.Unit_Random),
            Construct   => 0.0,
            Pop         => Pop,
            Industry    => Industry,
            Material    => Material,
            Actions     => Colony_Action_Lists.Empty_List,
            Stock       => Stock,
            Production  => Production));
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

   --------------------
   -- Set_Production --
   --------------------

   procedure Set_Production
     (Colony     : Colony_Handle;
      Production : Athena.Handles.Production.Production_Handle;
      Fraction   : Unit_Real)
   is
      Pop : constant Non_Negative_Real := Colony.Population * Fraction;
      Size : constant Non_Negative_Real := Pop / Production.Employment;
   begin
      Vector (Colony.Reference).Production (Production.Reference) :=
        Production_Record'
          (Size       => Size,
           Employment => Pop);
   end Set_Production;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (Colony    : Colony_Handle;
      Commodity : Athena.Handles.Commodity.Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      Vector (Colony.Reference).Stock (Commodity.Reference) := Quantity;
      Colony.Log (Commodity.Tag & ": " & Image (Quantity));
   end Set_Stock;

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

begin

   Athena.Signals.Create_Signal (Signal_Colony_Owner_Changed);

end Athena.Handles.Colony;
