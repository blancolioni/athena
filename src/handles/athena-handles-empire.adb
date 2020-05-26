with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Athena.Calendar;
with Athena.Random;

with Athena.Updates.Events;

with Athena.Handles.Ship;
with Athena.Handles.Star;

package body Athena.Handles.Empire is

   package Manager_Holders is
     new Ada.Containers.Indefinite_Holders
       (Athena.Managers.Root_Manager_Type'Class,
        Athena.Managers."=");

   type Manager_Array is
     array (Athena.Handles.Manager_Class) of Manager_Holders.Holder;

   type Design_Array is
     array (Standard_Empire_Design) of Design_Reference;

   package Colony_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Colony_Reference);

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type Manager_Update is
     new Athena.Updates.Update_Interface with
      record
         Empire  : Empire_Handle;
         Manager : Manager_Class;
      end record;

   overriding procedure Activate
     (Update : Manager_Update);

   type Empire_Record is
      record
         Identifier : Object_Identifier;
         Home       : Star_Reference;
         Capital    : Colony_Reference;
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Plural     : Ada.Strings.Unbounded.Unbounded_String;
         Adjective  : Ada.Strings.Unbounded.Unbounded_String;
         Color      : Athena.Color.Athena_Color;
         Cash       : Athena.Money.Money_Type;
         Debt       : Athena.Money.Money_Type;
         Interest   : Unit_Real;
         Managers   : Manager_Array;
         Designs    : Design_Array;
         Colonies   : Colony_Lists.List;
         Ships      : Ship_Lists.List;
         Knowledge  : Knowledge_Reference;
      end record;

   package Empire_Vectors is
     new Ada.Containers.Vectors
       (Real_Empire_Reference, Empire_Record);

   package Empire_Maps is
     new WL.String_Maps (Real_Empire_Reference);

   Vector : Empire_Vectors.Vector;
   Map : Empire_Maps.Map;

   function Name (Empire : Empire_Handle) return String
   is (-(Vector (Empire.Reference).Name));

   function Adjective (Empire : Empire_Handle) return String
   is (-(Vector (Empire.Reference).Adjective));

   function Plural (Empire : Empire_Handle) return String
   is (-(Vector (Empire.Reference).Plural));

   function Capital
     (Empire : Empire_Handle)
      return Colony_Reference
   is (Vector (Empire.Reference).Capital);

   function Color
     (Empire : Empire_Handle)
      return Athena.Color.Athena_Color
   is (Vector (Empire.Reference).Color);

   function Cash
     (Empire : Empire_Handle)
      return Athena.Money.Money_Type
   is (Vector (Empire.Reference).Cash);

   function Debt
     (Empire : Empire_Handle)
      return Athena.Money.Money_Type
   is (Vector (Empire.Reference).Debt);

   function Has_Manager
     (Empire  : Empire_Handle;
      Manager : Athena.Handles.Manager_Class)
      return Boolean
   is (not Vector (Empire.Reference).Managers (Manager).Is_Empty);

   function Standard_Design
     (Empire : Empire_Handle;
      Class  : Standard_Empire_Design)
      return Design_Reference
   is (Vector (Empire.Reference).Designs (Class));

   function Current_Tec_Level
     (Empire     : Empire_Handle;
      Technology : Athena.Handles.Technology.Technology_Handle)
      return Non_Negative_Real
   is (0.0);

   function Current_Tec_Investment
     (Empire     : Empire_Handle;
      Technology : Athena.Handles.Technology.Technology_Handle)
      return Non_Negative_Real
   is (0.0);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Manager_Update)
   is
      M : Athena.Managers.Root_Manager_Type'Class renames
            Vector (Update.Empire.Reference)
            .Managers (Update.Manager)
            .Reference;
   begin
      M.Log ("activating");
      M.Create_Orders;
      Athena.Updates.Events.Update_At
        (Clock  => M.Next_Update,
         Update => Update);
   end Activate;

   ----------------
   -- Add_Colony --
   ----------------

   procedure Add_Colony
     (Empire : Empire_Handle;
      Colony : Colony_Reference)
   is
   begin
      Vector (Empire.Reference).Colonies.Append (Colony);
   end Add_Colony;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (Empire : Empire_Handle;
      Ship   : Ship_Reference)
   is
   begin
      Vector (Empire.Reference).Ships.Append (Ship);
   end Add_Ship;

   -------------------
   -- Create_Empire --
   -------------------

   function Create_Empire
     (Star      : Star_Reference;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Color     : Athena.Color.Athena_Color;
      Cash      : Athena.Money.Money_Type;
      Debt      : Athena.Money.Money_Type)
      return Empire_Handle
   is
   begin
      Vector.Append
        (Empire_Record'
           (Identifier => Next_Identifier,
            Name       => +Name,
            Home       => Star,
            Capital    => Null_Colony_Reference,
            Plural     => +Plural,
            Adjective  => +Adjective,
            Color      => Color,
            Cash       => Cash,
            Debt       => Debt,
            Interest   => 0.1,
            Managers   => (others => <>),
            Designs    => (others => 0),
            Colonies   => <>,
            Ships      => <>,
            Knowledge  => <>));
      Map.Insert (Name, Vector.Last_Index);
      Athena.Handles.Star.Get (Star).Set_Owner (Vector.Last_Index);

      Vector (Vector.Last_Index).Knowledge :=
        Athena.Handles.Knowledge.Create (Vector.Last_Index).Reference;

      return Empire : constant Empire_Handle := (True, Vector.Last_Index);

   end Create_Empire;

   -----------------
   -- Find_Colony --
   -----------------

   function Find_Colony
     (Empire  : Empire_Handle;
      Test    : not null access
        function (Colony : Colony_Reference) return Boolean)
      return Colony_Reference
   is
   begin
      for Reference of Vector (Empire.Reference).Colonies loop
         if Test (Reference) then
            return Reference;
         end if;
      end loop;
      return Null_Colony_Reference;
   end Find_Colony;

   -----------------
   -- Get_By_Name --
   -----------------

   function Get_By_Name (Name : String) return Empire_Handle is
      use Empire_Maps;
      Position : constant Cursor := Map.Find (Name);
   begin
      return Handle : Empire_Handle do
         if Has_Element (Position) then
            Handle.Reference := Element (Position);
            Handle.Has_Element := True;
         end if;
      end return;
   end Get_By_Name;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Empire : Empire_Handle)
      return Object_Identifier
   is
   begin
      return Vector (Empire.Reference).Identifier;
   end Identifier;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Process : not null access
        procedure (Empire  : Empire_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process ((True, Reference));
      end loop;
   end Iterate_All;

   ----------------------
   -- Iterate_Colonies --
   ----------------------

   procedure Iterate_Colonies
     (Empire  : Empire_Handle;
      Process : not null access
        procedure (Colony : Colony_Reference))
   is
   begin
      for Colony of Vector (Empire.Reference).Colonies loop
         Process (Colony);
      end loop;
   end Iterate_Colonies;

   ---------------------------
   -- Iterate_Managed_Ships --
   ---------------------------

   procedure Iterate_Managed_Ships
     (Empire  : Empire_Handle;
      Manager : Manager_Class;
      Process : not null access
        procedure (Ship : Ship_Reference))
   is
      procedure Process_If_Managed
        (Reference : Ship_Reference);

      ------------------------
      -- Process_If_Managed --
      ------------------------

      procedure Process_If_Managed
        (Reference : Ship_Reference)
      is
         Ship : constant Athena.Handles.Ship.Ship_Handle :=
            Athena.Handles.Ship.Get (Reference);
      begin
         if Ship.Has_Manager
           and then Ship.Manager = Manager
         then
            Process (Ship.Reference);
         end if;
      end Process_If_Managed;

   begin
      Empire.Iterate_Ships (Process_If_Managed'Access);
   end Iterate_Managed_Ships;

   -------------------
   -- Iterate_Ships --
   -------------------

   procedure Iterate_Ships
     (Empire  : Empire_Handle;
      Process : not null access
        procedure (Ship : Ship_Reference))
   is
   begin
      for Ship of Vector (Empire.Reference).Ships loop
         Process (Ship);
      end loop;
   end Iterate_Ships;

   ---------------
   -- Knowledge --
   ---------------

   function Knowledge
     (Empire : Empire_Handle)
      return Athena.Handles.Knowledge.Knowledge_Handle
   is
   begin
      return Result : constant Athena.Handles.Knowledge.Knowledge_Handle :=
        Athena.Handles.Knowledge.Get
          (Vector (Empire.Reference).Knowledge)
      do
         if not Result.Is_Loaded then
            Result.Load;
         end if;
      end return;
   end Knowledge;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Empire_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         declare
            Rec : Empire_Record renames Vector (I);
         begin
            Map.Insert
              (Ada.Strings.Unbounded.To_String (Rec.Name), I);
            for M in Manager_Class loop
               if not Rec.Managers (M).Is_Empty then
                  Athena.Updates.Events.Update_At
                    (Clock  => Rec.Managers (M).Element.Next_Update,
                     Update =>
                       Manager_Update'
                         (Get (I), M));
               end if;
            end loop;
         end;
      end loop;
   end Load;

   -------------------
   -- Remove_Colony --
   -------------------

   procedure Remove_Colony
     (Empire : Empire_Handle;
      Colony : Colony_Reference)
   is
      use Colony_Lists;
      Colonies : List renames Vector (Empire.Reference).Colonies;
      Position : Cursor := Colonies.Find (Colony);
   begin
      pragma Assert (Has_Element (Position),
                     Empire.Name & ": does not contain colony"
                     & Colony'Image);
      Colonies.Delete (Position);
   end Remove_Colony;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Empire : Empire_Handle;
      Ship : Ship_Reference)
   is
      use Ship_Lists;
      Ships    : List renames Vector (Empire.Reference).Ships;
      Position : Cursor := Ships.Find (Ship);
   begin
      pragma Assert (Has_Element (Position),
                     Empire.Name & ": does not contain ship"
                     & Ship'Image);
      Ships.Delete (Position);
   end Remove_Ship;

   -----------------
   -- Run_Manager --
   -----------------

   procedure Run_Manager
     (Empire : Empire_Handle;
      Manager : Athena.Handles.Manager_Class)
   is
      Rec : Empire_Record renames
              Vector (Empire.Reference);
   begin
      Rec.Managers (Manager).Reference.Create_Orders;
   end Run_Manager;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Empire_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Empire  : Empire_Handle;
      To      : Manager_Class;
      Message : Athena.Managers.Message_Type'Class)
   is
      Rec : Empire_Record renames
              Vector (Empire.Reference);
   begin
      if not Rec.Managers (To).Is_Empty then
         Rec.Managers (To).Reference.Send_Message (Message);
      end if;
   end Send_Message;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Empire  : Empire_Handle;
      To      : Manager_Class)
   is
      Rec : Empire_Record renames
        Vector (Empire.Reference);
   begin
      if not Rec.Managers (To).Is_Empty then
         Rec.Managers (To).Reference.Set_Next_Update_Delay (0.0);
         Athena.Updates.Events.Update_With_Delay
           (0.0,
            Manager_Update'
              (Empire  => Empire,
               Manager => To));
      end if;
   end Send_Signal;

   -----------------
   -- Set_Capital --
   -----------------

   procedure Set_Capital
     (Empire  : Empire_Handle;
      Capital : Colony_Reference)
   is
   begin
      Vector (Empire.Reference).Capital := Capital;
   end Set_Capital;

   --------------
   -- Set_Cash --
   --------------

   procedure Set_Cash
     (Empire : Empire_Handle;
      Cash   : Athena.Money.Money_Type)
   is
   begin
      Vector (Empire.Reference).Cash := Cash;
   end Set_Cash;

   --------------
   -- Set_Debt --
   --------------

   procedure Set_Debt
     (Empire : Empire_Handle;
      Debt   : Athena.Money.Money_Type)
   is
   begin
      Vector (Empire.Reference).Debt := Debt;
   end Set_Debt;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (Empire  : Empire_Handle;
      Manager : Athena.Handles.Manager_Class;
      To      : Athena.Managers.Root_Manager_Type'Class)
   is
      Rec : Empire_Record renames
              Vector (Empire.Reference);
   begin
      Rec.Managers (Manager) :=
        Manager_Holders.To_Holder (To);
      Rec.Managers (Manager).Reference.Set_Next_Update_Delay
        (Athena.Calendar.Days (Athena.Random.Unit_Random));
      Athena.Updates.Events.Update_At
        (Rec.Managers (Manager).Element.Next_Update,
         Manager_Update'
           (Empire  => Empire,
            Manager => Manager));
   end Set_Manager;

   -------------------------
   -- Set_Standard_Design --
   -------------------------

   procedure Set_Standard_Design
     (Empire : Empire_Handle;
      Class  : Standard_Empire_Design;
      Design : Design_Reference)
   is
      Rec : Empire_Record renames
              Vector (Empire.Reference);
   begin
      Rec.Designs (Class) := Design;
   end Set_Standard_Design;

   ----------------
   -- Update_Tec --
   ----------------

   procedure Update_Tec
     (Empire         : Empire_Handle;
      Technology     : Athena.Handles.Technology.Technology_Handle;
      New_Level      : Non_Negative_Real;
      New_Investment : Non_Negative_Real)
   is null;

end Athena.Handles.Empire;
