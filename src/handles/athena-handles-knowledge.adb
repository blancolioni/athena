with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with WL.String_Maps;

with Athena.Ships;
with Athena.Stars;
with Athena.Treaties;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Ship;

package body Athena.Handles.Knowledge is

   type Neighbour_Record is
      record
         Neighbour : Star_Reference;
         Nearest   : Colony_Reference;
         Distance  : Non_Negative_Real;
      end record;

   package Neighbour_Maps is
     new WL.String_Maps (Neighbour_Record);

   package Neighbour_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Neighbour_Record);

   --  package Star_Maps is
   --    new WL.String_Maps (Star_Reference);
   --
   function Closer (Left, Right : Neighbour_Record) return Boolean
   is (Left.Distance < Right.Distance);

   package Neighbour_Sorting is
     new Neighbour_Lists.Generic_Sorting (Closer);

   type Star_Knowledge_Record is
      record
         Visited    : Boolean := False;
         Colonizing : Boolean := False;
         Owner      : Empire_Reference := Null_Empire_Reference;
         Last_Visit : Athena_Turn_Number := 0;
         Ships      : Known_Ship_Lists.List;
      end record;

   package Star_Knowledge_Maps is
     new Ada.Containers.Ordered_Maps
       (Star_Reference, Star_Knowledge_Record);

   type Knowledge_Record is
      record
         Empire         : Empire_Reference;
         Loaded         : Boolean           := False;
         Stars          : Star_Knowledge_Maps.Map;
         Neighbour_Map  : Neighbour_Maps.Map;
         Neighbour_List : Neighbour_Lists.List;
         Threat_Map     : Neighbour_Maps.Map;
         Threat_List    : Neighbour_Lists.List;
      end record;

   procedure Update_Neighbours
     (Knowledge : in out Knowledge_Record;
      Colony    : Athena.Handles.Colony.Colony_Handle);

   package Knowledge_Vectors is
     new Ada.Containers.Vectors
       (Real_Knowledge_Reference, Knowledge_Record);

   Vector : Knowledge_Vectors.Vector;

   function Get_Star_Knowledge
     (Knowledge : Knowledge_Handle;
      Star      : Star_Reference)
      return Star_Knowledge_Record;

   procedure Update_Star_Knowledge
     (Knowledge : Knowledge_Handle;
      Star      : Star_Reference;
      Rec       : Star_Knowledge_Record);

   overriding function Short_Name
     (Knowledge : Knowledge_Handle)
      return String
   is (Athena.Handles.Empire.Get (Vector (Knowledge.Reference).Empire).Name
       & " Knowledge");

   function Is_Loaded
     (Handle : Knowledge_Handle)
      return Boolean
   is (Vector (Handle.Reference).Loaded);

   function Colonizing
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Boolean
   is (Get_Star_Knowledge (Knowledge, Star.Reference).Colonizing);

   function Last_Visit
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Athena_Turn_Number
   is (Get_Star_Knowledge (Knowledge, Star.Reference).Last_Visit);

   function Visited
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Boolean
   is (Get_Star_Knowledge (Knowledge, Star.Reference).Visited);

   function Get_Known_Ships
     (Knowledge : Knowledge_Handle;
      At_Star   : Athena.Handles.Star.Star_Handle)
      return Known_Ship_Lists.List
   is (Get_Star_Knowledge (Knowledge, At_Star.Reference).Ships);

   ----------------------
   -- Clear_Colonizing --
   ----------------------

   procedure Clear_Colonizing
     (Knowledge  : Knowledge_Handle;
      Star       : Athena.Handles.Star.Star_Handle)
   is
      Rec : Star_Knowledge_Record :=
              Get_Star_Knowledge (Knowledge, Star.Reference);
   begin
      Rec.Colonizing := False;
      Update_Star_Knowledge (Knowledge, Star.Reference, Rec);
   end Clear_Colonizing;

   ------------
   -- Create --
   ------------

   function Create
     (Empire : Empire_Reference)
      return Knowledge_Handle
   is
   begin
      Vector.Append
        (Knowledge_Record'
           (Empire => Empire,
            others => <>));
      return Handle : constant Knowledge_Handle :=
        (True, Vector.Last_Index);
   end Create;

   ------------------------
   -- Get_Star_Knowledge --
   ------------------------

   function Get_Star_Knowledge
     (Knowledge : Knowledge_Handle;
      Star      : Star_Reference)
      return Star_Knowledge_Record
   is
      Position : constant Star_Knowledge_Maps.Cursor :=
                   Vector (Knowledge.Reference).Stars.Find (Star);
   begin
      if Star_Knowledge_Maps.Has_Element (Position) then
         return Star_Knowledge_Maps.Element (Position);
      else
         return Star_Knowledge_Record'
           (others => <>);
      end if;
   end Get_Star_Knowledge;

   ------------------------
   -- Iterate_Neighbours --
   ------------------------

   procedure Iterate_Neighbours
     (Knowledge : Knowledge_Handle;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Neighbour : Athena.Handles.Star.Star_Handle;
                   Nearest   : Colony_Reference;
                   Stop      : out Boolean))
   is
   begin
      for Element of Vector (Knowledge.Reference).Neighbour_List loop
         exit when Element.Distance > Max_Range;

         declare
            Stop : Boolean;
         begin
            Process (Athena.Handles.Star.Get (Element.Neighbour),
                     Element.Nearest, Stop);
            exit when Stop;
         end;
      end loop;
   end Iterate_Neighbours;

   ---------------------
   -- Iterate_Threats --
   ---------------------

   procedure Iterate_Threats
     (Knowledge : Knowledge_Handle;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Threat      : Empire_Reference;
                   Threat_Star : Athena.Handles.Star.Star_Handle;
                   Nearest     : Colony_Reference;
                   Stop        : out Boolean))
   is
   begin
      for Element of Vector (Knowledge.Reference).Neighbour_List loop
         exit when Element.Distance > Max_Range;

         declare
            Stop : Boolean;
         begin
            Process (Athena.Handles.Star.Get (Element.Neighbour).Owner,
                     Athena.Handles.Star.Get (Element.Neighbour),
                     Element.Nearest, Stop);
            exit when Stop;
         end;
      end loop;
   end Iterate_Threats;

   -------------------------
   -- Iterate_Uncolonized --
   -------------------------

   procedure Iterate_Uncolonized
     (Knowledge : Knowledge_Handle;
      Process   : not null access
        procedure (Star      : Athena.Handles.Star.Star_Handle;
                   Stop      : out Boolean))
   is
   begin
      for Position in Vector (Knowledge.Reference).Stars.Iterate loop
         declare
            Star : constant Star_Reference :=
                     Star_Knowledge_Maps.Key (Position);
            Element : constant Star_Knowledge_Record :=
                        Star_Knowledge_Maps.Element (Position);
         begin
            if not Element.Colonizing
              and then Element.Owner = Null_Empire_Reference
            then
               declare
                  Stop : Boolean;
               begin
                  Process (Athena.Handles.Star.Get (Star), Stop);
                  exit when Stop;
               end;
            end if;
         end;
      end loop;
   end Iterate_Uncolonized;

   ----------
   -- Load --
   ----------

   procedure Load
     (Handle : Knowledge_Handle)
   is

      Knowledge : Knowledge_Record renames Vector (Handle.Reference);
      Empire : constant Athena.Handles.Empire.Empire_Handle :=
                 Athena.Handles.Empire.Get (Knowledge.Empire);

      function Greater_Threat (Left, Right : Neighbour_Record) return Boolean;

      --------------------
      -- Greater_Threat --
      --------------------

      function Greater_Threat
        (Left, Right : Neighbour_Record)
         return Boolean
      is
         Left_Star   : constant Athena.Handles.Star.Star_Handle :=
                         Athena.Handles.Star.Get (Left.Neighbour);
         Right_Star  : constant Athena.Handles.Star.Star_Handle :=
                         Athena.Handles.Star.Get (Right.Neighbour);
         Left_War    : constant Boolean :=
                         Left_Star.Has_Owner
                             and then Athena.Treaties.At_War
                               (Empire,
                                Athena.Handles.Empire.Get (Left_Star.Owner));
         Right_War : constant Boolean :=
                         Right_Star.Has_Owner
                             and then Athena.Treaties.At_War
                               (Empire,
                                Athena.Handles.Empire.Get (Right_Star.Owner));
      begin
         if Left_War = Right_War then
            return Closer (Left, Right);
         else
            return Left_War;
         end if;
      end Greater_Threat;

      package Threat_Sorting is
        new Neighbour_Lists.Generic_Sorting (Greater_Threat);

   begin

      Empire.Log
        ("knowledge: clearing"
         & Knowledge.Neighbour_Map.Length'Image
         & " neighbours and"
         & Knowledge.Threat_Map.Length'Image
         & " threats");

      Knowledge.Neighbour_Map.Clear;
      Knowledge.Neighbour_List.Clear;
      Knowledge.Threat_Map.Clear;
      Knowledge.Threat_List.Clear;

      Empire.Log
        ("knowledge: scanning colonies");

      declare
         procedure Update (Reference : Colony_Reference);

         ------------
         -- Update --
         ------------

         procedure Update (Reference : Colony_Reference) is
         begin
            Update_Neighbours
              (Knowledge, Athena.Handles.Colony.Get (Reference));
         end Update;

      begin
         Empire.Iterate_Colonies
           (Update'Access);
      end;

      Empire.Log
        ("knowledge: sorting neighbours");

      for Neighbour of Knowledge.Neighbour_Map loop
         Knowledge.Neighbour_List.Append (Neighbour);
      end loop;

      Neighbour_Sorting.Sort (Knowledge.Neighbour_List);

      Empire.Log
        ("knowledge: finding threats");

      for Element of Knowledge.Neighbour_List loop
         declare
            Star : constant Athena.Handles.Star.Star_Handle :=
                     Athena.Handles.Star.Get (Element.Neighbour);
         begin
            if Star.Has_Owner then
               declare
                  Threat : constant Athena.Handles.Empire.Empire_Handle :=
                             Athena.Handles.Empire.Get (Star.Owner);
                  Id     : constant Object_Identifier :=
                             Threat.Identifier;
               begin
                  if not Knowledge.Threat_Map.Contains (Id) then
                     Knowledge.Threat_Map.Insert (Id, Element);
                     Knowledge.Threat_List.Append (Element);
                  end if;
               end;
            end if;
         end;
      end loop;

      Threat_Sorting.Sort (Knowledge.Threat_List);

      Knowledge.Loaded := True;

      Empire.Log
        ("knowledge: done");

   end Load;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Knowledge_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Knowledge_Vectors.Vector'Write (Stream, Vector);
   end Save;

   --------------------
   -- Set_Colonizing --
   --------------------

   procedure Set_Colonizing
     (Knowledge  : Knowledge_Handle;
      Star       : Athena.Handles.Star.Star_Handle;
      Colonizing : Boolean)
   is
      Rec : Star_Knowledge_Record :=
              Get_Star_Knowledge (Knowledge, Star.Reference);
   begin
      Rec.Colonizing := True;
      Update_Star_Knowledge (Knowledge, Star.Reference, Rec);
   end Set_Colonizing;

   -----------------------
   -- Update_Neighbours --
   -----------------------

   procedure Update_Neighbours
     (Knowledge : in out Knowledge_Record;
      Colony    : Athena.Handles.Colony.Colony_Handle)
   is

      function Update
        (Neighbour : Athena.Handles.Star.Star_Handle)
         return Boolean;

      ------------
      -- Update --
      ------------

      function Update
        (Neighbour : Athena.Handles.Star.Star_Handle)
         return Boolean
      is
         use Neighbour_Maps;
         Position : constant Neighbour_Maps.Cursor :=
                      Knowledge.Neighbour_Map.Find (Neighbour.Identifier);
         Distance : constant Non_Negative_Real :=
                      Athena.Stars.Distance (Neighbour, Colony.Star);
      begin
         if Neighbour.Has_Owner
           and then Neighbour.Owner = Knowledge.Empire
         then
            null;
         elsif Has_Element (Position) then
            declare
               Item : Neighbour_Record renames
                        Knowledge.Neighbour_Map (Position);
            begin
               if Distance < Item.Distance then
                  Item.Distance := Distance;
                  Item.Nearest  := Colony.Reference;
               end if;
            end;
         else
            Knowledge.Neighbour_Map.Insert
              (Neighbour.Identifier,
               Neighbour_Record'
                 (Neighbour => Neighbour.Reference,
                  Nearest   => Colony.Reference,
                  Distance  => Distance));
         end if;
         return True;
      end Update;

   begin

      Athena.Handles.Star.Iterate_Nearest_Stars
        (To_Star      => Colony.Star,
         Max_Distance => 20.0,
         Process      => Update'Access);

   end Update_Neighbours;

   ---------------------------
   -- Update_Star_Knowledge --
   ---------------------------

   procedure Update_Star_Knowledge
     (Knowledge : Knowledge_Handle;
      Star      : Star_Reference;
      Rec       : Star_Knowledge_Record)
   is
      Map : Star_Knowledge_Maps.Map renames
              Vector (Knowledge.Reference).Stars;
      Position : constant Star_Knowledge_Maps.Cursor :=
                   Map.Find (Star);
   begin
      if Star_Knowledge_Maps.Has_Element (Position) then
         Map (Position) := Rec;
      else
         Map.Insert (Star, Rec);
      end if;
   end Update_Star_Knowledge;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Knowledge  : Knowledge_Handle;
      Star       : Athena.Handles.Star.Star_Handle)
   is
      Rec : Star_Knowledge_Record :=
              Get_Star_Knowledge (Knowledge, Star.Reference);

      procedure Add_Orbiting_Ship
        (Reference : Ship_Reference);

      -----------------------
      -- Add_Orbiting_Ship --
      -----------------------

      procedure Add_Orbiting_Ship
        (Reference : Ship_Reference)
      is
         Ship : constant Athena.Handles.Ship.Ship_Handle :=
                  Athena.Handles.Ship.Get (Reference);
      begin
         if Ship.Owner.Reference /= Vector (Knowledge.Reference).Empire then
            Rec.Ships.Append
              (Known_Ship_Record'
                 (Owner        => Ship.Owner.Reference,
                  Hull_Tonnage => Athena.Ships.Tonnage (Ship),
                  Weapons      => 0.0));
         end if;
      end Add_Orbiting_Ship;

   begin
      if not Rec.Visited then
         Rec.Visited := True;
         Athena.Handles.Empire.Get (Vector (Knowledge.Reference).Empire)
           .Send_Signal (Colonization_Manager);
      end if;

      Rec.Last_Visit := Current_Turn;
      Rec.Ships.Clear;
      Star.Iterate_Orbiting_Ships (Add_Orbiting_Ship'Access);
      Update_Star_Knowledge (Knowledge, Star.Reference, Rec);
      Star.Log
        ("visited by "
         & Athena.Handles.Empire.Get
           (Vector (Knowledge.Reference).Empire).Short_Name);
   end Visit;

end Athena.Handles.Knowledge;
