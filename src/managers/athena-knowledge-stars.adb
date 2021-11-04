with Athena.Logging;
with Athena.Turns;

with Athena.Ships;
with Athena.Treaties;

with Minerva.Ship;
with Minerva.Ship_Knowledge;
with Minerva.Star_Knowledge;

with Minerva.Star_Distance;

package body Athena.Knowledge.Stars is

   function Closer (Left, Right : Neighbour_Record) return Boolean
   is (Left.Distance < Right.Distance);

   package Neighbour_Sorting is
     new Neighbour_Lists.Generic_Sorting (Closer);

   type Cached_Knowledge is
      record
         Turn      : Positive;
         Knowledge : Star_Knowledge;
      end record;

   package Cached_Knowledge_Maps is
     new WL.String_Maps (Cached_Knowledge);

   Cached_Knowledge_Map : Cached_Knowledge_Maps.Map;

   procedure Update_Neighbours
     (Knowledge : in out Star_Knowledge'Class;
      Colony    : Minerva.Colony.Colony_Class);

   function Get_Star_Knowledge
     (For_Empire : Minerva.Empire.Empire_Class;
      For_Star   : Minerva.Star.Star_Class)
      return Minerva.Star_Knowledge.Star_Knowledge_Class;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache is
   begin
      Cached_Knowledge_Map.Clear;
   end Clear_Cache;

   ----------------------
   -- Clear_Colonizing --
   ----------------------

   procedure Clear_Colonizing
     (Empire     : Minerva.Empire.Empire_Class;
      Star       : Minerva.Star.Star_Class)
   is
      K : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
            Get_Star_Knowledge (Empire, Star);
   begin
      K.Update_Star_Knowledge
        .Set_Colonizing (False)
        .Done;
   end Clear_Colonizing;

   ---------------------
   -- Get_Known_Ships --
   ---------------------

   function Get_Known_Ships
     (Knowledge : Star_Knowledge'Class;
      At_Star   : Minerva.Star.Star_Class)
      return Known_Ship_Lists.List
   is
      K : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
            Get_Star_Knowledge (Knowledge.Empire, At_Star);
   begin
      return List : Known_Ship_Lists.List do
         for Known_Ship of
           Minerva.Ship_Knowledge.Select_By_Ship_Knowledge
             (K, K.Last_Visit)
         loop
            List.Append
              (Known_Ship_Record'
                 (Mass        => Known_Ship.Mass,
                  Weapon_Mass => Known_Ship.Weapon_Mass));
         end loop;
      end return;
   end Get_Known_Ships;

   -----------------------------
   -- Get_Knowledge_Reference --
   -----------------------------

   function Get_Star_Knowledge
     (For_Empire : Minerva.Empire.Empire_Class;
      For_Star   : Minerva.Star.Star_Class)
      return Minerva.Star_Knowledge.Star_Knowledge_Class
   is
      K : constant Minerva.Star_Knowledge.Star_Knowledge_Handle :=
            Minerva.Star_Knowledge.Get_By_Star_Knowledge
              (For_Star, For_Empire);
   begin
      if not K.Has_Element then
         return Minerva.Star_Knowledge.Create
           (Star       => For_Star,
            Empire     => For_Empire,
            Owner      => Minerva.Empire.Empty_Handle,
            Last_Visit => Minerva.Turn.Empty_Handle,
            Last_Pop   => 0.0,
            Last_Ind   => 0.0,
            Visited    => False,
            Colonizing => False);
      else
         return K;
      end if;
   end Get_Star_Knowledge;

   ------------------------
   -- Iterate_Neighbours --
   ------------------------

   procedure Iterate_Neighbours
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access procedure
        (Neighbour : Minerva.Star.Star_Class;
         Nearest   : Minerva.Colony.Colony_Class; Stop : out Boolean))
   is
   begin
      for Rec of Knowledge.Neighbour_List loop
         exit when Rec.Distance > Max_Range;

         declare
            Stop : Boolean;
         begin
            Process (Rec.Neighbour, Rec.Nearest, Stop);
            exit when Stop;
         end;
      end loop;
   end Iterate_Neighbours;

   ---------------------
   -- Iterate_Threats --
   ---------------------

   procedure Iterate_Threats
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Threat      : Minerva.Empire.Empire_Class;
                   Threat_Star : Minerva.Star.Star_Class;
                   Nearest     : Minerva.Colony.Colony_Class;
                   Stop        : out Boolean))
   is
   begin
      for Element of Knowledge.Threat_List loop
         exit when Element.Distance > Max_Range;

         declare
            Stop : Boolean;
         begin
            Process (Element.Neighbour.Owner, Element.Neighbour,
                     Element.Nearest, Stop);
            exit when Stop;
         end;
      end loop;
   end Iterate_Threats;

   -------------------------
   -- Iterate_Uncolonized --
   -------------------------

   procedure Iterate_Uncolonized
     (Knowledge : Star_Knowledge'Class;
      Process   : not null access
        procedure (Star      : Minerva.Star.Star_Class;
                   Stop      : out Boolean))
   is
   begin
      for Item of Knowledge.Uncolonized loop
         declare
            Stop : Boolean;
         begin
            if not Knowledge.Colonizing.Contains (Item.Identifier) then
               Process (Item, Stop);
               exit when Stop;
            end if;
         end;
      end loop;
   end Iterate_Uncolonized;

   ----------------
   -- Last_Visit --
   ----------------

   function Last_Visit
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Minerva.Turn.Turn_Class
   is
      K : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
            Get_Star_Knowledge (Knowledge.Empire, Star);
   begin
      return K.Last_Visit;
   end Last_Visit;

   ----------
   -- Load --
   ----------

   procedure Load
     (Knowledge  : in out Star_Knowledge;
      For_Empire : Minerva.Empire.Empire_Class)
   is
      Turn : constant Positive := Athena.Turns.Current_Turn;

      function Greater_Threat (Left, Right : Neighbour_Record) return Boolean;

      --------------------
      -- Greater_Threat --
      --------------------

      function Greater_Threat
        (Left, Right : Neighbour_Record)
         return Boolean
      is
         Left_War : constant Boolean :=
                      Left.Neighbour.Owner.Has_Element
                          and then Athena.Treaties.At_War
                            (For_Empire, Left.Neighbour.Owner);
         Right_War : constant Boolean :=
                       Right.Neighbour.Owner.Has_Element
                           and then Athena.Treaties.At_War
                             (For_Empire, Right.Neighbour.Owner);
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

      declare
         use Cached_Knowledge_Maps;
         Position : Cursor :=
                      Cached_Knowledge_Map.Find (For_Empire.Identifier);
      begin
         if Has_Element (Position) then
            if Element (Position).Turn = Turn then
               Knowledge := Element (Position).Knowledge;
               return;
            else
               Cached_Knowledge_Map.Delete (Position);
            end if;
         end if;
      end;

      Knowledge.Empire :=
        Minerva.Empire.Get (For_Empire.Reference_Empire);

      Athena.Logging.Log
        (For_Empire.Name
         & "/knowledge: scanning colonies");

      for Colony of Minerva.Colony.Select_By_Empire (For_Empire) loop
         Update_Neighbours (Knowledge, Colony);
      end loop;

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: sorting neighbours");

      for Neighbour of Knowledge.Neighbour_Map loop
         Knowledge.Neighbour_List.Append (Neighbour);
      end loop;

      Neighbour_Sorting.Sort (Knowledge.Neighbour_List);

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: finding threats");

      for Element of Knowledge.Neighbour_List loop
         if Element.Neighbour.Owner.Has_Element then
            declare
               Id : constant String := Element.Neighbour.Owner.Identifier;
            begin
               if not Knowledge.Threat_Map.Contains (Id) then
                  Knowledge.Threat_Map.Insert
                    (Id, Element);
                  Knowledge.Threat_List.Append (Element);
               end if;
            end;
         end if;
      end loop;

      Threat_Sorting.Sort (Knowledge.Threat_List);

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: scanning star knowledge");

      for K of Minerva.Star_Knowledge.Select_By_Empire
        (For_Empire)
      loop
         if K.Visited then
            declare
               Star : constant Minerva.Star.Star_Class := K.Star;
            begin
               Knowledge.Visited.Insert (Star.Identifier, Star);
               if not Star.Owner.Has_Element then
                  Knowledge.Uncolonized.Insert (Star.Identifier, Star);
               end if;
               if K.Colonizing then
                  Knowledge.Colonizing.Insert (Star.Identifier, Star);
               end if;
            end;
         end if;
      end loop;

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: adding to cache");

      Cached_Knowledge_Map.Insert
        (For_Empire.Identifier, (Turn, Knowledge));

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: done");

   end Load;

   --------------------
   -- Set_Colonizing --
   --------------------

   procedure Set_Colonizing
     (Knowledge  : in out Star_Knowledge'Class;
      Star       : Minerva.Star.Star_Class;
      Colonizing : Boolean)
   is
      K : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
            Get_Star_Knowledge
              (Knowledge.Empire, Star);
   begin
      Minerva.Star_Knowledge.Update_Star_Knowledge (K)
        .Set_Colonizing (Colonizing)
        .Done;

      if Colonizing then
         Knowledge.Colonizing.Insert (Star.Identifier, Star);
      else
         Knowledge.Colonizing.Delete (Star.Identifier);
      end if;

      Cached_Knowledge_Map.Replace
        (Knowledge.Empire.Identifier,
         (Athena.Turns.Current_Turn, Star_Knowledge (Knowledge)));

   end Set_Colonizing;

   ----------------------------
   -- Turns_Since_Last_Visit --
   ----------------------------

   function Turns_Since_Last_Visit
     (Knowledge : Star_Knowledge'Class;
      Star      : Minerva.Star.Star_Class)
      return Natural
   is
      Last_Visit_Turn : constant Minerva.Turn.Turn_Class :=
                          Knowledge.Last_Visit (Star);
   begin
      if Last_Visit_Turn.Has_Element then
         return Athena.Turns.Current_Turn.Turn_Number
           - Last_Visit_Turn.Turn_Number;
      else
         return Natural'Last;
      end if;
   end Turns_Since_Last_Visit;

   -----------------------
   -- Update_Neighbours --
   -----------------------

   procedure Update_Neighbours
     (Knowledge : in out Star_Knowledge'Class;
      Colony    : Minerva.Colony.Colony_Class)
   is
      Handle : constant Minerva.Colony.Colony_Handle :=
                 Minerva.Colony.Get (Colony.Reference_Colony);

      procedure Update
        (Neighbour : Minerva.Star.Star_Class;
         Distance  : Non_Negative_Real);

      ------------
      -- Update --
      ------------

      procedure Update
        (Neighbour : Minerva.Star.Star_Class;
         Distance  : Non_Negative_Real)
      is
         use Neighbour_Maps;
         Position : constant Neighbour_Maps.Cursor :=
                      Knowledge.Neighbour_Map.Find (Neighbour.Identifier);
      begin
         if Neighbour.Owner.Has_Element
           and then Neighbour.Owner.Identifier = Knowledge.Empire.Identifier
         then
            null;
         elsif Has_Element (Position) then
            declare
               Item : Neighbour_Record renames
                        Knowledge.Neighbour_Map (Position);
            begin
               if Distance < Item.Distance then
                  Item.Distance := Distance;
                  Item.Nearest  := Handle;
               end if;
            end;
         else
            declare
               Star : constant Minerva.Star.Star_Handle :=
                        Minerva.Star.Get (Neighbour.Reference_Star);
            begin
               Knowledge.Neighbour_Map.Insert
                 (Neighbour.Identifier,
                  Neighbour_Record'
                    (Neighbour => Star,
                     Nearest   => Handle,
                     Distance  => Distance));
            end;
         end if;
      end Update;

   begin

      for Star_Distance of
        Minerva.Star_Distance.Select_Closest_Stars_Bounded_By_Distance
          (Colony.Star, 0.0, 20.0)
      loop
         Update (Star_Distance.To, Star_Distance.Distance);
      end loop;

   end Update_Neighbours;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Empire : Minerva.Empire.Empire_Class;
      Star   : Minerva.Star.Star_Class)
   is
      K      : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
                 Get_Star_Knowledge (Empire, Star);
      Colony : constant Minerva.Colony.Colony_Class :=
                 Minerva.Colony.Get_By_Star (Star);
      Owner  : constant Minerva.Empire.Empire_Class := Star.Owner;
      Pop    : constant Non_Negative_Real :=
                 (if Colony.Has_Element then Colony.Population else 0.0);
      Ind    : constant Non_Negative_Real :=
                 (if Colony.Has_Element then Colony.Industry else 0.0);

      procedure Record_Ship
        (Ship : Minerva.Ship.Ship_Class);

      -----------------
      -- Record_Ship --
      -----------------

      procedure Record_Ship
        (Ship : Minerva.Ship.Ship_Class)
      is
         Current : constant Minerva.Ship_Knowledge.Ship_Knowledge_Class :=
                     Minerva.Ship_Knowledge.Get_By_Observed_Ship
                       (K, Ship.Identifier);
      begin

         if not Ship.Alive
           or else Ship.Empire.Identifier = Empire.Identifier
         then
            return;
         end if;

         if Ship.Destination.Has_Element
           and then Ship.Progress > 0.0
         then
            return;
         end if;

         if Current.Has_Element then
            Current.Update_Ship_Knowledge
              .Set_Star_Knowledge (K)
              .Set_Turn (Athena.Turns.Current_Turn)
              .Done;
         else
            Minerva.Ship_Knowledge.Create
              (Star_Knowledge => K,
               Owner          => Ship.Empire,
               Turn           =>
                 Athena.Turns.Current_Turn,
               Identifier     => Ship.Identifier,
               Name           => Ship.Name,
               Mass           => Athena.Ships.Current_Mass (Ship),
               Weapon_Mass    => Athena.Ships.Weapon_Mass (Ship));
         end if;
      end Record_Ship;

   begin
      Minerva.Star_Knowledge.Update_Star_Knowledge (K)
        .Set_Visited (True)
        .Set_Last_Visit (Athena.Turns.Current_Turn)
        .Set_Owner (Owner)
        .Set_Last_Pop (Pop)
        .Set_Last_Ind (Ind)
        .Done;

      for Ship of
        Minerva.Ship.Select_By_Star (Star)
      loop
         Record_Ship (Ship);
      end loop;

      declare
         Knowledge : Star_Knowledge;

      begin
         Knowledge.Load (Empire);

         if not Knowledge.Visited.Contains (Star.Identifier) then
            Knowledge.Visited.Insert (Star.Identifier, Star);

            Cached_Knowledge_Map.Replace
              (Knowledge.Empire.Identifier,
               (Athena.Turns.Current_Turn, Knowledge));
         end if;
      end;

   end Visit;

end Athena.Knowledge.Stars;
