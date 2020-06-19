with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Athena.Elementary_Functions;

with Athena.Solar_System;

package body Athena.Handles.Star is

   Stored_Nearest_Count : constant := 24;

   type Deposit_Record is
      record
         Resource : Commodity_Reference;
         Quality  : Non_Negative_Real;
      end record;

   package Deposit_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Deposit_Record);

   type Neighbour_Record is
      record
         Reference : Star_Reference;
         Distance  : Non_Negative_Real;
      end record;

   package Neighbour_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Neighbour_Record);

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type Star_Record is
      record
         Identifier     : Object_Identifier;
         Star_Name      : Ada.Strings.Unbounded.Unbounded_String;
         X, Y           : Real;
         Mass           : Non_Negative_Real;
         Owner          : Empire_Reference;
         Occupier       : Empire_Reference;
         Core_Distance  : Non_Negative_Real;
         Neighbours     : Neighbour_Lists.List;
         Orbiting_Ships : Ship_Lists.List;
      end record;

   package Star_Vectors is
     new Ada.Containers.Vectors
       (Real_Star_Reference, Star_Record);

   package Star_Maps is
     new WL.String_Maps (Real_Star_Reference);

   Vector : Star_Vectors.Vector;
   Map    : Star_Maps.Map;

   overriding function Identifier
     (Star : Star_Handle)
      return Object_Identifier
   is (Vector (Star.Reference).Identifier);

   overriding function Mass
     (Star : Star_Handle)
      return Non_Negative_Real
   is (Vector (Star.Reference).Mass);

   function Name
     (Star : Star_Handle)
      return String
   is (-(Vector (Star.Reference).Star_Name));

   function X
     (Star : Star_Handle)
      return Real
   is (Vector (Star.Reference).X);

   function Y
     (Star : Star_Handle)
      return Real
   is (Vector (Star.Reference).Y);

   function Has_Owner
     (Star : Star_Handle)
      return Boolean
   is (Vector (Star.Reference).Owner /= 0);

   function Owner
     (Star : Star_Handle)
      return Empire_Reference
   is (Vector (Star.Reference).Owner);

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (Star : Star_Handle;
      Ship : Ship_Reference)
   is
   begin
      Vector (Star.Reference).Orbiting_Ships.Append (Ship);
   end Add_Ship;

   -------------------------
   -- Calculate_Distances --
   -------------------------

   procedure Calculate_Distances is
   begin
      for Index in 1 .. Vector.Last_Index loop
         declare
            Rec : constant Star_Record := Vector.Element (Index);
            List : Neighbour_Lists.List;
         begin
            for Other_Index in 1 .. Vector.Last_Index loop
               if Other_Index /= Index then
                  declare
                     Other_Rec : constant Star_Record :=
                                   Vector.Element (Other_Index);
                     Distance  : constant Non_Negative_Real :=
                                   (Rec.X - Other_Rec.X) ** 2
                                   + (Rec.Y - Other_Rec.Y) ** 2;
                     Length    : constant Natural :=
                                   Natural (List.Length);
                  begin
                     if Length < Stored_Nearest_Count
                       or else Distance < List.Last_Element.Distance
                     then
                        if Length >= Stored_Nearest_Count then
                           List.Delete_Last;
                        end if;

                        declare
                           use Neighbour_Lists;
                           Position : Cursor := List.First;
                        begin
                           while Has_Element (Position)
                             and then Distance >= Element (Position).Distance
                           loop
                              Next (Position);
                           end loop;
                           List.Insert
                             (Position, (Other_Index, Distance));
                        end;
                     end if;
                  end;
               end if;
            end loop;

            for Item of List loop
               Item.Distance :=
                 Athena.Elementary_Functions.Sqrt (Item.Distance);
            end loop;
            Vector (Index).Neighbours := List;
         end;
      end loop;
   end Calculate_Distances;

   ------------
   -- Create --
   ------------

   function Create
     (X, Y          : Real;
      Name          : String;
      Solar_Masses  : Non_Negative_Real;
      Core_Distance : Non_Negative_Real)
      return Star_Handle
   is
      --  Deposits : Deposit_Lists.List;
   begin
      if Map.Contains (Name) then
         raise Constraint_Error with
           "multiple stars called '" & Name & "'";
      end if;

      --  declare
      --     use Athena.Handles.Commodity;
      --  begin
      --     for Commodity of All_Commodities loop
      --        if Commodity.Class = Athena.Handles.Commodity.Resource
      --          and then not Commodity.Is_Abstract
      --        then
      --           declare
      --              Quality : constant Non_Negative_Real :=
      --                Resource * Execute (Commodity.Deposit_Constraint);
      --           begin
      --              Athena.Logging.Log
      --                ("star " & Name
      --                 & ": resource " & Image (Resource)
      --                 & "; deposit "
      --                 & Commodity.Tag
      --                 & "; quality "
      --                 & Image (Quality));
      --              Deposits.Append
      --                (Deposit_Record'
      --                   (Resource => Commodity.Reference,
      --                    Quality  => Quality));
      --           end;
      --        end if;
      --     end loop;
      --  end;

      Vector.Append
        (Star_Record'
           (Identifier     => Next_Identifier,
            Star_Name      => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            X              => X,
            Y              => Y,
            Mass           => Athena.Solar_System.Solar_Mass * Solar_Masses,
            Owner          => 0,
            Occupier       => 0,
            Core_Distance  => Core_Distance,
            Neighbours     => <>,
            Orbiting_Ships => <>));
      Map.Insert (Name, Vector.Last_Index);
      return Star_Handle'
        (Has_Element => True,
         Reference   => Vector.Last_Index);

   end Create;

   ----------------------
   -- Extract_Resource --
   ----------------------

   --  function Extract_Resource
   --    (Handle   : Star_Handle;
   --     Resource : Athena.Handles.Commodity.Commodity_Handle;
   --     Size     : Non_Negative_Real)
   --     return Non_Negative_Real
   --  is
   --  begin
   --     for Item of Vector (Handle.Reference).Deposits loop
   --        if Item.Resource = Resource.Reference then
   --           declare
   --              Quality : constant Non_Negative_Real := Item.Quality;
   --              Change  : constant Real :=
   --                (if Size <= 1.0
   --                 then 0.0
   --                 else Quality
   --                   * Athena.Elementary_Functions.Log (Size) / 100000.0);
   --           begin
   --              return Extracted : constant Non_Negative_Real :=
   --                Size * Change * 10000.0
   --              do
   --                 Item.Quality :=
   --                   (if Change < Quality
   --                    then Quality - Change
   --                    else Quality / 2.0);
   --                 Handle.Log
   --                   (Resource.Tag
   --                    & ": quality " & Image (Quality)
   --                    & "; size " & Image (Size)
   --                    & "; change " & Image (Change)
   --                    & "; new quality " & Image (Item.Quality)
   --                    & "; extracted " & Image (Extracted));
   --              end return;
   --           end;
   --        end if;
   --     end loop;
   --     return 0.0;
   --  end Extract_Resource;

   ---------------
   -- Find_Star --
   ---------------

   function Find_Star
     (Test : not null access function (Handle : Star_Handle) return Boolean)
      return Star_Handle
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         if Test (Get (Reference)) then
            return Get (Reference);
         end if;
      end loop;
      return (False, 0);
   end Find_Star;

   -----------------
   -- Get_By_Name --
   -----------------

   function Get_By_Name (Name : String) return Star_Handle is
      use Star_Maps;
      Position : constant Cursor := Map.Find (Name);
   begin
      return Handle : Star_Handle do
         if Has_Element (Position) then
            Handle.Reference := Element (Position);
            Handle.Has_Element := True;
         end if;
      end return;
   end Get_By_Name;

   ---------------------------
   -- Iterate_Nearest_Stars --
   ---------------------------

   procedure Iterate_Nearest_Stars
     (To_Star      : Star_Handle;
      Max_Distance : Non_Negative_Real;
      Process      : not null access
        function (Handle : Star_Handle) return Boolean)
   is
   begin
      for Neighbour of Vector (To_Star.Reference).Neighbours loop
         exit when Neighbour.Distance > Max_Distance
           or else not Process (Get (Neighbour.Reference));
      end loop;
   end Iterate_Nearest_Stars;

   ----------------------------
   -- Iterate_Orbiting_Ships --
   ----------------------------

   procedure Iterate_Orbiting_Ships
     (Star         : Star_Handle;
      Process      : not null access
        procedure (Reference : Ship_Reference))
   is
   begin
      for Reference of Vector (Star.Reference).Orbiting_Ships loop
         Process (Reference);
      end loop;
   end Iterate_Orbiting_Ships;

   -------------------
   -- Iterate_Stars --
   -------------------

   procedure Iterate_Stars
     (Process      : not null access
        procedure (Handle : Star_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end Iterate_Stars;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Star_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Star_Name), I);
      end loop;
   end Load;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Star : Star_Handle;
      Ship : Ship_Reference)
   is
      use Ship_Lists;
      Rec : Star_Record renames Vector (Star.Reference);
      Position : Cursor := Rec.Orbiting_Ships.Find (Ship);
   begin
      pragma Assert (Has_Element (Position),
                     "cannot remove ship" & Ship'Image
                     & " from " & Star.Name
                     & " because it is not there");
      Rec.Orbiting_Ships.Delete (Position);
   end Remove_Ship;

   ----------------------
   -- Resource_Quality --
   ----------------------

   --  function Resource_Quality
   --    (Handle   : Star_Handle;
   --     Resource : Athena.Handles.Commodity.Commodity_Handle)
   --     return Non_Negative_Real
   --  is
   --  begin
   --     for Item of Vector (Handle.Reference).Deposits loop
   --        if Item.Resource = Resource.Reference then
   --           return Item.Quality;
   --        end if;
   --     end loop;
   --     return 0.0;
   --  end Resource_Quality;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Star_Vectors.Vector'Write (Stream, Vector);
   end Save;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Star     : Star_Handle;
      New_Name : String)
   is
   begin
      Vector (Star.Reference).Star_Name := +New_Name;
   end Set_Name;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Star      : Star_Handle;
      New_Owner : Empire_Reference)
   is
   begin
      Vector (Star.Reference).Owner := New_Owner;
   end Set_Owner;

end Athena.Handles.Star;
