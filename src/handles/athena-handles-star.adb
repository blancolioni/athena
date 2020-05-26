with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Athena.Elementary_Functions;

package body Athena.Handles.Star is

   Stored_Nearest_Count : constant := 24;

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
         Owner          : Empire_Reference;
         Occupier       : Empire_Reference;
         Core_Distance  : Non_Negative_Real;
         Space          : Positive;
         Resource       : Unit_Real;
         Habitability   : Unit_Real;
         Neighbours     : Neighbour_Lists.List;
         Colony         : Colony_Reference;
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

   function Space
     (Star : Star_Handle)
      return Natural
   is (Vector (Star.Reference).Space);

   function Resource
     (Star : Star_Handle)
      return Unit_Real
   is (Vector (Star.Reference).Resource);

   function Habitability
     (Star : Star_Handle)
      return Unit_Real
   is (Vector (Star.Reference).Habitability);

   function Has_Colony
     (Star : Star_Handle)
      return Boolean
   is (Vector (Star.Reference).Colony /= Null_Colony_Reference);

   function Colony
     (Star : Star_Handle)
      return Colony_Reference
   is (Vector (Star.Reference).Colony);

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
      Core_Distance : Non_Negative_Real;
      Space         : Positive;
      Resource      : Unit_Real;
      Habitability  : Unit_Real)
      return Star_Handle
   is
   begin
      if Map.Contains (Name) then
         raise Constraint_Error with
           "multiple stars called '" & Name & "'";
      end if;

      Vector.Append
        (Star_Record'
           (Identifier     => Next_Identifier,
            Star_Name      => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            X              => X,
            Y              => Y,
            Owner          => 0,
            Occupier       => 0,
            Core_Distance  => Core_Distance,
            Space          => Space,
            Resource       => Resource,
            Habitability   => Habitability,
            Neighbours     => <>,
            Colony         => Null_Colony_Reference,
            Orbiting_Ships => <>));
      Map.Insert (Name, Vector.Last_Index);
      return Star_Handle'
        (Has_Element => True,
         Reference   => Vector.Last_Index);

   end Create;

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

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Star_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ----------------
   -- Set_Colony --
   ----------------

   procedure Set_Colony
     (Star   : Star_Handle;
      Colony : Colony_Reference)
   is
   begin
      Vector (Star.Reference).Colony := Colony;
   end Set_Colony;

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
