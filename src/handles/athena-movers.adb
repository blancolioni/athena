with Athena.Real_Images;

with Athena.Solar_System;

package body Athena.Movers is

   use Athena.Real_Arrays;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   function Image (Location : Mover_Location) return String
   is (case Location.Loc_Type is
          when Nowhere => "nowhere",
          when Deep_Space =>
             "deep space (" & Image (Location.Position (1))
       & "," & Image (Location.Position (2)) & ")",
          when System_Space =>
             Image (abs (Location.System_Position)
                      / Athena.Solar_System.Earth_Orbit)
       & " AUs from " & Location.Star.Spectral_Class
       & " " & Location.Star.Name,
          when World_Orbit  =>
             "orbiting " & Location.World.Name);

   function System_Position
     (Location : Mover_Location)
      return Athena.Real_Arrays.Real_Vector
     with Pre => Location.Loc_Type in System_Space | World_Orbit;

   function Deep_Space_Position
     (Location : Mover_Location)
      return Athena.Real_Arrays.Real_Vector
   is (case Real_Location (Location.Loc_Type) is
          when Deep_Space => Location.Position,
          when System_Space => (Location.Star.X, Location.Star.Y, 0.0),
          when World_Orbit  =>
            (Location.World.Star.X, Location.World.Star.Y, 0.0))
     with Pre => Location.Loc_Type /= Nowhere;

   function Is_Closer (Left, Right : Mover_Location;
                       To          : Mover_Location)
                       return Boolean;

   -------------
   -- At_Star --
   -------------

   function At_Star
     (Mover : Mover_Interface'Class;
      Star  : Athena.Handles.Star.Star_Handle)
      return Boolean
   is
      use type Athena.Handles.Star.Star_Handle;
   begin
      return (Mover.In_System_Space or else Mover.Orbiting_World)
        and then Mover.Location_Star = Star;
   end At_Star;

   --------------
   -- At_World --
   --------------

   function At_World
     (Mover : Mover_Interface'Class;
      World : Athena.Handles.World.World_Handle)
      return Boolean
   is
      use type Athena.Handles.World.World_Handle;
   begin
      return Mover.Orbiting_World
        and then Mover.Location_World = World;
   end At_World;

   ----------------------------
   -- Current_Journey_Length --
   ----------------------------

   function Current_Journey_Length
     (Mover : Mover_Interface'Class)
      return Non_Negative_Real
   is
   begin
      if not Mover.Has_Destination then
         return 0.0;
      end if;

      case Mover.Location.Loc_Type is
         when Nowhere =>
            return 0.0;
         when Deep_Space =>
            declare
               A : constant Real_Vector :=
                     Deep_Space_Position (Mover.Location);
               B : constant Real_Vector :=
                     Deep_Space_Position (Mover.Destination);
            begin
               return abs (B - A);
            end;
         when System_Space | World_Orbit =>
            declare
               A : constant Real_Vector := System_Position (Mover.Location);
               B : constant Real_Vector := System_Position (Mover.Destination);
            begin
               return abs (B - A);
            end;
      end case;
   end Current_Journey_Length;

   ----------------------
   -- Current_Location --
   ----------------------

   function Current_Location
     (Mover : Mover_Interface'Class)
      return Mover_Location
   is
   begin
      if not Mover.Has_Destination then
         return Mover.Location;
      end if;

      case Mover.Location.Loc_Type is
         when Nowhere =>
            return Mover.Location;
         when Deep_Space =>
            declare
               A : constant Real_Vector :=
                     Deep_Space_Position (Mover.Location);
               B : constant Real_Vector :=
                     Deep_Space_Position (Mover.Destination);
            begin
               return (Deep_Space, A + (B - A) * Mover.Progress);
            end;
         when System_Space | World_Orbit =>
            declare
               A : constant Real_Vector := System_Position (Mover.Location);
               B : constant Real_Vector := System_Position (Mover.Destination);
            begin
               return (System_Space, Mover.Location_Star,
                       A + (B - A) * Mover.Progress);
            end;
      end case;
   end Current_Location;

   ---------------------------
   -- Current_Location_Name --
   ---------------------------

   function Current_Location_Name
     (Mover : Mover_Interface'Class)
      return String
   is
   begin
      return Image (Mover.Current_Location);
   end Current_Location_Name;

   ----------------------
   -- Destination_Name --
   ----------------------

   function Destination_Name (Mover : Mover_Interface'Class) return String is
   begin
      return Image (Mover.Destination);
   end Destination_Name;

   --------------
   -- Get_Star --
   --------------

   function Get_Star
     (Location : Mover_Location)
      return Athena.Handles.Star.Star_Handle
   is
   begin
      case Location.Loc_Type is
         when Nowhere =>
            return Athena.Handles.Star.Empty_Handle;
         when Deep_Space =>
            return Athena.Handles.Star.Empty_Handle;
         when System_Space =>
            return Location.Star;
         when World_Orbit =>
            return Location.World.Star;
      end case;
   end Get_Star;

   ---------------
   -- Is_Closer --
   ---------------

   function Is_Closer
     (Mover : Mover_Interface'Class;
      Than  : Mover_Interface'Class;
      World : Athena.Handles.World.World_Handle)
      return Boolean
   is
   begin
      return Is_Closer
        (Left  => Mover.Location,
         Right => Than.Location,
         To    => (System_Space, World.Star, World.Current_Global_Position));
   --
   --     use type Athena.Handles.Star.Star_Handle;
   --     use type Athena.Handles.World.World_Handle;
   --  begin
   --     if Mover.Location = Than.Location then
   --        case Mover.Location is
   --           when Nowhere =>
   --              return False;
   --           when Deep_Space =>
   --              return Mover.Is_Closer (Than, World.Star);
   --           when System_Space =>
   --              if Mover.Location_Star = World.Star then
   --                 if Than.Location_Star /= World.Star then
   --                    return True;
   --                 else
   --                    declare
   --                       use Athena.Real_Arrays;
   --                       Move_Pos  : constant Real_Vector :=
   --                                     Mover.System_Position;
   --                       Than_Pos  : constant Real_Vector :=
   --                                     Than.System_Position;
   --                       World_Pos : constant Real_Vector :=
   --                                     World.Current_Global_Position;
   --                    begin
   --                       return abs (Move_Pos - World_Pos)
   --                         < abs (Than_Pos - World_Pos);
   --                    end;
   --                 end if;
   --              elsif Than.Location_Star = World.Star then
   --                 return False;
   --              else
   --                 return Mover.Is_Closer (Than, World.Star);
   --              end if;
   --           when World_Orbit =>
   --              if Mover.Location_World = World then
   --                 return Than.Location_World /= World;
   --              elsif Than.Location_World = World then
   --                 return False;
   --              elsif Mover.Location_Star = World.Star then
   --                 if Than.Location_Star /= World.Star then
   --                    return True;
   --                 else
   --                    declare
   --                       use Athena.Real_Arrays;
   --                       Move_Pos  : constant Real_Vector :=
   --                                     Mover.System_Position;
   --                       Than_Pos  : constant Real_Vector :=
   --                                     Than.System_Position;
   --                       World_Pos : constant Real_Vector :=
   --                                     World.Current_Global_Position;
   --                    begin
   --                       return abs (Move_Pos - World_Pos)
   --                         < abs (Than_Pos - World_Pos);
   --                    end;
   --                 end if;
   --              elsif Than.Location_Star = World.Star then
   --                 return False;
   --              elsif Mover.Location_Star = Than.Location_Star then
   --                 return False;
   --              else
   --                 declare
   --                    use Athena.Real_Arrays;
   --                    Mover_Pos : constant Real_Vector :=
   --                                  (Mover.Location_Star.X,
   --                                   Mover.Location_Star.Y);
   --                    Than_Pos  : constant Real_Vector :=
   --                                  (Than.Location_Star.X,
   --                                   Than.Location_Star.Y);
   --                    World_Pos : constant Real_Vector :=
   --                                  (World.Star.X, World.Star.Y);
   --                 begin
   --                    return abs (Mover_Pos - World_Pos)
   --                      < abs (Than_Pos - World_Pos);
   --                 end;
   --              end if;
   --        end case;
   --     else
   --        case Mover.Location is
   --           when Nowhere =>
   --              return False;
   --           when Deep_Space =>
   --              return not Than.At_Star (World.Star);
   --           when System_Space =>
   --              return Mover.At_Star (World.Star)
   --                and then not Than.At_World (World);
   --           when World_Orbit =>
   --              return Mover.At_World (World)
   --                or else (Mover.At_Star (World.Star)
   --                         and then not Than.At_Star (World.Star));
   --        end case;
   --     end if;
   end Is_Closer;

   ---------------
   -- Is_Closer --
   ---------------

   function Is_Closer
     (Mover : Mover_Interface'Class;
      Than  : Mover_Interface'Class;
      Star  : Athena.Handles.Star.Star_Handle)
      return Boolean
   is
   begin
      return Is_Closer
        (Left  => Mover.Location,
         Right => Than.Location,
         To    => (Deep_Space, (Star.X, Star.Y, 0.0)));

      --  if Mover.Location = Than.Location then
      --     case Mover.Location is
      --        when Nowhere =>
      --           return False;
      --        when Deep_Space =>
      --           declare
      --              use Athena.Real_Arrays;
      --              Mover_Pos : constant Real_Vector :=
      --                            Mover.Deep_Space_Position;
      --              Than_Pos  : constant Real_Vector :=
      --                            Than.Deep_Space_Position;
      --              Star_Pos  : constant Real_Vector :=
      --                            (Star.X, Star.Y);
      --           begin
      --              return abs (Mover_Pos - Star_Pos)
      --                < abs (Than_Pos - Star_Pos);
      --           end;
      --        when System_Space =>
      --           if Mover.Location_Star = Star then
      --              return Than.Location_Star /= Star;
      --           elsif Than.Location_Star = Star then
      --              return False;
      --           else
      --              declare
      --                 use Athena.Real_Arrays;
      --                 Mover_Pos : constant Real_Vector :=
      --                               (Mover.Location_Star.X,
      --                                Mover.Location_Star.Y);
      --                 Than_Pos  : constant Real_Vector :=
      --                               (Than.Location_Star.X,
      --                                Than.Location_Star.Y);
      --                 Star_Pos  : constant Real_Vector :=
      --                               (Star.X, Star.Y);
      --              begin
      --                 return abs (Mover_Pos - Star_Pos)
      --                   < abs (Than_Pos - Star_Pos);
      --              end;
      --           end if;
      --        when World_Orbit =>
      --           if Mover.Location_Star = Star then
      --              return Than.Location_Star /= Star;
      --           elsif Than.Location_Star = Star then
      --              return False;
      --           else
      --              declare
      --                 use Athena.Real_Arrays;
      --                 Move_Pos  : constant Real_Vector :=
      --                               (Mover.Location_Star.X,
      --                                Mover.Location_Star.Y);
      --                 Than_Pos  : constant Real_Vector :=
      --                               (Than.Location_Star.X,
      --                                Than.Location_Star.Y);
      --                 Star_Pos : constant Real_Vector :=
      --                               (Star.X, Star.Y);
      --              begin
      --                 return abs (Move_Pos - Star_Pos)
      --                   < abs (Than_Pos - Star_Pos);
      --              end;
      --           end if;
      --     end case;
      --  else
      --     case Mover.Location is
      --        when Nowhere =>
      --           return False;
      --        when Deep_Space =>
      --           return not Than.At_Star (Star);
      --        when System_Space =>
      --           return Mover.At_Star (Star)
      --             and then not Than.At_Star (Star);
      --        when World_Orbit =>
      --           return Mover.At_Star (Star)
      --             and then not Than.At_Star (Star);
      --     end case;
      --  end if;
   end Is_Closer;

   ---------------
   -- Is_Closer --
   ---------------

   function Is_Closer
     (Left, Right : Mover_Location;
      To          : Mover_Location)
      return Boolean
   is
      function Closer_To_Star
        (Star : Athena.Handles.Star.Star_Handle)
         return Boolean;

      function Closer_To_World
        (World : Athena.Handles.World.World_Handle)
         return Boolean;

      --------------------
      -- Closer_To_Star --
      --------------------

      function Closer_To_Star
        (Star : Athena.Handles.Star.Star_Handle)
         return Boolean
      is
         A : constant Real_Vector := Deep_Space_Position (Left);
         B : constant Real_Vector := Deep_Space_Position (Right);
         C : constant Real_Vector := (Star.X, Star.Y, 0.0);
      begin
         return abs (A - C) < abs (B - C);
      end Closer_To_Star;

      ---------------------
      -- Closer_To_World --
      ---------------------

      function Closer_To_World
        (World : Athena.Handles.World.World_Handle)
         return Boolean
      is
         use Athena.Handles.Star;
         Left_Star : constant Star_Handle := Get_Star (Left);
         Right_Star : constant Star_Handle := Get_Star (Right);
      begin
         if not Left_Star.Has_Element then
            return False;
         elsif not Right_Star.Has_Element then
            return True;
         end if;

         if Left_Star = Right_Star then
            if World.Star = Left_Star then
               declare
                  A : constant Real_Vector := System_Position (Left);
                  B : constant Real_Vector := System_Position (Right);
                  C : constant Real_Vector := World.Current_Global_Position;
               begin
                  return abs (A - C) < abs (B - C);
               end;
            else
               return False;
            end if;
         elsif Left_Star = World.Star then
            return True;
         elsif Right_Star = World.Star then
            return False;
         else
            declare
               A : constant Real_Vector := Deep_Space_Position (Left);
               B : constant Real_Vector := Deep_Space_Position (Right);
               C : constant Real_Vector := (World.Star.X, World.Star.Y, 0.0);
            begin
               return abs (A - C) < abs (B - C);
            end;
         end if;
      end Closer_To_World;

   begin
      if Left.Loc_Type = Nowhere then
         return False;
      elsif Right.Loc_Type = Nowhere then
         return True;
      else
         case To.Loc_Type is
            when Nowhere =>
               return False;
            when Deep_Space =>
               declare
                  A : constant Real_Vector := Deep_Space_Position (Left);
                  B : constant Real_Vector := Deep_Space_Position (Right);
                  C : constant Real_Vector := To.Position;
               begin
                  return abs (A - C) < abs (B - C);
               end;
            when System_Space =>
               return Closer_To_Star (To.Star);
            when World_Orbit =>
               return Closer_To_World (To.World);
         end case;
      end if;

   end Is_Closer;

   -------------------
   -- Location_Name --
   -------------------

   function Location_Name (Mover : Mover_Interface'Class) return String is
   begin
      return Image (Mover.Location);
   end Location_Name;

   -------------------
   -- Location_Star --
   -------------------

   function Location_Star
     (Mover : Mover_Interface'Class)
      return Athena.Handles.Star.Star_Handle
   is
   begin
      return Get_Star (Mover.Location);
   end Location_Star;

   --------------------
   -- Location_World --
   --------------------

   function Location_World
     (Mover : Mover_Interface'Class)
      return Athena.Handles.World.World_Handle
   is
   begin
      return Mover.Location.World;
   end Location_World;

   ---------------------
   -- System_Position --
   ---------------------

   function System_Position
     (Location : Mover_Location)
      return Athena.Real_Arrays.Real_Vector
   is
   begin
      case Location.Loc_Type is
         when Nowhere | Deep_Space =>
            raise Constraint_Error with "precodition violation";
         when System_Space =>
            return Location.System_Position;
         when World_Orbit =>
            return Location.World.Current_Global_Position;
      end case;
   end System_Position;

   -------------------
   -- Travelling_To --
   -------------------

   function Travelling_To
     (Mover : Mover_Interface'Class;
      Star  : Athena.Handles.Star.Star_Handle)
      return Boolean
   is
      use type Athena.Handles.Star.Star_Handle;
   begin
      return Mover.Has_Destination
        and then Mover.In_Deep_Space
        and then Mover.Destination_Star = Star;
   end Travelling_To;

   -------------------
   -- Travelling_To --
   -------------------

   function Travelling_To
     (Mover : Mover_Interface'Class;
      World : Athena.Handles.World.World_Handle)
      return Boolean
   is
      use type Athena.Handles.World.World_Handle;
   begin
      return Mover.Has_Destination
        and then Mover.In_System_Space
        and then Mover.Destination.Loc_Type = World_Orbit
        and then Mover.Destination_World = World;
   end Travelling_To;

end Athena.Movers;
