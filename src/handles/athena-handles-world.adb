with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Athena.Elementary_Functions;
with Athena.Logging;

with Athena.Solar_System;

package body Athena.Handles.World is

   type Deposit_Record is
      record
         Resource : Commodity_Reference;
         Quality  : Non_Negative_Real;
      end record;

   package Deposit_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Deposit_Record);

   type Atmosphere_Component is
      record
         Gas : Atmospheric_Gas;
         Partial : Unit_Real;
      end record;

   package Atmosphere_Component_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Atmosphere_Component);

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type World_Record is
      record
         Identifier          : Object_Identifier;
         World_Name          : Ada.Strings.Unbounded.Unbounded_String;
         Star                : Star_Reference;
         Radius              : Non_Negative_Real;
         Density             : Non_Negative_Real;
         Rotation_Period     : Non_Negative_Real;
         Tilt                : Athena.Trigonometry.Angle;
         Seed                : Integer;
         Semimajor_Axis      : Non_Negative_Real;
         Zero_Time_Angle     : Athena.Trigonometry.Angle;
         Eccentricity        : Non_Negative_Real;
         Inclination         : Athena.Trigonometry.Angle;
         Period              : Non_Negative_Real;
         Mass                : Non_Negative_Real;
         Composition         : World_Composition;
         Climate             : World_Climate;
         Orbit_Zone          : Stellar_Orbit_Zone;
         Gas_Giant           : Boolean;
         Age                 : Non_Negative_Real;
         Habitability        : Non_Negative_Real;
         Surface_Pressure    : Non_Negative_Real;
         Atmosphere          : Atmosphere_Component_Lists.List;
         Average_Temperature : Non_Negative_Real;
         Hydrosphere         : Non_Negative_Real;
         Life                : Life_Complexity_Type;
         Smoothness          : Natural;
         Elevation_Range     : Natural;
         Sea_Level           : Natural;
         Space               : Natural;
         Resource            : Unit_Real;
         Deposits            : Deposit_Lists.List;
         Owner               : Empire_Reference;
         Occupier            : Empire_Reference;
         Colony              : Colony_Reference;
         Orbiting_Ships      : Ship_Lists.List;
      end record;

   package World_Vectors is
     new Ada.Containers.Vectors
       (Real_World_Reference, World_Record);

   package World_Maps is
     new WL.String_Maps (Real_World_Reference);

   Vector : World_Vectors.Vector;
   Map    : World_Maps.Map;

   overriding function Identifier
     (World : World_Handle)
      return Object_Identifier
   is (Vector (World.Reference).Identifier);

   function Name
     (World : World_Handle)
      return String
   is (-(Vector (World.Reference).World_Name));

   function Star
     (Handle : World_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Handle.Reference).Star));

   overriding function Mass
     (World : World_Handle)
      return Non_Negative_Real
   is (Vector (World.Reference).Mass);

   overriding function Semimajor_Axis
     (World : World_Handle)
      return Non_Negative_Real
   is (Vector (World.Reference).Semimajor_Axis);

   overriding function Zero_Time_Angle
     (World : World_Handle)
      return Athena.Trigonometry.Angle
   is (Vector (World.Reference).Zero_Time_Angle);

   overriding function Primary
     (World : World_Handle)
      return Athena.Orbits.Massive_Interface'Class
   is (Athena.Handles.Star.Get (Vector (World.Reference).Star));

   overriding function Eccentricity
     (World : World_Handle)
      return Unit_Real
   is (Vector (World.Reference).Eccentricity);

   overriding function Inclination
     (World : World_Handle)
      return Athena.Trigonometry.Angle
   is (Vector (World.Reference).Inclination);

   function Radius
     (Handle   : World_Handle)
      return Non_Negative_Real
   is (Vector (Handle.Reference).Radius);

   function Has_Owner
     (World : World_Handle)
      return Boolean
   is (Vector (World.Reference).Owner /= 0);

   function Owner
     (World : World_Handle)
      return Empire_Reference
   is (Vector (World.Reference).Owner);

   function Space
     (World : World_Handle)
      return Natural
   is (Vector (World.Reference).Space);

   function Resource
     (World : World_Handle)
      return Unit_Real
   is (Vector (World.Reference).Resource);

   function Habitability
     (World : World_Handle)
      return Unit_Real
   is (Vector (World.Reference).Habitability);

   function Has_Colony
     (World : World_Handle)
      return Boolean
   is (Vector (World.Reference).Colony /= Null_Colony_Reference);

   function Colony
     (World : World_Handle)
      return Colony_Reference
   is (Vector (World.Reference).Colony);

   -------------
   -- Add_Gas --
   -------------

   procedure Add_Gas
     (Handle           : World_Handle;
      Gas              : Atmospheric_Gas;
      Partial_Pressure : Unit_Real)
   is
   begin
      Vector (Handle.Reference).Atmosphere.Append ((Gas, Partial_Pressure));
   end Add_Gas;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (World : World_Handle;
      Ship  : Ship_Reference)
   is
   begin
      Vector (World.Reference).Orbiting_Ships.Append (Ship);
   end Add_Ship;

   ------------
   -- Create --
   ------------

   function Create
     (Star                : Athena.Handles.Star.Star_Handle;
      Name                : String;
      Radius              : Non_Negative_Real;
      Density             : Non_Negative_Real;
      Rotation_Period     : Non_Negative_Real;
      Tilt                : Athena.Trigonometry.Angle;
      Seed                : Integer;
      Semimajor_Axis      : Non_Negative_Real;
      Zero_Time_Angle     : Athena.Trigonometry.Angle;
      Eccentricity        : Non_Negative_Real;
      Inclination         : Athena.Trigonometry.Angle;
      Period              : Non_Negative_Real;
      Mass                : Non_Negative_Real;
      Composition         : World_Composition;
      Climate             : World_Climate;
      Orbit_Zone          : Stellar_Orbit_Zone;
      Gas_Giant           : Boolean;
      Age                 : Non_Negative_Real;
      Habitability        : Non_Negative_Real;
      Resource            : Unit_Real;
      Surface_Pressure    : Non_Negative_Real;
      Average_Temperature : Non_Negative_Real;
      Hydrosphere         : Non_Negative_Real;
      Life                : Life_Complexity_Type;
      Smoothness          : Natural;
      Elevation_Range     : Natural;
      Sea_Level           : Natural)
      return World_Handle
   is
      Deposits : Deposit_Lists.List;
   begin
      if Map.Contains (Name) then
         raise Constraint_Error with
           "multiple worlds called '" & Name & "'";
      end if;

      declare
         use Athena.Handles.Commodity;
      begin
         for Commodity of All_Commodities loop
            if Commodity.Class = Athena.Handles.Commodity.Resource
              and then not Commodity.Is_Abstract
            then
               declare
                  Quality : constant Non_Negative_Real :=
                              Resource
                                * Execute (Commodity.Deposit_Constraint);
               begin
                  Athena.Logging.Log
                    ("world " & Name
                     & ": resource " & Image (Resource)
                     & "; deposit "
                     & Commodity.Tag
                     & "; quality "
                     & Image (Quality));
                  Deposits.Append
                    (Deposit_Record'
                       (Resource => Commodity.Reference,
                        Quality  => Quality));
               end;
            end if;
         end loop;
      end;

      Vector.Append
        (World_Record'
           (Identifier          => Next_Identifier,
            World_Name          => +Name,
            Star                => Star.Reference,
            Mass                => Mass,
            Semimajor_Axis      => Semimajor_Axis,
            Zero_Time_Angle     => Zero_Time_Angle,
            Eccentricity        => Eccentricity,
            Inclination         => Inclination,
            Radius              => Radius,
            Density             => Density,
            Rotation_Period     => Rotation_Period,
            Tilt                => Tilt,
            Seed                => Seed,
            Period              => Period,
            Composition         => Composition,
            Climate             => Climate,
            Orbit_Zone          => Orbit_Zone,
            Gas_Giant           => Gas_Giant,
            Age                 => Age,
            Habitability        => Habitability,
            Surface_Pressure    => Surface_Pressure,
            Atmosphere          => <>,
            Average_Temperature => Average_Temperature,
            Hydrosphere         => Hydrosphere,
            Life                => Life,
            Smoothness          => Smoothness,
            Elevation_Range     => Elevation_Range,
            Sea_Level           => Sea_Level,
            Space               =>
              Natural (Radius / Athena.Solar_System.Earth_Radius * 10_000.0),
            Resource            => Resource,
            Deposits            => <>,
            Owner               => Null_Empire_Reference,
            Occupier            => Null_Empire_Reference,
            Colony              => Null_Colony_Reference,
            Orbiting_Ships      => <>));
      Map.Insert (Name, Vector.Last_Index);
      Star.Add_World (Vector.Last_Index);
      return World_Handle'
        (Has_Element => True,
         Reference   => Vector.Last_Index);

   end Create;

   ----------------------
   -- Extract_Resource --
   ----------------------

   function Extract_Resource
     (Handle   : World_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle;
      Size     : Non_Negative_Real)
      return Non_Negative_Real
   is
   begin
      for Item of Vector (Handle.Reference).Deposits loop
         if Item.Resource = Resource.Reference then
            declare
               Quality : constant Non_Negative_Real := Item.Quality;
               Change  : constant Real :=
                           (if Size <= 1.0
                            then 0.0
                            else Quality
                            * Athena.Elementary_Functions.Log (Size)
                            / 100000.0);
            begin
               return Extracted : constant Non_Negative_Real :=
                 Size * Change * 10000.0
               do
                  Item.Quality :=
                    (if Change < Quality
                     then Quality - Change
                     else Quality / 2.0);
                  Handle.Log
                    (Resource.Tag
                     & ": quality " & Image (Quality)
                     & "; size " & Image (Size)
                     & "; change " & Image (Change)
                     & "; new quality " & Image (Item.Quality)
                     & "; extracted " & Image (Extracted));
               end return;
            end;
         end if;
      end loop;
      return 0.0;
   end Extract_Resource;

   ---------------
   -- Find_World --
   ---------------

   function Find_World
     (Test : not null access function (Handle : World_Handle) return Boolean)
      return World_Handle
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         if Test (Get (Reference)) then
            return Get (Reference);
         end if;
      end loop;
      return (False, 0);
   end Find_World;

   -----------------
   -- Get_By_Name --
   -----------------

   function Get_By_Name (Name : String) return World_Handle is
      use World_Maps;
      Position : constant Cursor := Map.Find (Name);
   begin
      return Handle : World_Handle do
         if Has_Element (Position) then
            Handle.Reference := Element (Position);
            Handle.Has_Element := True;
         end if;
      end return;
   end Get_By_Name;

   ----------------------------
   -- Iterate_Orbiting_Ships --
   ----------------------------

   procedure Iterate_Orbiting_Ships
     (World         : World_Handle;
      Process       : not null access
        procedure (Reference : Ship_Reference))
   is
   begin
      for Reference of Vector (World.Reference).Orbiting_Ships loop
         Process (Reference);
      end loop;
   end Iterate_Orbiting_Ships;

   -------------------
   -- Iterate_Worlds --
   -------------------

   procedure Iterate_Worlds
     (Process      : not null access
        procedure (Handle : World_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end Iterate_Worlds;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      World_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).World_Name), I);
      end loop;
   end Load;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (World : World_Handle;
      Ship  : Ship_Reference)
   is
      use Ship_Lists;
      Rec      : World_Record renames Vector (World.Reference);
      Position : Cursor := Rec.Orbiting_Ships.Find (Ship);
   begin
      pragma Assert (Has_Element (Position),
                     "cannot remove ship" & Ship'Image
                     & " from " & World.Name
                     & " because it is not there");
      Rec.Orbiting_Ships.Delete (Position);
   end Remove_Ship;

   ----------------------
   -- Resource_Quality --
   ----------------------

   function Resource_Quality
     (Handle   : World_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle)
      return Non_Negative_Real
   is
   begin
      for Item of Vector (Handle.Reference).Deposits loop
         if Item.Resource = Resource.Reference then
            return Item.Quality;
         end if;
      end loop;
      return 0.0;
   end Resource_Quality;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      World_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ----------------
   -- Set_Colony --
   ----------------

   procedure Set_Colony
     (World   : World_Handle;
      Colony  : Colony_Reference)
   is
   begin
      Vector (World.Reference).Colony := Colony;
   end Set_Colony;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (World     : World_Handle;
      New_Name  : String)
   is
   begin
      Vector (World.Reference).World_Name := +New_Name;
   end Set_Name;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (World      : World_Handle;
      New_Owner  : Empire_Reference)
   is
   begin
      Vector (World.Reference).Owner := New_Owner;
   end Set_Owner;

end Athena.Handles.World;
