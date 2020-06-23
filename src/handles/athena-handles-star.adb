with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Tropos.Reader;

with Athena.Elementary_Functions;

with Athena.Solar_System;

with Athena.Configure.Worlds;

with Athena.Paths;

package body Athena.Handles.Star is

   type Star_Info_Record is
      record
         Solar_Masses : Non_Negative_Real;
         Class        : Star_Spectral_Class;
         Subclass     : Natural;
         Surface_Temp : Non_Negative_Real;
         Color        : Athena.Color.Athena_Color;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
      end record;

   package Star_Info_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Star_Info_Record);

   Main_Sequence_Table : Star_Info_Lists.List;

   procedure Read_Tables;

   procedure Check_Tables;

   function Brighten
     (Color       : Athena.Color.Athena_Color;
      Temperature : Non_Negative_Real)
      return Athena.Color.Athena_Color;

   Stored_Nearest_Count : constant := 24;

   type Neighbour_Record is
      record
         Reference : Star_Reference;
         Distance  : Non_Negative_Real;
      end record;

   package Neighbour_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Neighbour_Record);

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists (World_Reference);

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Reference);

   type Star_Record is
      record
         Identifier        : Object_Identifier;
         Star_Name         : Ada.Strings.Unbounded.Unbounded_String;
         X, Y              : Real;
         Generated         : Boolean;
         Mass              : Non_Negative_Real;
         Spectral_Class    : Star_Spectral_Class;
         Spectral_Subclass : Natural;
         Luminosity        : Non_Negative_Real;
         Temperature       : Non_Negative_Real;
         Age               : Non_Negative_Real;
         Color             : Athena.Color.Athena_Color;
         Owner             : Empire_Reference;
         Occupier          : Empire_Reference;
         Core_Distance     : Non_Negative_Real;
         Neighbours        : Neighbour_Lists.List;
         Worlds            : World_Lists.List;
         Ships             : Ship_Lists.List;
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

   function Color
     (Star : Star_Handle)
      return Athena.Color.Athena_Color
   is (Vector (Star.Reference).Color);

   function Has_Owner
     (Star : Star_Handle)
      return Boolean
   is (Vector (Star.Reference).Owner /= 0);

   function Owner
     (Star : Star_Handle)
      return Empire_Reference
   is (Vector (Star.Reference).Owner);

   function Age
     (Star : Star_Handle)
      return Non_Negative_Real
   is (Vector (Star.Reference).Age);

   function Luminosity
     (Star : Star_Handle)
      return Non_Negative_Real
   is (Vector (Star.Reference).Luminosity);

   function Temperature
     (Star : Star_Handle)
      return Non_Negative_Real
   is (Vector (Star.Reference).Temperature);

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Athena.Handles.Star.Star_Spectral_Class;
      Subclass     : out Natural;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      Color        : out Athena.Color.Athena_Color);

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship
     (Star : Star_Handle;
      Ship : Ship_Reference)
   is
   begin
      Vector (Star.Reference).Ships.Append (Ship);
   end Add_Ship;

   ---------------
   -- Add_World --
   ---------------

   procedure Add_World
     (Star  : Star_Handle;
      World : World_Reference)
   is
   begin
      Vector (Star.Reference).Worlds.Append (World);
   end Add_World;

   --------------
   -- Brighten --
   --------------

   function Brighten
     (Color       : Athena.Color.Athena_Color;
      Temperature : Non_Negative_Real)
      return Athena.Color.Athena_Color
   is
      R : constant Real :=
            Color.Red * Temperature * (0.0534 / 255.0) - (43.0 / 255.0);
      G : constant Real :=
            Color.Green * Temperature * (0.0628 / 255.0) - (77.0 / 255.0);
      B : constant Real :=
            Color.Blue * Temperature * (0.0735 / 255.0) - (115.0 / 255.0);
   begin
      return (Unit_Clamp (R), Unit_Clamp (G), Unit_Clamp (B), Color.Alpha);
   end Brighten;

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

   ------------------
   -- Check_Tables --
   ------------------

   procedure Check_Tables is
   begin
      if Main_Sequence_Table.Is_Empty then
         Read_Tables;
      end if;
   end Check_Tables;

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
      Class        : Star_Spectral_Class;
      Subclass     : Natural;
      Radius       : Non_Negative_Real;
      Luminosity   : Non_Negative_Real;
      Color        : Athena.Color.Athena_Color;
   begin

      if Map.Contains (Name) then
         raise Constraint_Error with
           "multiple stars called '" & Name & "'";
      end if;

      Get_Main_Sequence_Info
        (Solar_Masses => Solar_Masses,
         Class        => Class,
         Subclass     => Subclass,
         Radius       => Radius,
         Luminosity   => Luminosity,
         Color        => Color);

      declare
         use Athena.Elementary_Functions;
         Temperature : constant Non_Negative_Real :=
                         (Luminosity ** 0.25)
                         * Athena.Solar_System.Solar_Surface_Temperature;
         Age         : constant Non_Negative_Real :=
                         1.0e10
                           * Solar_Masses
                         / Luminosity;
      begin
         Vector.Append
           (Star_Record'
              (Identifier        => Next_Identifier,
               Star_Name         => +Name,
               X                 => X,
               Y                 => Y,
               Generated         => True,
               Mass              =>
                 Athena.Solar_System.Solar_Mass * Solar_Masses,
               Spectral_Class    => Class,
               Spectral_Subclass => Subclass,
               Luminosity        => Luminosity,
               Temperature       => Temperature,
               Age               => Age,
               Color             => Color,
               Owner             => 0,
               Occupier          => 0,
               Core_Distance     => Core_Distance,
               Neighbours        => <>,
               Ships             => <>,
               Worlds            => <>));
         Map.Insert (Name, Vector.Last_Index);
      end;

      return Star : constant Star_Handle := Star_Handle'
        (Has_Element => True,
         Reference   => Vector.Last_Index)
      do
         Athena.Configure.Worlds.Generate_Worlds (Star);
      end return;

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

   ----------------------------
   -- Get_Main_Sequence_Info --
   ----------------------------

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Athena.Handles.Star.Star_Spectral_Class;
      Subclass     : out Natural;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      Color        : out Athena.Color.Athena_Color)
   is
      use Star_Info_Lists;
      Position : Cursor;
   begin

      Check_Tables;

      Position := Main_Sequence_Table.First;

      while Has_Element (Position) loop
         declare
            Info : constant Star_Info_Record := Element (Position);
         begin
            exit when Solar_Masses >= Info.Solar_Masses;
         end;
         Next (Position);
      end loop;

      if not Has_Element (Position) then
         Position := Main_Sequence_Table.Last;
      end if;

      declare
         Info : Star_Info_Record renames Element (Position);
      begin
         Class := Info.Class;
         Subclass := Info.Subclass;
         Radius := Info.Radius;
         Luminosity := Info.Luminosity;
         Color := Info.Color;
      end;

   end Get_Main_Sequence_Info;

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

   -------------------
   -- Iterate_Ships --
   -------------------

   procedure Iterate_Ships
     (Star         : Star_Handle;
      Process      : not null access
        procedure (Reference : Ship_Reference))
   is
   begin
      for Reference of Vector (Star.Reference).Ships loop
         Process (Reference);
      end loop;
   end Iterate_Ships;

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

   --------------------
   -- Iterate_Worlds --
   --------------------

   procedure Iterate_Worlds
     (Star         : Star_Handle;
      Process      : not null access
        procedure (Reference : World_Reference))
   is
   begin
      if not Vector (Star.Reference).Generated then
         Athena.Configure.Worlds.Generate_Worlds (Star);
         Vector (Star.Reference).Generated := True;
      end if;

      for World of Vector (Star.Reference).Worlds loop
         Process (World);
      end loop;
   end Iterate_Worlds;

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
   -- Read_Tables --
   -----------------

   procedure Read_Tables is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_CSV_Config
                   (Athena.Paths.Config_File
                      ("star-systems/star-classification.txt"),
                    Header_Line   => True,
                    Separator     => ';',
                    Extend_Header => False);
   begin
      for Info_Config of Config loop
         declare

            function Get (Name : String) return Real
            is (Real (Float'(Info_Config.Get (Name))));

            Class        : constant String := Info_Config.Get ("type");
            Surface_Temp : constant Real := Get ("surface-temp");
            Radius       : constant Real := Get ("radius");
            Mass         : constant Real := Get ("mass");
            Luminosity   : constant Real := Get ("luminosity");
            Red          : constant Natural := Info_Config.Get ("r");
            Green        : constant Natural := Info_Config.Get ("g");
            Blue         : constant Natural := Info_Config.Get ("b");
            Color       : constant Athena.Color.Athena_Color :=
                             Athena.Color.Athena_Color'
                               (Red   => Real (Red) / 255.0,
                                Green => Real (Green) / 255.0,
                                Blue  => Real (Blue) / 255.0,
                                Alpha => 1.0);
            Info         : constant Star_Info_Record :=
                             (Solar_Masses => Mass,
                              Class        =>
                                Star_Spectral_Class'Value
                                  ((1 => Class (Class'First))),
                              Subclass     =>
                                Natural'Value
                                  ((1 => Class (Class'First + 1))),
                              Surface_Temp => Surface_Temp,
                              Color       =>
                                Brighten (Color, Surface_Temp),
                              Radius       => Radius,
                              Luminosity   => Luminosity);
         begin
            Main_Sequence_Table.Append (Info);
         end;
      end loop;
   end Read_Tables;

   -----------------
   -- Remove_Ship --
   -----------------

   procedure Remove_Ship
     (Star : Star_Handle;
      Ship : Ship_Reference)
   is
      use Ship_Lists;
      Rec : Star_Record renames Vector (Star.Reference);
      Position : Cursor := Rec.Ships.Find (Ship);
   begin
      pragma Assert (Has_Element (Position),
                     "cannot remove ship" & Ship'Image
                     & " from " & Star.Name
                     & " because it is not there");
      Rec.Ships.Delete (Position);
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

   --------------------
   -- Spectral_Class --
   --------------------

   function Spectral_Class
     (Star : Star_Handle)
      return String
   is
      Rec : Star_Record renames Vector (Star.Reference);
   begin
      return Star_Spectral_Class'Image (Rec.Spectral_Class)
        & Character'Val (Character'Pos ('0') + Rec.Spectral_Subclass);
   end Spectral_Class;

end Athena.Handles.Star;
