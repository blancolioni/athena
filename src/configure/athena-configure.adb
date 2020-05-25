with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Text_IO;

with WL.String_Sets;

with Athena.Elementary_Functions;
with Athena.Random;
with Athena.Real_Images;

with Athena.Configure.Ships;

with Athena.Handles.Star;

package body Athena.Configure is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   -------------------
   -- Create_Galaxy --
   -------------------

   procedure Create_Galaxy
     (Radius_X, Radius_Y : Non_Negative_Real;
      Star_Count         : Positive;
      Name_Generator     : WL.Random.Names.Name_Generator)
   is
      use Athena.Elementary_Functions;
      Area : constant Non_Negative_Real :=
               Ada.Numerics.Pi * Radius_X * Radius_Y;
      Area_Per_Star : constant Non_Negative_Real :=
                        Area / Real (Star_Count);
      Radius_Per_Star : constant Non_Negative_Real :=
                          Sqrt (Area_Per_Star / Ada.Numerics.Pi);

      Minimum_Distance : constant Non_Negative_Real :=
                           Radius_Per_Star / 2.0;
      Stored_Nearest_Count : constant := 24;

      type Star_Distance_Record is
         record
            To       : Positive;
            Distance : Non_Negative_Real;
         end record;

      package Star_Distance_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Star_Distance_Record);

      function Closer (Left, Right : Star_Distance_Record) return Boolean
      is (Left.Distance < Right.Distance);

      package Sorting is
        new Star_Distance_Lists.Generic_Sorting (Closer);

      type Generated_Star_Record is
         record
            X, Y    : Real;
            Handle  : Athena.Handles.Star.Star_Handle;
            Nearest : Star_Distance_Lists.List;
         end record;

      package Generated_Star_Vectors is
        new Ada.Containers.Vectors (Positive, Generated_Star_Record);

      Vector : Generated_Star_Vectors.Vector;

      function Distance
        (From, To : Generated_Star_Record)
         return Non_Negative_Real
      is ((From.X - To.X) ** 2
          + (From.Y - To.Y) ** 2);

      Name_Set : WL.String_Sets.Set;

      function Create_System_Name return String;

      ------------------------
      -- Create_System_Name --
      ------------------------

      function Create_System_Name return String is
      begin
         loop
            declare
               Name : constant String :=
                 WL.Random.Names.Random_Name (Name_Generator);
            begin
               if not Name_Set.Contains (Name) then
                  Name_Set.Include (Name);
                  return Name;
               end if;
            end;
         end loop;
      end Create_System_Name;

   begin

      if Area = 0.0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "At least one of galaxy-radius, galaxy-radius-[xyz]"
            & " must have a value");
         raise Program_Error;
      end if;

      Ada.Text_IO.Put_Line
        ("Galaxy size: ("
         & Image (Radius_X)
         & ","
         & Image (Radius_Y)
         & ")");
      Ada.Text_IO.Put_Line
        ("Area (ly**2): "
         & Image (Area)
         & " per star: "
         & Image (Area_Per_Star));
      Ada.Text_IO.Put_Line
        ("Average star distance: "
         & Image (Radius_Per_Star));
      Ada.Text_IO.Put_Line
        ("Minimum star distance: "
         & Image (Minimum_Distance));

      for I in 1 .. Star_Count loop

         declare
            Rec : Generated_Star_Record;
            OK  : Boolean := False;
            Min : constant Non_Negative_Real := Minimum_Distance ** 2;
         begin
            while not OK loop
               if I = 1 then
                  Rec.X := 0.0;
                  Rec.Y := 0.0;
               elsif False then
                  Rec.X := (Athena.Random.Unit_Random - 0.5) * 2.0;
                  Rec.Y := (Athena.Random.Unit_Random - 0.5) * 2.0;
               else
                  Rec.X := Athena.Random.Normal_Random (0.4);
                  Rec.Y := Athena.Random.Normal_Random (0.4);
               end if;

               Rec.X := Rec.X * Radius_X;
               Rec.Y := Rec.Y * Radius_Y;

               OK := True;
               for J in 1 .. I - 1 loop
                  if Distance (Rec, Vector.Element (J)) < Min then
                     OK := False;
                     exit;
                  end if;
               end loop;
            end loop;

            Vector.Append (Rec);
         end;
      end loop;

      for I in 1 .. Star_Count loop
         declare
            From : Generated_Star_Record := Vector.Element (I);
         begin
            for J in 1 .. Star_Count loop
               if I /= J then
                  declare
                     D : constant Non_Negative_Real :=
                           Distance (Vector.Element (I), Vector.Element (J));
                     Length : constant Natural :=
                                Natural (From.Nearest.Length);
                  begin
                     if Length < Stored_Nearest_Count
                       or else D < From.Nearest.Last_Element.Distance
                     then
                        if Length >= Stored_Nearest_Count then
                           From.Nearest.Delete_Last;
                        end if;

                        declare
                           use Star_Distance_Lists;
                           Position : Cursor := From.Nearest.First;
                        begin
                           while Has_Element (Position)
                             and then D >= Element (Position).Distance
                           loop
                              Next (Position);
                           end loop;
                           From.Nearest.Insert (Position, (J, D));
                        end;
                     end if;
                  end;
               end if;
            end loop;

            if False then
               Sorting.Sort (From.Nearest);
            end if;

            Vector.Replace_Element (I, From);
         end;
      end loop;

      for I in 1 .. Star_Count loop

         declare
            Gen          : constant Generated_Star_Record :=
                             Vector.Element (I);
            Star_Name    : constant String := Create_System_Name;
            Space        : constant Positive :=
                             WL.Random.Random_Number (1, 2_500);
            Resource     : constant Unit_Real :=
                             Athena.Random.Unit_Random;
            Habitability : constant Unit_Real :=
                             Athena.Random.Unit_Random;
            Star_Handle  : constant Athena.Handles.Star.Star_Handle :=
                             Athena.Handles.Star.Create
                               (X             => Gen.X,
                                Y             => Gen.Y,
                                Core_Distance =>
                                  Sqrt (Gen.X ** 2 + Gen.Y ** 2),
                                Name          => Star_Name,
                                Space         => Space,
                                Resource      => Resource,
                                Habitability  => Habitability);
         begin

            Vector (I).Handle := Star_Handle;

         end;
      end loop;

      Ada.Text_IO.Put ("calculating distances ...");
      Ada.Text_IO.Flush;

      Athena.Handles.Star.Calculate_Distances;

      Ada.Text_IO.Put_Line (" done");

   end Create_Galaxy;

   -------------------------
   -- Initialize_Database --
   -------------------------

   procedure Initialize_Database is
   begin
      --  Athena.Handles.Turn.Create (1, True);
      --
      --  Athena.Handles.Manager.Create (Tag => "explore", Priority => 1060);
      --  Athena.Handles.Manager.Create (Tag => "colonize", Priority => 1080);
      --  Athena.Handles.Manager.Create (Tag => "upgrade", Priority => 10);
      --  Athena.Handles.Manager.Create (Tag => "repair", Priority => 10);
      --  Athena.Handles.Manager.Create (Tag => "defend", Priority => 1030);
      --  Athena.Handles.Manager.Create (Tag => "attack", Priority => 800);
      --  Athena.Handles.Manager.Create (Tag => "develop", Priority => 1050);
      --  Athena.Handles.Manager.Create (Tag => "research", Priority => 100);
      --  Athena.Handles.Manager.Create (Tag => "transport", Priority => 1040);
      --
      --  Athena.Handles.Technology.Create (Tag => "drive");
      --  Athena.Handles.Technology.Create (Tag => "weapon");
      --  Athena.Handles.Technology.Create (Tag => "shield");
      --  Athena.Handles.Technology.Create (Tag => "cargo");
      --

      Athena.Configure.Ships.Configure_Ships;

      null;
   end Initialize_Database;

end Athena.Configure;
