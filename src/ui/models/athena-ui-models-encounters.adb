with Ada.Text_IO;
with Athena.Color;

with Nazar.Colors;
with Nazar.Main;

with Athena.Elementary_Functions;
with Athena.Options;
with Athena.Trigonometry;

with Athena.Encounters.Frames;
with Athena.Encounters.Manager;
with Timon.Vectors;

package body Athena.UI.Models.Encounters is

   type Root_Encounter_Model is tagged;
   type Encounter_Model_Access is access all Root_Encounter_Model'Class;

   task type Tick_Task is
      entry Start (Model    : Encounter_Model_Access;
                   Interval : Duration);
      entry Stop;
   end Tick_Task;

   type Tick_Task_Access is access Tick_Task;

   type Root_Encounter_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Encounter    : Athena.Encounters.Frames.Frame_Sequence_Type;
         Current_Tick : Athena.Encounters.Encounter_Tick := 1;
         Ticker       : Tick_Task_Access;
      end record;

   overriding function Background_Color
     (View : Root_Encounter_Model)
      return Nazar.Colors.Nazar_Color
   is (0.0, 0.0, 0.0, 1.0);

   overriding procedure Reload
     (Model : in out Root_Encounter_Model);

   overriding procedure Unload
     (Model : in out Root_Encounter_Model);

   procedure Draw_Encounter
     (Model : in out Root_Encounter_Model'Class);

   procedure Draw_Shape
     (Model     : in out Root_Encounter_Model'Class;
      Shape     : Athena.Encounters.Shape_Type;
      Size      : Non_Negative_Real;
      Color     : Athena.Color.Athena_Color;
      X1, Y1    : Real;
      X2, Y2    : Real;
      Heading   : Athena.Trigonometry.Angle;
      Condition : Unit_Real;
      Shield    : Unit_Real);

   procedure Draw_Ship
     (Model       : in out Root_Encounter_Model'Class;
      Size        : Non_Negative_Real;
      Color       : Nazar.Colors.Nazar_Color;
      Location    : Athena.Encounters.Encounter_Point;
      Destination : Athena.Encounters.Encounter_Point;
      Heading     : Athena.Trigonometry.Angle;
      Condition   : Unit_Real;
      Shield      : Unit_Real);

   procedure Draw_Beam
     (Model    : in out Root_Encounter_Model'Class;
      Color    : Nazar.Colors.Nazar_Color;
      Location : Athena.Encounters.Encounter_Point;
      Target   : Athena.Encounters.Encounter_Point);

   function To_Nazar_Color
     (Color : Athena.Color.Athena_Color)
      return Nazar.Colors.Nazar_Color
   is (Nazar.Colors.Nazar_Color'
         (Red   =>
             Nazar.Nazar_Unit_Float (Color.Red),
          Green =>
             Nazar.Nazar_Unit_Float (Color.Green),
          Blue  =>
             Nazar.Nazar_Unit_Float (Color.Blue),
          Alpha =>
             Nazar.Nazar_Unit_Float (Color.Alpha)));

   ---------------
   -- Draw_Beam --
   ---------------

   procedure Draw_Beam
     (Model    : in out Root_Encounter_Model'Class;
      Color    : Nazar.Colors.Nazar_Color;
      Location : Athena.Encounters.Encounter_Point;
      Target   : Athena.Encounters.Encounter_Point)
   is
   begin

      Model.Save_State;
      Model.Set_Color (Color);
      Model.Move_To (Nazar.Nazar_Float (Location.X),
                     Nazar.Nazar_Float (Location.Y));
      Model.Line_To (Nazar.Nazar_Float (Target.X),
                     Nazar.Nazar_Float (Target.Y));
      Model.Render;
      Model.Restore_State;
   end Draw_Beam;

   --------------------
   -- Draw_Encounter --
   --------------------

   procedure Draw_Encounter
     (Model : in out Root_Encounter_Model'Class)
   is
      Left   : Real := -200.0;
      Right  : Real := 200.0;
      Top    : Real := -200.0;
      Bottom : Real := 200.0;

      procedure Process_Sprite
        (Actor  : Athena.Encounters.Frames.Frame_Actor_Type;
         Sprite : Athena.Encounters.Frames.Frame_Element_Type);

      --------------------
      -- Process_Shape --
      --------------------

      procedure Process_Sprite
        (Actor : Athena.Encounters.Frames.Frame_Actor_Type;
         Sprite : Athena.Encounters.Frames.Frame_Element_Type)
      is
         use Timon.Vectors;
         Position    : constant Vector_3 := Sprite.Position;
         Velocity    : constant Vector_3 := Sprite.Velocity;
         Orientation : constant Vector_3 := Sprite.Orientation;
         Target      : constant Vector_3 := Position + Velocity;
         X1          : constant Real := X (Position);
         Y1          : constant Real := Y (Position);
         X2          : constant Real := X (Target);
         Y2          : constant Real := Y (Target);
      begin

         Model.Draw_Shape
           (Shape     => Actor.Shape,
            Size      => Actor.Size,
            Color     => Actor.Color,
            X1        => X1,
            Y1        => Y1,
            X2        => X2,
            Y2        => Y2,
            Heading   =>
              Athena.Trigonometry.Arctan
                (Y (Orientation), X (Orientation)),
            Condition => Sprite.Condition,
            Shield    => Sprite.Shields);
         Left := Real'Min (Left, X1);
         Right := Real'Max (Right, X1);
         Top := Real'Min (Top, Y1);
         Bottom := Real'Max (Bottom, Y1);
      end Process_Sprite;

   begin
      Model.Clear;

      Athena.Encounters.Frames.Iterate_Elements
        (Model.Encounter, Model.Current_Tick, Process_Sprite'Access);

      declare
         use Nazar;
         Width  : constant Non_Negative_Real := Right - Left;
         Height : constant Non_Negative_Real := Bottom - Top;
      begin

         Model.Set_Bounding_Box
           (Box => Rectangle'
              (X => Nazar_Float (Left - Width / 20.0),
               Y => Nazar_Float (Top - Height / 20.0),
               W => Nazar_Float (Width * 1.1),
               H => Nazar_Float (Height * 1.1)));
      end;

      declare
         use type Athena.Encounters.Encounter_Tick;
      begin
         Model.Current_Tick := Model.Current_Tick + 1;
      end;

   end Draw_Encounter;

   -----------------
   -- Draw_Shape --
   -----------------

   procedure Draw_Shape
     (Model     : in out Root_Encounter_Model'Class;
      Shape     : Athena.Encounters.Shape_Type;
      Size      : Non_Negative_Real;
      Color     : Athena.Color.Athena_Color;
      X1, Y1    : Real;
      X2, Y2    : Real;
      Heading   : Athena.Trigonometry.Angle;
      Condition : Unit_Real;
      Shield    : Unit_Real)
   is
      use Athena.Encounters;
   begin
      case Shape is
         when Military_Shape_Type | Civilian_Shape_Type =>
            Model.Draw_Ship
              (Size        =>
                  Athena.Elementary_Functions.Sqrt (Size) * 4.0,
               Color       => To_Nazar_Color (Color),
               Location    => (X1, Y1),
               Destination => (X2, Y2),
               Heading     => Heading,
               Condition   => Condition,
               Shield      => Shield);
         when Beam_Weapon =>
            declare
               Color : constant Nazar.Nazar_Unit_Float :=
                         Nazar.Nazar_Unit_Float (Shield);
            begin
               Model.Draw_Beam
                 (Color    => (Color, Color, Color, 1.0),
                  Location => (X1, Y1),
                  Target   => (X2, Y2));
            end;
         when Missile_Weapon =>
            Model.Draw_Ship
              (Size        => 2.0,
               Color       => To_Nazar_Color (Color),
               Location    => (X1, Y1),
               Destination => (X2, Y2),
               Heading     => Heading,
               Condition   => 1.0,
               Shield      => 0.0);

         when others =>
            null;
      end case;
   end Draw_Shape;

   ---------------
   -- Draw_Ship --
   ---------------

   procedure Draw_Ship
     (Model       : in out Root_Encounter_Model'Class;
      Size        : Non_Negative_Real;
      Color       : Nazar.Colors.Nazar_Color;
      Location    : Athena.Encounters.Encounter_Point;
      Destination : Athena.Encounters.Encounter_Point;
      Heading     : Athena.Trigonometry.Angle;
      Condition   : Unit_Real;
      Shield      : Unit_Real)
   is
      use Athena.Encounters;
      A      : constant Encounter_Point :=
                 Rotate (Translate (Location, X => Size * 0.75),
                         Location,
                         Heading);
      B      : constant Encounter_Point :=
                 Rotate (Translate
                         (Location,
                            X => -Size / 2.0,
                            Y => Size / 2.0),
                         Location,
                         Heading);
      C      : constant Encounter_Point :=
                 Rotate (Translate
                         (Location,
                            X => -Size / 2.0,
                            Y => -Size / 2.0),
                         Location,
                         Heading);
      Cond_Color : constant Nazar.Colors.Nazar_Color :=
                     (Color with
                      delta Alpha =>
                        Nazar.Nazar_Unit_Float (Condition * 0.8 + 0.2));
   begin

      Model.Save_State;
      Model.Set_Color (Cond_Color);
      Model.Set_Fill (True);
      Model.Move_To (Nazar.Nazar_Float (A.X),
                     Nazar.Nazar_Float (A.Y));
      Model.Line_To (Nazar.Nazar_Float (B.X),
                     Nazar.Nazar_Float (B.Y));
      Model.Line_To (Nazar.Nazar_Float (C.X),
                     Nazar.Nazar_Float (C.Y));
      Model.Line_To (Nazar.Nazar_Float (A.X),
                     Nazar.Nazar_Float (A.Y));
      Model.Render;
      Model.Restore_State;

      if Shield > 0.0 then
         declare
            E : constant Nazar.Nazar_Unit_Float :=
                  Nazar.Nazar_Unit_Float (Shield * 0.75 + 0.25);
         begin
            Model.Save_State;
            Model.Set_Color (E, E, E, 1.0);
            Model.Move_To (Nazar.Nazar_Float (Location.X),
                           Nazar.Nazar_Float (Location.Y));
            Model.Circle (Nazar.Nazar_Float (Size * 0.75));
            Model.Render;
            Model.Restore_State;
         end;
      end if;

      if Destination /= Location
        and then Athena.Options.Show_Destination
      then
         Model.Save_State;
         Model.Set_Color (0.5, 0.5, 0.5, 1.0);
         Model.Move_To (Nazar.Nazar_Float (Location.X),
                        Nazar.Nazar_Float (Location.Y));
         Model.Line_To (Nazar.Nazar_Float (Destination.X),
                        Nazar.Nazar_Float (Destination.Y));
         Model.Render;
         Model.Restore_State;
      end if;

   end Draw_Ship;

   ---------------------
   -- Encounter_Model --
   ---------------------

   function Encounter_Model
     (Encounter : Minerva.Encounter.Encounter_Class)
      return Nazar.Models.Draw.Nazar_Draw_Model
   is
      Model : constant Encounter_Model_Access :=
                new Root_Encounter_Model;
   begin
      Ada.Text_IO.Put ("Loading encounter ... ");
      Ada.Text_IO.Flush;

      Athena.Encounters.Manager.Load_Encounter
        (Encounter => Encounter,
         Sequence  => Model.Encounter);
      Model.Draw_Encounter;

      Model.Ticker := new Tick_Task;
      Model.Ticker.Start (Model, 0.1);

      Ada.Text_IO.Put_Line ("done");

      return Nazar.Models.Draw.Nazar_Draw_Model (Model);
   end Encounter_Model;

   ------------
   -- Reload --
   ------------

   overriding procedure Reload
     (Model : in out Root_Encounter_Model)
     is
   begin
      Model.Draw_Encounter;
      Model.Notify_Observers;
   end Reload;

   ---------------
   -- Tick_Task --
   ---------------

   task body Tick_Task is
      Tick_Model  : Encounter_Model_Access;
      Tick_Length : Duration;

      procedure Reload;

      ------------
      -- Reload --
      ------------

      procedure Reload is
      begin
         Tick_Model.Reload;
      end Reload;

   begin
      accept Start (Model    : Encounter_Model_Access;
                    Interval : Duration)
      do
         Tick_Model := Model;
         Tick_Length := Interval;
      end Start;

      loop
         select
            accept Stop;
            exit;
         or
            delay Tick_Length;
            Nazar.Main.With_Render_Lock (Reload'Access);
         end select;
      end loop;
   end Tick_Task;

   ------------
   -- Unload --
   ------------

   overriding procedure Unload
     (Model : in out Root_Encounter_Model)
   is
   begin
      Model.Ticker.Stop;
   end Unload;

end Athena.UI.Models.Encounters;
