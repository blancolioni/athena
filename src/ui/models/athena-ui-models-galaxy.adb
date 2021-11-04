with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Nazar.Colors;
with Nazar.Main;

with Athena.Calendar;
with Athena.Color;
with Athena.Empires;
with Athena.Voronoi_Diagrams;

--  with Minerva.Fleet;
--  with Minerva.Ship;
with Minerva.Star;

package body Athena.UI.Models.Galaxy is

   type Boundary_Point is
      record
         X, Y : Real;
      end record;

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Boundary_Point);

   type Empire_Ships_Record is
      record
         Empire : Minerva.Empire.Empire_Handle;
         Count  : Natural;
      end record;

   package Empire_Ships_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Empire_Ships_Record);

   type Star_Record is
      record
         Handle    : Minerva.Star.Star_Handle;
         Color     : Nazar.Colors.Nazar_Color;
         X, Y      : Nazar.Nazar_Float;
         Ships     : Empire_Ships_Lists.List;
         Boundary  : Point_Vectors.Vector;
         Encounter : Natural;
      end record;

   package Star_Record_Vectors is
     new Ada.Containers.Vectors (Positive, Star_Record);

   type Journey_Record is
      record
         Empire    : Minerva.Empire.Empire_Handle;
         Size      : Nazar.Nazar_Float;
         X1, Y1    : Nazar.Nazar_Float;
         X2, Y2    : Nazar.Nazar_Float;
         Departure : Athena.Calendar.Time;
         Arrival   : Athena.Calendar.Time;
         Color     : Athena.Color.Athena_Color;
      end record;

   package Journey_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Journey_Record);

   package Index_Maps is new WL.String_Maps (Positive);

   type Galaxy_Model_Record is
      record
         Empire     : Minerva.Empire.Empire_Handle;
         Stars      : Star_Record_Vectors.Vector;
         Index_Map  : Index_Maps.Map;
         Journeys   : Journey_Lists.List;
      end record;

   type Galaxy_Model_Record_Access is access Galaxy_Model_Record;

   procedure Load_Journeys
     (Data : Galaxy_Model_Record_Access);

   type Galaxy_Model_Layer is
     (Background, Boundaries, Stars, Owner, Ships, Journeys);

   type Root_Galaxy_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Data        : Galaxy_Model_Record_Access;
         Layer       : Galaxy_Model_Layer;
      end record;

   type Galaxy_Model_Access is access all Root_Galaxy_Model'Class;

   overriding function Background_Color
     (View : Root_Galaxy_Model)
      return Nazar.Colors.Nazar_Color
   is (0.0, 0.0, 0.0, 1.0);

   procedure Load_Galaxy
     (Model : Galaxy_Model_Record_Access);

   procedure Draw_Galaxy
     (Model : in out Root_Galaxy_Model'Class);

   function Orbiting_Ships
     (Around : Minerva.Star.Star_Handle)
      return Empire_Ships_Lists.List
     with Unreferenced;

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

   function To_Nazar_Color
     (Color : Natural)
      return Nazar.Colors.Nazar_Color
   is (Nazar.Colors.Nazar_Color'
         (Red   =>
             Nazar.Nazar_Unit_Float (Real (Color / 65536 mod 256) / 255.0),
          Green =>
             Nazar.Nazar_Unit_Float (Real (Color / 256 mod 256) / 255.0),
          Blue  =>
             Nazar.Nazar_Unit_Float (Real (Color mod 256) / 255.0),
          Alpha => 1.0));

   -----------------
   -- Draw_Galaxy --
   -----------------

   procedure Draw_Galaxy
     (Model : in out Root_Galaxy_Model'Class)
   is

      procedure Do_Draw;

      procedure Do_Draw is
      begin

         Model.Clear;
         Model.Save_State;

         case Model.Layer is
         when Background =>

            null;

         when Boundaries =>

            for Rec of Model.Data.Stars loop
               if Rec.Handle.Owner.Has_Element
                 or else Athena.Empires.Visited (Model.Data.Empire, Rec.Handle)
               then
                  declare
                     Owner : constant Minerva.Empire.Empire_Class :=
                               Rec.Handle.Owner;
                  begin
                     Model.Save_State;
                     Model.Set_Fill (True);
                     if Owner.Has_Element then
                        Model.Set_Color (To_Nazar_Color (Owner.Rgb));
                     else
                        declare
                           use Nazar.Colors;
                           Color : Nazar_Color :=
                                     To_Nazar_Color
                                       (Model.Data.Empire.Rgb);
                        begin
                           Color.Alpha := 0.5;
                           Model.Set_Color (Color);
                        end;
                     end if;
                  end;

                  declare
                     use Nazar;
                     First : Boolean := True;
                  begin
                     for Pt of Rec.Boundary loop
                        if First then
                           Model.Move_To
                             (Nazar_Float (Pt.X), Nazar_Float (Pt.Y));
                           First := False;
                        else
                           Model.Line_To
                             (Nazar_Float (Pt.X), Nazar_Float (Pt.Y));
                        end if;
                     end loop;

                     Model.Render;
                  end;

                  Model.Restore_State;

               end if;
            end loop;

         when Stars =>

            for Rec of Model.Data.Stars loop

               --  if Rec.Encounter > 0 then
               --     Model.Save_State;
               --     Model.Set_Fill (True);
               --     Model.Set_Color (1.0, 0.31, 0.0, 1.0);
               --     Model.Move_To (Rec.X, Rec.Y);
               --     Model.Circle
               --  (Nazar.Nazar_Float (Rec.Encounter / 5 + 10));
               --     Model.Render;
               --     Model.Restore_State;
               --  end if;

               Model.Save_State;
               Model.Set_Fill (True);
               Model.Move_To (Rec.X, Rec.Y);
               Model.Set_Color (Rec.Color);
               Model.Circle (2.0);
               Model.Render;
               Model.Restore_State;
            end loop;

         when Owner =>

            for Rec of Model.Data.Stars loop
               if Rec.Handle.Owner.Has_Element then
                  Model.Save_State;
                  Model.Set_Color (1.0, 1.0, 1.0, 1.0);
                  Model.Set_Font ("Courier", 12.0, False, False);
                  Model.Move_To (Rec.X, Rec.Y);
                  Model.Draw_Text (Rec.Handle.Name);
                  Model.Render;
                  Model.Restore_State;
               end if;
            end loop;

         when Ships =>

            for Rec of Model.Data.Stars loop
               if not Rec.Ships.Is_Empty then
                  declare
                     use type Nazar.Nazar_Float;
                     Radius : Nazar.Nazar_Float := 5.0;
                  begin
                     Model.Save_State;
                     Model.Set_Fill (False);
                     for Empire_Ship of Rec.Ships loop
                        if Rec.Handle.Owner.Has_Element
                          and then Rec.Handle.Owner.Identifier
                            = Empire_Ship.Empire.Identifier
                        then
                           Model.Set_Color ((1.0, 1.0, 1.0, 1.0));
                        else
                           Model.Set_Color
                             (To_Nazar_Color (Empire_Ship.Empire.Rgb));
                        end if;

                        Model.Move_To (Rec.X, Rec.Y);
                        Model.Circle (Radius);
                        Model.Render;
                        Radius := Radius + 2.0;
                     end loop;
                     Model.Restore_State;
                  end;
               end if;
            end loop;

         when Journeys =>

            declare
               use Athena.Calendar;
               Now : constant Athena.Calendar.Time :=
                       Athena.Calendar.Clock;
               Keep : Journey_Lists.List;
            begin
               for Journey of Model.Data.Journeys loop

                  if Journey.Arrival <= Now then
                     null;
                  else
                     Model.Save_State;

                     declare
                        use Nazar;
                        Progress : constant Nazar_Float :=
                                     Nazar_Float
                                       ((Now - Journey.Departure)
                                        / (Journey.Arrival
                                          - Journey.Departure));
                        X        : constant Nazar_Float :=
                                     Journey.X1
                                       + Progress * (Journey.X2 - Journey.X1);
                        Y        : constant Nazar_Float :=
                                     Journey.Y1
                                       + Progress * (Journey.Y2 - Journey.Y1);
                     begin
                        Model.Move_To (X, Y);
                        Model.Set_Fill (True);
                        Model.Set_Color (To_Nazar_Color (Journey.Color));
                        Model.Circle (Journey.Size);
                        Model.Render;
                        Model.Set_Fill (False);
                        Model.Set_Color ((0.8, 0.8, 0.8, 1.0));
                        Model.Circle (Journey.Size);
                        Model.Render;
                     end;

                     Model.Restore_State;
                     Keep.Append (Journey);
                  end if;
               end loop;

               Model.Data.Journeys := Keep;
            end;
         end case;
      end Do_Draw;

   begin
      Nazar.Main.With_Render_Lock (Do_Draw'Access);
   end Draw_Galaxy;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (Empire : Minerva.Empire.Empire_Class)
      return Draw_Model_Layers
   is
      Count  : constant Positive :=
                 Galaxy_Model_Layer'Pos (Galaxy_Model_Layer'Last) + 1;
      Layers : Draw_Model_Layers (1 .. Count);
      Index  : Natural := 0;

      --  Centre_Star : constant Minerva.Star.Star_Handle :=
      --                  Athena.Empires.Capital (Empire);
      Left        : constant Real := -60.0;  -- Centre_Star.X - 40.0;
      Top         : constant Real := -60.0;  -- Centre_Star.Y - 40.0;
      Right       : constant Real := 60.0;  -- Centre_Star.X + 40.0;
      Bottom      : constant Real := 60.0;  -- Centre_Star.Y + 40.0;

      Data   : constant Galaxy_Model_Record_Access :=
                 new Galaxy_Model_Record'
                   (Empire    => Empire.To_Empire_Handle,
                    Stars     => <>,
                    Index_Map => <>,
                    Journeys  => <>);
   begin

      Ada.Text_IO.Put ("Creating galaxy view ...");
      Ada.Text_IO.Flush;

      Load_Galaxy (Data);

      for Layer in Galaxy_Model_Layer loop
         Index := Index + 1;
         Layers (Index) :=
           new Root_Galaxy_Model'
             (Nazar.Models.Draw.Root_Draw_Model with
              Data        => Data,
              Layer       => Layer);
         Layers (Index).Set_Bounding_Box
           (Box => Nazar.Rectangle'
              (X => Nazar.Nazar_Float (Left),
               Y => Nazar.Nazar_Float (Top),
               W => Nazar.Nazar_Float (Right - Left),
               H => Nazar.Nazar_Float (Bottom - Top)));

         Galaxy_Model_Access (Layers (Index)).Draw_Galaxy;
      end loop;

      Ada.Text_IO.Put_Line ("done");

      return Layers;

   end Galaxy_Model;

   -----------------
   -- Load_Galaxy --
   -----------------

   procedure Load_Galaxy
     (Model : Galaxy_Model_Record_Access)
   is
      Voronoi     : Athena.Voronoi_Diagrams.Voronoi_Diagram;
   begin

      if Model.Stars.Is_Empty then
         for Star of Minerva.Star.Scan_By_Core_Distance loop
            declare
               Color : constant Nazar.Colors.Nazar_Color :=
                         (1.0, 1.0, 1.0, 1.0);
               Rec   : constant Star_Record := Star_Record'
                 (Handle    => Star.To_Star_Handle,
                  Color     => Color,
                  X         => Nazar.Nazar_Float (Star.X),
                  Y         => Nazar.Nazar_Float (Star.Y),
                  Encounter => 0,
                  Ships     => <>,
                  Boundary  => <>);
            begin
               Model.Stars.Append (Rec);
               Voronoi.Add_Point (Star.X, Star.Y);
               if Model.Index_Map.Contains (Star.Name) then
                  raise Constraint_Error with
                    "multiple systems called " & Star.Name;
               end if;

               Model.Index_Map.Insert
                 (Star.Name, Model.Stars.Last_Index);
            end;
         end loop;

         Voronoi.Generate;

         for I in 1 .. Voronoi.Polygon_Count loop
            declare
               Pts : Point_Vectors.Vector;
            begin
               for J in 1 .. Voronoi.Vertex_Count (I) loop
                  Pts.Append (Boundary_Point'
                                (Voronoi.Vertex_X (I, J),
                                 Voronoi.Vertex_Y (I, J)));
               end loop;
               Model.Stars (I).Boundary := Pts;
            end;
         end loop;

      end if;

      --  declare
      --     Knowledge : Athena.Knowledge.Stars.Star_Knowledge;
      --  begin
      --     Knowledge.Load (Model.Empire);
      --
      --     for Rec of Model.Stars loop
      --        Rec.Ships := Orbiting_Ships (Rec.Handle);
      --        Rec.Encounter := 0;
      --
      --        if Knowledge.Visited (Rec.Handle) then
      --           Left := Real'Min (Left, Rec.Handle.X - 5.0);
      --           Top  := Real'Min (Top, Rec.Handle.Y - 5.0);
      --           Right := Real'Max (Right, Rec.Handle.X + 5.0);
      --           Bottom  := Real'Max (Bottom, Rec.Handle.Y + 5.0);
      --        end if;
      --     end loop;
      --  end;

      --  declare
      --     use Minerva.Encounter.Selections;
      --  begin
      --     for Encounter of
      --       Select_Where (Turn = Athena.Turns.Previous_Turn)
      --     loop
      --        declare
      --           Name  : constant String := Encounter.Star.Name;
      --           Index : constant Positive := Model.Index_Map (Name);
      --        begin
      --           Model.Stars (Index).Encounter := Encounter.Size;
      --        end;
      --     end loop;
      --  end;

      Load_Journeys (Model);

   end Load_Galaxy;

   -------------------
   -- Load_Journeys --
   -------------------

   procedure Load_Journeys
     (Data : Galaxy_Model_Record_Access)
   is null;

      --  procedure Add_Ship
      --    (Ship : Minerva.Ship.Ship_Handle);
      --
      --  procedure Add_Fleet
      --    (Fleet : Minerva.Fleet.Fleet_Handle);
      --
      --  procedure Add_Journey
      --    (From, To : Athena.Real_Arrays.Real_Vector;
      --     Empire   : Minerva.Empire.Empire_Handle;
      --     Progress : Unit_Real;
      --     Size     : Non_Negative_Real);
      --
      --  ---------------
      --  -- Add_Fleet --
      --  ---------------
      --
      --  procedure Add_Fleet
      --    (Fleet : Minerva.Fleet.Fleet_Handle)
      --  is
      --  begin
      --     if Fleet.In_Deep_Space then
      --        Add_Journey
      --          (From     => Fleet.Deep_Space_Position,
      --           To       => Fleet.Destination.Position,
      --           Empire   => Fleet.Owner,
      --           Progress => Fleet.Progress,
      --           Size     => Non_Negative_Real (Fleet.Ship_Count));
      --     end if;
      --  end Add_Fleet;
      --
      --  -----------------
      --  -- Add_Journey --
      --  -----------------
      --
      --  procedure Add_Journey
      --    (From, To : Athena.Real_Arrays.Real_Vector;
      --     Empire   : Minerva.Empire.Empire_Handle;
      --     Progress : Unit_Real;
      --     Size     : Non_Negative_Real)
      --  is
      --     use Nazar;
      --     X1       : constant Real := From (1);
      --     Y1       : constant Real := From (2);
      --     X2       : constant Real := To (1);
      --     Y2       : constant Real := To (2);
      --     Rec      : constant Journey_Record :=
      --                  Journey_Record'
      --                    (Empire   => Empire,
      --                     Size     => Nazar_Float (Size),
      --                     X1       => Nazar_Float (X1),
      --                     Y1       => Nazar_Float (Y1),
      --                     X2       => Nazar_Float (X2),
      --                     Y2       => Nazar_Float (Y2),
      --                     Progress => Nazar_Float (Progress),
      --                     Color    => Empire.Color);
      --  begin
      --     Data.Journeys.Append (Rec);
      --  end Add_Journey;
      --
      --  --------------
      --  -- Add_Ship --
      --  --------------
      --
      --  procedure Add_Ship
      --    (Ship : Minerva.Ship.Ship_Handle)
      --  is
      --  begin
      --     if not Ship.Has_Fleet then
      --        declare
      --           use type Athena.Movers.Mover_Location_Type;
      --           use type Athena.Real_Arrays.Real_Vector;
      --           Origin      : Athena.Movers.Mover_Location;
      --           Destination : Athena.Movers.Mover_Location;
      --           Current     : Athena.Movers.Mover_Location;
      --           Progress    : Unit_Real;
      --        begin
      --           Ship.Get_Journey (Origin, Destination, Current);
      --           if Current.Loc_Type = Athena.Movers.Deep_Space then
      --              Progress :=
      --                abs (Current.Position - Origin.Position)
      --                / abs (Destination.Position - Origin.Position);
      --              Add_Journey
      --                (From     => Origin.Position,
      --                 To       => Destination.Position,
      --                 Empire   => Ship.Owner,
      --                 Progress => Progress,
      --                 Size     => 3.0); --  Ship.Design.Tonnage / 100.0);
      --           end if;
      --
      --        exception
      --           when others =>
      --              if not Ship.Has_Destination then
      --                 Ship.Log ("should have had a destination");
      --
      --                 Ada.Text_IO.Put_Line
      --                   (Ada.Text_IO.Standard_Error,
      --                    Athena.Calendar.Image (Athena.Calendar.Clock)
      --                    & ": "
      --               & Ship.Short_Name & " should have had a destination");
      --              else
      --                 Ada.Text_IO.Put_Line
      --                   (Ada.Text_IO.Standard_Error,
      --                    Athena.Calendar.Image (Athena.Calendar.Clock)
      --                    & ": "
      --                    & Ship.Short_Name & " has unexpected destination: "
      --                    & Ship.Destination_Name);
      --              end if;
      --              raise;
      --        end;
      --     end if;
      --  end Add_Ship;
   --  begin
   --     Data.Journeys.Clear;
   --     Minerva.Ship.Iterate_All (Add_Ship'Access);
   --     Minerva.Fleet.Iterate_All (Add_Fleet'Access);
   --  end Load_Journeys;

   --------------------
   -- Orbiting_Ships --
   --------------------

   function Orbiting_Ships
     (Around : Minerva.Star.Star_Handle)
      return Empire_Ships_Lists.List
   is
      pragma Unreferenced (Around);
   begin
      return Empire_Ships_Lists.Empty_List;

      --  return List : Empire_Ships_Lists.List do
      --     for Ship of Select_Where (Star = Around) loop
      --        if Ship.Alive and then not Ship.Destination.Has_Element then
      --           declare
      --              Found : Boolean := False;
      --           begin
      --              for Item of List loop
      --              if Item.Empire.Identifier = Ship.Empire.Identifier then
      --                    Item.Count := Item.Count + 1;
      --                    Found := True;
      --                    exit;
      --                 end if;
      --              end loop;
      --              if not Found then
      --                 List.Append
      --                   (Empire_Ships_Record'
      --                      (Empire =>
      --                           Minerva.Empire.Get
      --                         (Ship.Empire.Reference_Empire),
      --                       Count  => 1));
      --              end if;
      --           end;
      --        end if;
      --     end loop;
      --  end return;
   end Orbiting_Ships;

   ------------
   -- Reload --
   ------------

   --  overriding procedure Reload
   --    (Model : in out Root_Galaxy_Model)
   --  is
   --  begin
   --     Model.Load_Galaxy;
   --     Model.Draw_Galaxy;
   --     Model.Notify_Observers;
   --  end Reload;

end Athena.UI.Models.Galaxy;
