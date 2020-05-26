with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Nazar.Colors;

with Athena.Color;
with Athena.Voronoi_Diagrams;

with Athena.Empires;

--  with Athena.Knowledge.Stars;

with Athena.Handles.Ship;
with Athena.Handles.Star;

package body Athena.UI.Models.Galaxy is

   type Boundary_Point is
      record
         X, Y : Real;
      end record;

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Boundary_Point);

   type Empire_Ships_Record is
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
         Count  : Natural;
      end record;

   package Empire_Ships_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Empire_Ships_Record);

   type Star_Record is
      record
         Handle    : Athena.Handles.Star.Star_Handle;
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
         Empire   : Athena.Handles.Empire.Empire_Handle;
         Size     : Nazar.Nazar_Float;
         X1, Y1   : Nazar.Nazar_Float;
         X2, Y2   : Nazar.Nazar_Float;
         Progress : Nazar.Nazar_Float;
         Color    : Athena.Color.Athena_Color;
      end record;

   package Journey_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Journey_Record);

   package Index_Maps is new WL.String_Maps (Positive);

   type Root_Galaxy_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Empire     : Athena.Handles.Empire.Empire_Handle;
         Stars      : Star_Record_Vectors.Vector;
         Index_Map  : Index_Maps.Map;
         Journeys   : Journey_Lists.List;
      end record;

   type Galaxy_Model_Access is access all Root_Galaxy_Model'Class;

   overriding function Background_Color
     (View : Root_Galaxy_Model)
      return Nazar.Colors.Nazar_Color
   is (0.0, 0.0, 0.0, 1.0);

   overriding procedure Reload
     (Model : in out Root_Galaxy_Model);

   procedure Load_Galaxy
     (Model : in out Root_Galaxy_Model'Class);

   procedure Draw_Galaxy
     (Model : in out Root_Galaxy_Model'Class);

   function Orbiting_Ships
     (Around : Athena.Handles.Star.Star_Handle)
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

   -----------------
   -- Draw_Galaxy --
   -----------------

   procedure Draw_Galaxy
     (Model : in out Root_Galaxy_Model'Class)
   is
   begin

      Model.Clear;
      Model.Save_State;

      for Rec of Model.Stars loop
         if Rec.Handle.Has_Owner then
            declare
               Owner : constant Athena.Handles.Empire.Empire_Handle :=
                         Athena.Handles.Empire.Get (Rec.Handle.Owner);
            begin
               Model.Save_State;
               Model.Set_Fill (True);
               Model.Set_Color (To_Nazar_Color (Owner.Color));
            end;

            declare
               use Nazar;
               First : Boolean := True;
            begin
               for Pt of Rec.Boundary loop
                  if First then
                     Model.Move_To (Nazar_Float (Pt.X), Nazar_Float (Pt.Y));
                     First := False;
                  else
                     Model.Line_To (Nazar_Float (Pt.X), Nazar_Float (Pt.Y));
                  end if;
               end loop;

               Model.Render;
            end;

            Model.Restore_State;

         end if;
      end loop;

      for Rec of Model.Stars loop

         if Rec.Encounter > 0 then
            Model.Save_State;
            Model.Set_Fill (True);
            Model.Set_Color (1.0, 0.31, 0.0, 1.0);
            Model.Move_To (Rec.X, Rec.Y);
            Model.Circle (Nazar.Nazar_Float (Rec.Encounter / 5 + 10));
            Model.Render;
            Model.Restore_State;
         end if;

         Model.Save_State;
         Model.Set_Fill (True);
         Model.Move_To (Rec.X, Rec.Y);
         Model.Set_Color (Rec.Color);
         Model.Circle (2.0);
         Model.Render;
         Model.Restore_State;

         if Rec.Handle.Has_Owner then
            declare
               --  use type Nazar.Nazar_Float;
            begin
               Model.Save_State;
               Model.Set_Color (1.0, 1.0, 1.0, 1.0);
               Model.Set_Font ("Courier", 12.0, False, False);
               Model.Move_To (Rec.X, Rec.Y);
               Model.Draw_Text (Rec.Handle.Name);
               Model.Render;
               Model.Restore_State;
            end;
         end if;

         if not Rec.Ships.Is_Empty then
            declare
               use type Nazar.Nazar_Float;
               use type Athena.Handles.Empire_Reference;
               Radius : Nazar.Nazar_Float := 5.0;
            begin
               Model.Save_State;
               Model.Set_Fill (False);
               for Empire_Ship of Rec.Ships loop
                  if Rec.Handle.Has_Owner
                    and then Rec.Handle.Owner
                      = Empire_Ship.Empire.Reference
                  then
                     Model.Set_Color ((1.0, 1.0, 1.0, 1.0));
                  else
                     Model.Set_Color
                       (To_Nazar_Color (Empire_Ship.Empire.Color));
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

      for Journey of Model.Journeys loop
         Model.Save_State;
         Model.Set_Fill (True);
         Model.Set_Color ((0.8, 0.8, 0.8, 1.0));
         --  To_Nazar_Color (Journey.Ship.Empire.Rgb));

         declare
            use Nazar;
            X : constant Nazar_Float :=
                  Journey.X1 + Journey.Progress * (Journey.X2 - Journey.X1);
            Y : constant Nazar_Float :=
                  Journey.Y1 + Journey.Progress * (Journey.Y2 - Journey.Y1);
         begin
            Model.Move_To (X, Y);
            Model.Set_Fill (True);
            Model.Set_Color (To_Nazar_Color (Journey.Color));
            Model.Circle (3.0);
            Model.Set_Fill (False);
            Model.Set_Color ((0.8, 0.8, 0.8, 1.0));
            Model.Circle (3.0);
            Model.Render;
         end;

      end loop;

      Model.Restore_State;

   end Draw_Galaxy;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Nazar.Models.Draw.Nazar_Draw_Model
   is
      Model         : constant Galaxy_Model_Access :=
                        new Root_Galaxy_Model;
   begin

      Model.Empire := Empire;

      Ada.Text_IO.Put ("Creating galaxy view ...");
      Ada.Text_IO.Flush;

      Model.Load_Galaxy;
      Model.Draw_Galaxy;

      Ada.Text_IO.Put_Line ("done");

      return Nazar.Models.Draw.Nazar_Draw_Model (Model);

   end Galaxy_Model;

   -----------------
   -- Load_Galaxy --
   -----------------

   procedure Load_Galaxy
     (Model : in out Root_Galaxy_Model'Class)
   is
      Centre_Star : constant Athena.Handles.Star.Star_Handle :=
                      Athena.Empires.Capital (Model.Empire);
      Left        : Real := Centre_Star.X - 40.0;
      Top         : Real := Centre_Star.Y - 40.0;
      Right       : Real := Centre_Star.X + 40.0;
      Bottom      : Real := Centre_Star.Y + 40.0;
      Voronoi     : Athena.Voronoi_Diagrams.Voronoi_Diagram;
   begin

      if Model.Stars.Is_Empty then
         declare
            procedure Load_Star (Star : Athena.Handles.Star.Star_Handle);

            procedure Load_Star (Star : Athena.Handles.Star.Star_Handle) is
               Color : constant Nazar.Colors.Nazar_Color :=
                         (1.0, 1.0, 1.0, 1.0);
               Rec   : constant Star_Record := Star_Record'
                 (Handle    => Star,
                  Color     => Color,
                  X         => Nazar.Nazar_Float (Star.X),
                  Y         => Nazar.Nazar_Float (Star.Y),
                  Encounter => 0,
                  Ships     => <>,
                  Boundary  => <>);
            begin
               Model.Stars.Append (Rec);
               Voronoi.Add_Point (Star.X, Star.Y);
               if False then
                  Left := Real'Min (Left, Star.X);
                  Top  := Real'Min (Top, Star.Y);
                  Right := Real'Max (Right, Star.X);
                  Bottom  := Real'Max (Bottom, Star.Y);
               end if;
               if Model.Index_Map.Contains (Star.Name) then
                  raise Constraint_Error with
                    "multiple systems called " & Star.Name;
               end if;

               Model.Index_Map.Insert (Star.Name, Model.Stars.Last_Index);
            end Load_Star;

         begin
            Athena.Handles.Star.Iterate_Stars (Load_Star'Access);
         end;

         Voronoi.Generate;

         for I in 1 .. Voronoi.Polygon_Count loop
            declare
               Pts : Point_Vectors.Vector;
            begin
               for J in 1 .. Voronoi.Vertex_Count (I) loop
                  Pts.Append ((Voronoi.Vertex_X (I, J),
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
      --     use Athena.Handles.Encounter.Selections;
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

      Model.Journeys.Clear;

      declare
         procedure Add_Ship
           (Ship : Athena.Handles.Ship.Ship_Handle);

         --------------
         -- Add_Ship --
         --------------

         procedure Add_Ship
           (Ship : Athena.Handles.Ship.Ship_Handle)
         is
         begin
            if Ship.Has_Deep_Space_Location then
               declare
                  use Nazar;
                  From : constant Athena.Handles.Star.Star_Handle :=
                           Ship.Origin;
                  To   : constant Athena.Handles.Star.Star_Handle :=
                           Ship.Destination;
                  X1   : constant Real := From.X;
                  Y1   : constant Real := From.Y;
                  X2   : constant Real := To.X;
                  Y2   : constant Real := To.Y;
                  Progress : constant Unit_Real := Ship.Progress;
                  Rec      : constant Journey_Record :=
                               Journey_Record'
                                 (Empire   => Ship.Owner,
                                  Size     =>
                                    Nazar.Nazar_Float
                                      (Ship.Design.Tonnage / 100.0),
                                  X1       => Nazar_Float (X1),
                                  Y1       => Nazar_Float (Y1),
                                  X2       => Nazar_Float (X2),
                                  Y2       => Nazar_Float (Y2),
                                  Progress => Nazar_Float (Progress),
                                  Color    => Ship.Owner.Color);
               begin
                  Model.Journeys.Append (Rec);
               end;
            end if;
         end Add_Ship;
      begin

         Athena.Handles.Ship.Iterate_All (Add_Ship'Access);

      end;

      Model.Set_Bounding_Box
        (Box => Nazar.Rectangle'
           (X => Nazar.Nazar_Float (Left),
            Y => Nazar.Nazar_Float (Top),
            W => Nazar.Nazar_Float (Right - Left),
            H => Nazar.Nazar_Float (Bottom - Top)));

   end Load_Galaxy;

   --------------------
   -- Orbiting_Ships --
   --------------------

   function Orbiting_Ships
     (Around : Athena.Handles.Star.Star_Handle)
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
      --                           Athena.Handles.Empire.Get
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

   overriding procedure Reload
     (Model : in out Root_Galaxy_Model)
   is
   begin
      Model.Load_Galaxy;
      Model.Draw_Galaxy;
      Model.Notify_Observers;
   end Reload;

end Athena.UI.Models.Galaxy;
