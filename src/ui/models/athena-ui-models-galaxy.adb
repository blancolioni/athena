with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Nazar.Colors;
with Nazar.Main;

with Athena.Color;
with Athena.Voronoi_Diagrams;

with Athena.Server;
with Athena.Signals;

with Athena.Empires;

--  with Athena.Knowledge.Stars;

with Athena.Handles.Colony;
with Athena.Handles.Fleet;
with Athena.Handles.Knowledge;
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

   type Galaxy_Model_Record is
      record
         Empire     : Athena.Handles.Empire.Empire_Handle;
         Stars      : Star_Record_Vectors.Vector;
         Index_Map  : Index_Maps.Map;
         Journeys   : Journey_Lists.List;
      end record;

   type Galaxy_Model_Record_Access is access Galaxy_Model_Record;

   procedure Load_Journeys
     (Data : Galaxy_Model_Record_Access);

   type Galaxy_Model_Layer is
     (Background, Boundaries, Stars, Owner, Ships, Journeys);

   package Handler_Id_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Athena.Signals.Handler_Id, Athena.Signals."=");

   type Root_Galaxy_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Data        : Galaxy_Model_Record_Access;
         Layer       : Galaxy_Model_Layer;
         Handler_Ids : Handler_Id_Lists.List;
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

   procedure Connect_Signals
     (Model : not null access Root_Galaxy_Model'Class);

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

   type Model_User_Data is new Athena.Signals.User_Data_Interface with
      record
         Model : Galaxy_Model_Access;
      end record;

   type Colony_Owner_Changed_Handler is
     new Athena.Signals.Signal_Handler_Interface with
      record
         null;
      end record;

   overriding function Handle
     (Handler     : Colony_Owner_Changed_Handler;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class)
      return Boolean;

   type Visited_Handler is
     new Athena.Signals.Signal_Handler_Interface with
      record
         null;
      end record;

   overriding function Handle
     (Handler     : Visited_Handler;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class)
      return Boolean;

   type Clock_Tick_Handler is
     new Athena.Signals.Signal_Handler_Interface with
      record
         null;
      end record;

   overriding function Handle
     (Handler     : Clock_Tick_Handler;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class)
      return Boolean;

   ---------------------
   -- Connect_Signals --
   ---------------------

   procedure Connect_Signals
     (Model : not null access Root_Galaxy_Model'Class)
   is
   begin
      case Model.Layer is
         when Background =>
            null;

         when Boundaries =>
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Handles.Colony.Colony_Owner_Changed,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Galaxy_Model_Access (Model)),
                  Handler   => Colony_Owner_Changed_Handler'(null record)));
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Handles.Knowledge.Visited,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Galaxy_Model_Access (Model)),
                  Handler   => Visited_Handler'(null record)));

         when Stars =>
            null;

         when Owner =>
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Handles.Colony.Colony_Owner_Changed,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Galaxy_Model_Access (Model)),
                  Handler   => Colony_Owner_Changed_Handler'(null record)));

         when Ships =>
            null;

         when Journeys =>
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Signals.Clock_Tick,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Galaxy_Model_Access (Model)),
                  Handler   => Clock_Tick_Handler'(null record)));

      end case;
   end Connect_Signals;

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
               if Rec.Handle.Has_Owner
                 or else Model.Data.Empire.Knowledge.Visited
                   (Rec.Handle)
               then
                  declare
                     Owner : constant Athena.Handles.Empire.Empire_Handle :=
                               Athena.Handles.Empire.Get (Rec.Handle.Owner);
                  begin
                     Model.Save_State;
                     Model.Set_Fill (True);
                     if Rec.Handle.Has_Owner then
                        Model.Set_Color (To_Nazar_Color (Owner.Color));
                     else
                        declare
                           Color : Athena.Color.Athena_Color :=
                                     Model.Data.Empire.Color;
                        begin
                           Color.Alpha := 0.5;
                           Model.Set_Color (To_Nazar_Color (Color));
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
               if Rec.Handle.Has_Owner then
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

         when Journeys =>

            for Journey of Model.Data.Journeys loop
               Model.Save_State;
               Model.Set_Fill (True);
               Model.Set_Color ((0.8, 0.8, 0.8, 1.0));
               --  To_Nazar_Color (Journey.Ship.Empire.Rgb));

               declare
                  use Nazar;
                  X : constant Nazar_Float :=
                        Journey.X1
                          + Journey.Progress * (Journey.X2 - Journey.X1);
                  Y : constant Nazar_Float :=
                        Journey.Y1
                          + Journey.Progress * (Journey.Y2 - Journey.Y1);
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

            end loop;
         end case;
      end Do_Draw;

   begin
      Nazar.Main.With_Render_Lock (Do_Draw'Access);
   end Draw_Galaxy;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Draw_Model_Layers
   is
      Count  : constant Positive :=
                 Galaxy_Model_Layer'Pos (Galaxy_Model_Layer'Last) + 1;
      Layers : Draw_Model_Layers (1 .. Count);
      Index  : Natural := 0;

      Centre_Star : constant Athena.Handles.Star.Star_Handle :=
                      Athena.Empires.Capital (Empire);
      Left        : constant Real := Centre_Star.X - 40.0;
      Top         : constant Real := Centre_Star.Y - 40.0;
      Right       : constant Real := Centre_Star.X + 40.0;
      Bottom      : constant Real := Centre_Star.Y + 40.0;

      Data   : constant Galaxy_Model_Record_Access :=
                 new Galaxy_Model_Record'
                   (Empire    => Empire,
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
              Layer       => Layer,
              Handler_Ids => <>);
         Connect_Signals (Galaxy_Model_Access (Layers (Index)));
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

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Colony_Owner_Changed_Handler;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class)
      return Boolean
   is
      Colony : constant Athena.Handles.Colony.Colony_Handle :=
                 Athena.Handles.Colony.Colony_Handle (Source);
      Model  : constant Galaxy_Model_Access :=
                 Model_User_Data (User_Data).Model;
   begin
      Model.Data.Stars (Model.Data.Index_Map (Colony.Star.Name)).Color :=
        To_Nazar_Color (Colony.Owner.Color);
      Model.Draw_Galaxy;
      Model.Queue_Render;
      Model.Notify_Observers;
      return False;
   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Visited_Handler;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class)
      return Boolean
   is
      Model  : constant Galaxy_Model_Access :=
                 Model_User_Data (User_Data).Model;
   begin
      Model.Draw_Galaxy;
      Model.Queue_Render;
      Model.Notify_Observers;
      return False;
   end Handle;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Clock_Tick_Handler;
      Source      : Athena.Signals.Signal_Source_Interface'Class;
      Signal_Data : Athena.Signals.Signal_Data_Interface'Class;
      User_Data   : Athena.Signals.User_Data_Interface'Class)
      return Boolean
   is
      Model  : constant Galaxy_Model_Access :=
                 Model_User_Data (User_Data).Model;
   begin
      Load_Journeys (Model.Data);
      Model.Draw_Galaxy;
      Model.Queue_Render;
      Model.Notify_Observers;
      return False;
   end Handle;

   -----------------
   -- Load_Galaxy --
   -----------------

   procedure Load_Galaxy
     (Model : Galaxy_Model_Record_Access)
   is
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
               if Model.Index_Map.Contains (Star.Name) then
                  raise Constraint_Error with
                    "multiple systems called " & Star.Name;
               end if;

               Model.Index_Map.Insert
                 (Star.Name, Model.Stars.Last_Index);
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

      Load_Journeys (Model);

   end Load_Galaxy;

   -------------------
   -- Load_Journeys --
   -------------------

   procedure Load_Journeys
     (Data : Galaxy_Model_Record_Access)
   is

      procedure Add_Ship
        (Ship : Athena.Handles.Ship.Ship_Handle);

      procedure Add_Fleet
        (Fleet : Athena.Handles.Fleet.Fleet_Handle);

      procedure Add_Journey
        (From, To : Athena.Handles.Star.Star_Handle;
         Empire   : Athena.Handles.Empire.Empire_Handle;
         Progress : Unit_Real;
         Size     : Non_Negative_Real);

      ---------------
      -- Add_Fleet --
      ---------------

      procedure Add_Fleet
        (Fleet : Athena.Handles.Fleet.Fleet_Handle)
      is
      begin
         if Fleet.Is_Jumping then
            Add_Journey
              (From     => Fleet.Location,
               To       => Fleet.Destination,
               Empire   => Fleet.Owner,
               Progress => Fleet.Progress,
               Size     => Non_Negative_Real (Fleet.Ship_Count));
         end if;
      end Add_Fleet;

      -----------------
      -- Add_Journey --
      -----------------

      procedure Add_Journey
        (From, To : Athena.Handles.Star.Star_Handle;
         Empire   : Athena.Handles.Empire.Empire_Handle;
         Progress : Unit_Real;
         Size     : Non_Negative_Real)
      is
         use Nazar;
         X1       : constant Real := From.X;
         Y1       : constant Real := From.Y;
         X2       : constant Real := To.X;
         Y2       : constant Real := To.Y;
         Rec      : constant Journey_Record :=
                      Journey_Record'
                        (Empire   => Empire,
                         Size     => Nazar_Float (Size),
                         X1       => Nazar_Float (X1),
                         Y1       => Nazar_Float (Y1),
                         X2       => Nazar_Float (X2),
                         Y2       => Nazar_Float (Y2),
                         Progress => Nazar_Float (Progress),
                         Color    => Empire.Color);
      begin
         Data.Journeys.Append (Rec);
      end Add_Journey;

      --------------
      -- Add_Ship --
      --------------

      procedure Add_Ship
        (Ship : Athena.Handles.Ship.Ship_Handle)
      is
      begin
         if not Ship.Has_Fleet and then Ship.Is_Jumping then
            Add_Journey
              (From     => Ship.Origin,
               To       => Ship.Destination,
               Empire   => Ship.Owner,
               Progress => Ship.Progress,
               Size     => 3.0); --  Ship.Design.Tonnage / 100.0);
         end if;
      end Add_Ship;
   begin
      Data.Journeys.Clear;
      Athena.Handles.Ship.Iterate_All (Add_Ship'Access);
      Athena.Handles.Fleet.Iterate_All (Add_Fleet'Access);
   end Load_Journeys;

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

   --  overriding procedure Reload
   --    (Model : in out Root_Galaxy_Model)
   --  is
   --  begin
   --     Model.Load_Galaxy;
   --     Model.Draw_Galaxy;
   --     Model.Notify_Observers;
   --  end Reload;

end Athena.UI.Models.Galaxy;
