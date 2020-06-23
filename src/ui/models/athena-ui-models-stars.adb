with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Nazar.Colors;
with Nazar.Main;

with Athena.Color;
with Athena.Elementary_Functions;
with Athena.Real_Arrays;
with Athena.Solar_System;

with Athena.Server;
with Athena.Signals;

with Athena.Movers;

with Athena.Handles.Colony;
with Athena.Handles.Fleet;
with Athena.Handles.Knowledge;
with Athena.Handles.Ship;
with Athena.Handles.World;

package body Athena.UI.Models.Stars is

   type Empire_Ships_Record is
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
         Count  : Natural;
      end record;

   package Empire_Ships_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Empire_Ships_Record);

   type World_Record is
      record
         Handle    : Athena.Handles.World.World_Handle;
         Color     : Nazar.Colors.Nazar_Color;
         X, Y      : Nazar.Nazar_Float;
         Radius    : Non_Negative_Real;
         Ships     : Empire_Ships_Lists.List;
         Encounter : Natural;
      end record;

   package World_Record_Vectors is
     new Ada.Containers.Vectors (Positive, World_Record);

   type Journey_Record is
      record
         Empire   : Athena.Handles.Empire.Empire_Handle;
         Size     : Nazar.Nazar_Float;
         X, Y     : Nazar.Nazar_Float;
         Color    : Athena.Color.Athena_Color;
      end record;

   package Journey_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Journey_Record);

   package Index_Maps is new WL.String_Maps (Positive);

   type Star_Model_Record is
      record
         Empire     : Athena.Handles.Empire.Empire_Handle;
         Star       : Athena.Handles.Star.Star_Handle;
         Worlds     : World_Record_Vectors.Vector;
         Index_Map  : Index_Maps.Map;
         Journeys   : Journey_Lists.List;
      end record;

   type Star_Model_Record_Access is access Star_Model_Record;

   procedure Load_Journeys
     (Data : Star_Model_Record_Access);

   type Star_Model_Layer is
     (Background, Star, Worlds, Owner, Ships, Journeys);

   package Handler_Id_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Athena.Signals.Handler_Id, Athena.Signals."=");

   type Root_Star_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Data        : Star_Model_Record_Access;
         Layer       : Star_Model_Layer;
         Handler_Ids : Handler_Id_Lists.List;
      end record;

   type Star_Model_Access is access all Root_Star_Model'Class;

   overriding function Background_Color
     (View : Root_Star_Model)
      return Nazar.Colors.Nazar_Color
   is (0.0, 0.0, 0.0, 1.0);

   procedure Load_Star
     (Model : Star_Model_Record_Access);

   procedure Draw_System
     (Model : in out Root_Star_Model'Class);

   procedure Connect_Signals
     (Model : not null access Root_Star_Model'Class);

   function Orbiting_Ships
     (Around : Athena.Handles.World.World_Handle)
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
         Model : Star_Model_Access;
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
     (Model : not null access Root_Star_Model'Class)
   is
   begin
      case Model.Layer is
         when Background =>
            null;

         when Star =>
            null;

         when Worlds =>
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Handles.Colony.Colony_Owner_Changed,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Star_Model_Access (Model)),
                  Handler   => Colony_Owner_Changed_Handler'(null record)));
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Handles.Knowledge.Visited,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Star_Model_Access (Model)),
                  Handler   => Visited_Handler'(null record)));
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Signals.Clock_Tick,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Star_Model_Access (Model)),
                  Handler   => Clock_Tick_Handler'(null record)));

         when Owner =>
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Handles.Colony.Colony_Owner_Changed,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Star_Model_Access (Model)),
                  Handler   => Colony_Owner_Changed_Handler'(null record)));

         when Ships =>
            null;

         when Journeys =>
            Model.Handler_Ids.Append
              (Athena.Server.Add_Handler
                 (Signal    => Athena.Signals.Clock_Tick,
                  Source    => Athena.Signals.Any_Source,
                  User_Data =>
                     Model_User_Data'(Model => Star_Model_Access (Model)),
                  Handler   => Clock_Tick_Handler'(null record)));

      end case;
   end Connect_Signals;

   -----------------
   -- Draw_System --
   -----------------

   procedure Draw_System
     (Model : in out Root_Star_Model'Class)
   is

      procedure Do_Draw;

      -------------
      -- Do_Draw --
      -------------

      procedure Do_Draw is
      begin

         Model.Clear;
         Model.Save_State;

         case Model.Layer is
            when Background =>

               null;

            when Star =>
               Model.Save_State;
               Model.Set_Fill (True);
               Model.Move_To (0.0, 0.0);
               Model.Set_Color (To_Nazar_Color (Model.Data.Star.Color));
               Model.Circle (0.1);

            when Worlds =>

               for Rec of Model.Data.Worlds loop
                  declare
                     Position : constant Athena.Real_Arrays.Real_Vector :=
                                  Rec.Handle.Current_Global_Position;
                  begin
                     Rec.X :=
                       Nazar.Nazar_Float (Position (1) * 10.0
                                          / Athena.Solar_System.Earth_Orbit);
                     Rec.Y :=
                       Nazar.Nazar_Float (Position (2) * 10.0
                                          / Athena.Solar_System.Earth_Orbit);
                  end;

                  Model.Save_State;
                  Model.Set_Fill (True);
                  Model.Move_To (Rec.X, Rec.Y);
                  Model.Set_Color (Rec.Color);
                  Model.Circle (Nazar.Nazar_Float (8.0 * Rec.Radius));
                  Model.Render;
                  Model.Restore_State;
               end loop;

            when Owner =>

               for Rec of Model.Data.Worlds loop
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

               for Rec of Model.Data.Worlds loop
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
                           Journey.X * 10.0 / Athena.Solar_System.Earth_Orbit;
                     Y : constant Nazar_Float :=
                           Journey.Y * 10.0 / Athena.Solar_System.Earth_Orbit;
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
   end Draw_System;

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
      Model  : constant Star_Model_Access :=
                 Model_User_Data (User_Data).Model;
      Key    : constant String := Colony.World.Name;
   begin
      if Model.Data.Index_Map.Contains (Key) then
         Model.Data.Worlds (Model.Data.Index_Map (Key)).Color :=
           To_Nazar_Color (Colony.Owner.Color);
         Model.Draw_System;
         Model.Queue_Render;
         Model.Notify_Observers;
      end if;
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
      Model  : constant Star_Model_Access :=
                 Model_User_Data (User_Data).Model;
   begin
      if False then
         Model.Draw_System;
         Model.Queue_Render;
         Model.Notify_Observers;
      end if;
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
      Model  : constant Star_Model_Access :=
                 Model_User_Data (User_Data).Model;
   begin
      Load_Journeys (Model.Data);
      Model.Draw_System;
      Model.Queue_Render;
      Model.Notify_Observers;
      return False;
   end Handle;

   -------------------
   -- Load_Journeys --
   -------------------

   procedure Load_Journeys
     (Data : Star_Model_Record_Access)
   is

      procedure Add_Ship
        (Ship : Athena.Handles.Ship.Ship_Handle);

      procedure Add_Fleet
        (Fleet : Athena.Handles.Fleet.Fleet_Handle);

      procedure Add_Journey
        (Empire   : Athena.Handles.Empire.Empire_Handle;
         Position : Athena.Real_Arrays.Real_Vector;
         Size     : Non_Negative_Real);

      ---------------
      -- Add_Fleet --
      ---------------

      procedure Add_Fleet
        (Fleet : Athena.Handles.Fleet.Fleet_Handle)
      is
      begin
         if Fleet.In_System_Space
           and then Fleet.At_Star (Data.Star)
         then
            Add_Journey
              (Empire   => Fleet.Owner,
               Position => Fleet.Current_Location.System_Position,
               Size     => Non_Negative_Real (Fleet.Ship_Count));
         end if;
      end Add_Fleet;

      -----------------
      -- Add_Journey --
      -----------------

      procedure Add_Journey
        (Empire   : Athena.Handles.Empire.Empire_Handle;
         Position : Athena.Real_Arrays.Real_Vector;
         Size     : Non_Negative_Real)
      is
         use Nazar;
         Rec      : constant Journey_Record :=
                      Journey_Record'
                        (Empire   => Empire,
                         Size     => Nazar_Float (Size),
                         X        => Nazar_Float (Position (1)),
                         Y        => Nazar_Float (Position (2)),
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
         if not Ship.Has_Fleet
           and then Ship.In_System_Space
           and then Ship.At_Star (Data.Star)
         then
            Ship.Log ("added to ui " & Data.Star.Name);
            Add_Journey
              (Empire   => Ship.Owner,
               Position => Ship.Current_Location.System_Position,
               Size     => 3.0); --  Ship.Design.Tonnage / 100.0);
         end if;
      end Add_Ship;

   begin
      Data.Journeys.Clear;
      Athena.Handles.Ship.Iterate_All (Add_Ship'Access);
      Athena.Handles.Fleet.Iterate_All (Add_Fleet'Access);
   end Load_Journeys;

   -----------------
   -- Load_Star --
   -----------------

   procedure Load_Star
     (Model : Star_Model_Record_Access)
   is
   begin

      if Model.Worlds.Is_Empty then
         declare
            procedure Load_World (Reference : Athena.Handles.World_Reference);

            ----------------
            -- Load_World --
            ----------------

            procedure Load_World
              (Reference : Athena.Handles.World_Reference)
            is
               World : constant Athena.Handles.World.World_Handle :=
                         Athena.Handles.World.Get (Reference);
               Color : constant Nazar.Colors.Nazar_Color :=
                         (1.0, 1.0, 1.0, 1.0);
               Position : constant Athena.Real_Arrays.Real_Vector :=
                            World.Current_Global_Position;
               Rec   : constant World_Record := World_Record'
                 (Handle    => World,
                  Color     => Color,
                  X         => Nazar.Nazar_Float (Position (1)),
                  Y         => Nazar.Nazar_Float (Position (2)),
                  Radius    =>
                    Athena.Elementary_Functions.Sqrt
                      (World.Radius / Athena.Solar_System.Earth_Radius),
                  Encounter => 0,
                  Ships     => <>);
            begin
               Model.Worlds.Append (Rec);
               if Model.Index_Map.Contains (World.Name) then
                  raise Constraint_Error with
                    "multiple systems called " & World.Name;
               end if;

               Model.Index_Map.Insert
                 (World.Name, Model.Worlds.Last_Index);
            end Load_World;

         begin
            Model.Star.Iterate_Worlds (Load_World'Access);
         end;

      end if;

      --  declare
      --     Knowledge : Athena.Knowledge.Worlds.World_Knowledge;
      --  begin
      --     Knowledge.Load (Model.Empire);
      --
      --     for Rec of Model.Worlds loop
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
      --           Name  : constant String := Encounter.World.Name;
      --           Index : constant Positive := Model.Index_Map (Name);
      --        begin
      --           Model.Worlds (Index).Encounter := Encounter.Size;
      --        end;
      --     end loop;
      --  end;

      Load_Journeys (Model);

   end Load_Star;

   --------------------
   -- Orbiting_Ships --
   --------------------

   function Orbiting_Ships
     (Around : Athena.Handles.World.World_Handle)
      return Empire_Ships_Lists.List
   is
      pragma Unreferenced (Around);
   begin
      return Empire_Ships_Lists.Empty_List;

      --  return List : Empire_Ships_Lists.List do
      --     for Ship of Select_Where (World = Around) loop
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
   --    (Model : in out Root_Star_Model)
   --  is
   --  begin
   --     Model.Load_Star;
   --     Model.Draw_Star;
   --     Model.Notify_Observers;
   --  end Reload;

   -----------------------
   -- Star_System_Model --
   -----------------------

   function Star_System_Model
     (Empire : Athena.Handles.Empire.Empire_Handle;
      Star   : Athena.Handles.Star.Star_Handle)
      return Draw_Model_Layers
   is
      Count  : constant Positive :=
                 Star_Model_Layer'Pos (Star_Model_Layer'Last) + 1;
      Layers : Draw_Model_Layers (1 .. Count);
      Index  : Natural := 0;

      Left        : constant Real := -40.0;
      Top         : constant Real := -40.0;
      Right       : constant Real := 40.0;
      Bottom      : constant Real := 40.0;

      Data   : constant Star_Model_Record_Access :=
                 new Star_Model_Record'
                   (Empire     => Empire,
                    Star       => Star,
                    Worlds     => <>,
                    Index_Map  => <>,
                    Journeys   => <>);
   begin

      Ada.Text_IO.Put ("Creating star view ...");
      Ada.Text_IO.Flush;

      Load_Star (Data);

      for Layer in Star_Model_Layer loop
         Index := Index + 1;
         Layers (Index) :=
           new Root_Star_Model'
             (Nazar.Models.Draw.Root_Draw_Model with
              Data        => Data,
              Layer       => Layer,
              Handler_Ids => <>);
         Connect_Signals (Star_Model_Access (Layers (Index)));
         Layers (Index).Set_Bounding_Box
           (Box => Nazar.Rectangle'
              (X => Nazar.Nazar_Float (Left),
               Y => Nazar.Nazar_Float (Top),
               W => Nazar.Nazar_Float (Right - Left),
               H => Nazar.Nazar_Float (Bottom - Top)));

         Star_Model_Access (Layers (Index)).Draw_System;
      end loop;

      Ada.Text_IO.Put_Line ("done");

      return Layers;

   end Star_System_Model;

end Athena.UI.Models.Stars;
