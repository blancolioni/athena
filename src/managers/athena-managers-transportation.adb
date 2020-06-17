with Athena.Ships;
with Athena.Stars;

with Athena.Ships.Lists;

with Athena.Handles.Design;
with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.Ship.Actions;
with Athena.Handles.Star;

package body Athena.Managers.Transportation is

   type Transportation_Manager is
     new Root_Manager_Type with null record;

   overriding function Identifier
     (Manager : Transportation_Manager)
      return String
   is ("transportation");

   overriding procedure Dispatch_Create_Orders
     (Manager : in out Transportation_Manager);

   type Transport_Message_Type is
     new Message_Type with
      record
         Empire   : Athena.Handles.Empire_Reference;
         From     : Athena.Handles.Star_Reference;
         To       : Athena.Handles.Star_Reference;
         Cargo    : Athena.Cargo.Cargo_Container;
         Priority : Athena.Handles.Order_Priority;
      end record;

   procedure Get_Transport
     (For_Empire : Athena.Handles.Empire.Empire_Handle;
      From_Star  : Athena.Handles.Star.Star_Handle;
      Cargo      : Athena.Cargo.Cargo_Container;
      Ships      : out Athena.Ships.Lists.List);

   --  function Find_Idle_Ship
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle;
   --     From_Star  : Athena.Handles.Star.Star_Handle;
   --     Cargo      : Athena.Cargo.Cargo_Interface'Class)
   --     return Athena.Handles.Ship.Ship_Handle;

   function Cargo_Transport_Design
     (For_Empire : Athena.Handles.Empire.Empire_Handle;
      Cargo      : Athena.Cargo.Cargo_Container)
      return Athena.Handles.Design.Design_Handle
     with Unreferenced;

   ----------------------------
   -- Cargo_Transport_Design --
   ----------------------------

   function Cargo_Transport_Design
     (For_Empire : Athena.Handles.Empire.Empire_Handle;
      Cargo      : Athena.Cargo.Cargo_Container)
      return Athena.Handles.Design.Design_Handle
   is
      Design_Class : constant Athena.Handles.Empire.Standard_Empire_Design :=
                       (if Cargo.Tonnage (Athena.Cargo.People) > 0.0
                        then Athena.Handles.Empire.Transport
                        else Athena.Handles.Empire.Freighter);
   begin
      return Athena.Handles.Design.Get
        (For_Empire.Standard_Design (Design_Class));
   end Cargo_Transport_Design;

   ------------------------------------
   -- Default_Transportation_Manager --
   ------------------------------------

   function Default_Transportation_Manager
     return Root_Manager_Type'Class
   is
   begin
      return Manager : constant Transportation_Manager :=
        Transportation_Manager'
          (Name     => +"transport",
           Empire   => <>,
           Priority => 1050,
           Has_Next_Update => True,
           Next_Update => Athena.Calendar.Clock,
           Messages => <>);
   end Default_Transportation_Manager;

   ----------------------------
   -- Dispatch_Create_Orders --
   ----------------------------

   overriding procedure Dispatch_Create_Orders
     (Manager : in out Transportation_Manager)
   is
      Empire : constant Athena.Handles.Empire.Empire_Handle :=
                 Athena.Handles.Empire.Get (Manager.Empire);
   begin
      while not Manager.Messages.Is_Empty loop
         declare
            Message : constant Transport_Message_Type :=
                        Transport_Message_Type
                          (Manager.Messages.First_Element);
            From    : constant Athena.Handles.Star.Star_Handle :=
                        Athena.Handles.Star.Get (Message.From);

            function More_Convenient
              (Left, Right : Athena.Handles.Ship.Ship_Handle)
               return Boolean;

            ---------------------
            -- More_Convenient --
            ---------------------

            function More_Convenient
              (Left, Right : Athena.Handles.Ship.Ship_Handle)
               return Boolean
            is
               use type Athena.Handles.Star.Star_Handle;
            begin
               if Left.Has_Star_Location then
                  if Left.Star_Location = From then
                     return True;
                  elsif Right.Has_Star_Location then
                     if Right.Star_Location = From then
                        return False;
                     else
                        return Athena.Stars.Distance (Left.Star_Location, From)
                          < Athena.Stars.Distance (Right.Star_Location, From);
                     end if;
                  else
                     return Right.Destination /= From;
                  end if;
               elsif Right.Has_Star_Location then
                  return Right.Star_Location = From
                    or else Left.Destination /= From;
               elsif Left.Destination = From then
                  return True;
               else
                  return False;
               end if;
            end More_Convenient;

            package Ship_Sorting is
              new Athena.Ships.Lists.Generic_Sorting (More_Convenient);

            Ships   : Athena.Ships.Lists.List;
         begin
            Manager.Messages.Delete_First;

            Get_Transport
              (For_Empire => Empire,
               From_Star  => From,
               Cargo      => Message.Cargo,
               Ships      => Ships);

            Ship_Sorting.Sort (Ships);

            declare
               use type Athena.Handles.Star.Star_Handle;
               Container : Athena.Cargo.Cargo_Container :=
                             Message.Cargo;
            begin
               while not Container.Is_Empty
                 and then not Ships.Is_Empty
               loop

                  declare
                     Ship : constant Athena.Handles.Ship.Ship_Handle :=
                              Ships.First_Element;
                  begin
                     Ships.Delete_First;
                     if Ship.Is_Idle then
                        Manager.Log
                          ("using ship: " & Ship.Short_Name
                           & " to transport " & Container.Content_Summary);

                        Container.Remove (Ship);

                        if not Container.Is_Empty
                          and then Ship.Star_Location /= From
                        then
                           Athena.Handles.Ship.Actions.Move_To (Ship, From);
                        end if;

                        declare
                           Cargo : Athena.Cargo.Cargo_Container :=
                                     Container;
                        begin
                           Cargo.Remove_Capacity (Ship);
                           Ship.Add_Action
                             (Athena.Handles.Ship.Actions.Load_Cargo
                                (Cargo));
                           Container.Remove_Cargo (Cargo);
                        end;

                        Athena.Handles.Ship.Actions.Move_To
                          (Ship, Athena.Handles.Star.Get (Message.To));

                        Ship.Add_Action
                          (Athena.Handles.Ship.Actions.Unload_Cargo
                             (Message.Cargo));

                     end if;
                  end;
               end loop;
            end;
         end;
      end loop;
      Manager.Set_Next_Update_Delay (Athena.Calendar.Days (10));
   end Dispatch_Create_Orders;

   --------------------
   -- Find_Idle_Ship --
   --------------------

   --  function Find_Idle_Ship
   --    (For_Empire : Athena.Handles.Empire.Empire_Handle;
   --     From_Star  : Athena.Handles.Star.Star_Handle;
   --     Cargo      : Athena.Cargo.Cargo_Interface'Class)
   --     return Athena.Handles.Ship.Ship_Handle
   --  is
   --     Closest : Athena.Handles.Ship.Ship_Handle :=
   --                 Athena.Handles.Ship.Empty_Handle;
   --     Distance : Non_Negative_Real := Non_Negative_Real'Last;
   --
   --     procedure Check_Available
   --       (Reference : Athena.Handles.Ship_Reference);
   --
   --     ---------------------
   --     -- Check_Available --
   --     ---------------------
   --
   --     procedure Check_Available
   --       (Reference : Athena.Handles.Ship_Reference)
   --     is
   --        Ship : constant Athena.Handles.Ship.Ship_Handle :=
   --                 Athena.Handles.Ship.Get (Reference);
   --     begin
   --        if Ship.Idle
   --          and then Athena.Ships.Cargo_Space (Ship, Cargo.Category) > 0.0
   --        then
   --           declare
   --              D : constant Non_Negative_Real :=
   --                    Athena.Stars.Distance
   --                      (From_Star, Ship.Star_Location);
   --           begin
   --              if D < Distance then
   --                 Closest := Ship;
   --                 Distance := D;
   --              end if;
   --           end;
   --        end if;
   --     end Check_Available;
   --
   --  begin
   --     For_Empire.Iterate_Managed_Ships
   --       (Manager => Athena.Handles.Transport_Manager,
   --        Process => Check_Available'Access);
   --
   --     return Closest;
   --
   --  end Find_Idle_Ship;

   -------------------
   -- Get_Transport --
   -------------------

   procedure Get_Transport
     (For_Empire : Athena.Handles.Empire.Empire_Handle;
      From_Star  : Athena.Handles.Star.Star_Handle;
      Cargo      : Athena.Cargo.Cargo_Container;
      Ships      : out Athena.Ships.Lists.List)
   is
      pragma Unreferenced (From_Star);

      procedure Check_Available
        (Reference : Athena.Handles.Ship_Reference);

      ---------------------
      -- Check_Available --
      ---------------------

      procedure Check_Available
        (Reference : Athena.Handles.Ship_Reference)
      is
         Ship : constant Athena.Handles.Ship.Ship_Handle :=
                  Athena.Handles.Ship.Get (Reference);

         Suitable : Boolean := Ship.Is_Idle;

      begin

         if Suitable then
            for Category in Athena.Cargo.Cargo_Category loop
               if Cargo.Tonnage (Category) > 0.0
                 and then Ship.Cargo_Space (Category) = 0.0
               then
                  Suitable := False;
                  exit;
               end if;
            end loop;
         end if;

         if Suitable then
            Ships.Append (Ship);
         end if;

      end Check_Available;

   begin
      Ships.Clear;

      For_Empire.Iterate_Managed_Ships
        (Manager => Athena.Handles.Transport_Manager,
         Process => Check_Available'Access);
   end Get_Transport;

   -----------------------
   -- Transport_Message --
   -----------------------

   function Transport_Message
     (Empire   : Athena.Handles.Empire_Reference;
      From     : Athena.Handles.Star_Reference;
      To       : Athena.Handles.Star_Reference;
      Cargo    : Athena.Cargo.Cargo_Container;
      Priority : Athena.Handles.Order_Priority)
      return Message_Type'Class
   is
   begin
      return Transport_Message_Type'
        (Empire   => Empire,
         From     => From,
         To       => To,
         Cargo    => Cargo,
         Priority => Priority);
   end Transport_Message;

end Athena.Managers.Transportation;
