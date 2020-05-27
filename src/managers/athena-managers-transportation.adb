with Athena.Ships;

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

   overriding procedure Create_Orders
     (Manager : in out Transportation_Manager);

   type Transport_Message_Type is
     new Message_Type with
      record
         Empire   : Athena.Handles.Empire_Reference;
         From     : Athena.Handles.Star_Reference;
         To       : Athena.Handles.Star_Reference;
         Cargo    : Athena.Handles.Cargo_Class;
         Quantity : Non_Negative_Real;
         Priority : Athena.Handles.Order_Priority;
      end record;

   procedure Get_Transport
     (For_Empire : Athena.Handles.Empire.Empire_Handle;
      From_Star  : Athena.Handles.Star.Star_Handle;
      Capacity   : Non_Negative_Real;
      Ships      : out Athena.Ships.Lists.List;
      Remaining  : out Non_Negative_Real);

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
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
            Ships   : Athena.Ships.Lists.List;
            Missing : Non_Negative_Real;
         begin
            Manager.Messages.Delete_First;
            Get_Transport
              (For_Empire => Empire,
               From_Star  => From,
               Capacity   => Message.Quantity,
               Ships      => Ships,
               Remaining  => Missing);

            if Missing > 0.0 then
               declare
                  Transport : constant Athena.Handles.Design.Design_Handle :=
                                Athena.Handles.Design.Get
                                  (Empire.Standard_Design
                                     (Athena.Handles.Empire.Transport));
                  Cargo     : constant Non_Negative_Real :=
                                Transport.Cargo_Space;
                  Required  : constant Natural :=
                                Natural (Missing / Cargo);
               begin
                  if Required > 0 then
                     Manager.Log
                       ("missing capacity: " & Image (Missing)
                        & "; transport cargo: " & Image (Cargo)
                        & "; required transports:" & Required'Image);

                     --  Athena.Orders.Build_Ships
                     --    (Empire   => For_Empire,
                     --     Design   => Transport,
                     --     Count    => Required,
                     --     Fleet    => Athena.Handles.Fleet.Empty_Handle,
                     --     Manager  => Manager,
                     --     Send_To  =>
                     --       Athena.Handles.Star.Get (Order.From),
                     --     Priority => Manager.Priority);
                  end if;
               end;
            end if;

            declare
               use type Athena.Handles.Star.Star_Handle;
               Remaining : Non_Negative_Real := Message.Quantity;
            begin
               for Ship of Ships loop
                  if Ship.Idle then
                     declare
                        Available : constant Non_Negative_Real :=
                                      Athena.Ships.Available_Cargo_Space
                                        (Ship);

                        Loaded : constant Non_Negative_Real :=
                                      Real'Min (Remaining, Available);
                        Have   : constant Non_Negative_Real :=
                                   Athena.Ships.Current_Cargo
                                     (Ship, Message.Cargo);
                     begin
                        if Have < Loaded then
                           if Ship.Star_Location /= From then
                              Ship.Add_Action
                                (Athena.Handles.Ship.Actions.Move_To
                                   (From));
                           end if;

                           Ship.Add_Action
                             (Athena.Handles.Ship.Actions.Load_Cargo
                                (Message.Cargo, Loaded - Have));
                        end if;

                        Remaining := Remaining - Loaded;

                        Ship.Add_Action
                          (Athena.Handles.Ship.Actions.Move_To
                             (Athena.Handles.Star.Get (Message.To)));
                        Ship.Add_Action
                          (Athena.Handles.Ship.Actions.Unload_Cargo
                             (Message.Cargo, Loaded - Have));
                        Ship.Add_Action
                          (Athena.Handles.Ship.Actions.Move_To
                             (From));
                     end;
                  end if;
               end loop;
            end;
         end;
      end loop;
      Manager.Set_Next_Update_Delay (Athena.Calendar.Days (10));
   end Create_Orders;

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
           Next_Update => Athena.Calendar.Clock,
           Messages => <>);
   end Default_Transportation_Manager;

   -------------------
   -- Get_Transport --
   -------------------

   procedure Get_Transport
     (For_Empire : Athena.Handles.Empire.Empire_Handle;
      From_Star  : Athena.Handles.Star.Star_Handle;
      Capacity   : Non_Negative_Real;
      Ships      : out Athena.Ships.Lists.List;
      Remaining  : out Non_Negative_Real)
   is
      pragma Unreferenced (From_Star);
      Available : Non_Negative_Real := 0.0;

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
      begin
         if Available >= Capacity then
            return;
         end if;

         if Ship.Idle
           and then Athena.Ships.Total_Cargo_Space (Ship) > 0.0
         then
            Available := Available
              + Athena.Ships.Total_Cargo_Space (Ship);
            Ships.Append (Ship);
         end if;
      end Check_Available;

   begin
      Ships.Clear;

      For_Empire.Iterate_Managed_Ships
        (Manager => Athena.Handles.Transport_Manager,
         Process => Check_Available'Access);

      Remaining := Real'Max (Capacity - Available, 0.0);
   end Get_Transport;

   -----------------------
   -- Transport_Message --
   -----------------------

   function Transport_Message
     (Empire   : Athena.Handles.Empire_Reference;
      From     : Athena.Handles.Star_Reference;
      To       : Athena.Handles.Star_Reference;
      Cargo    : Athena.Handles.Cargo_Class;
      Quantity : Non_Negative_Real;
      Priority : Athena.Handles.Order_Priority)
      return Message_Type'Class
   is
   begin
      return Transport_Message_Type'
        (Empire   => Empire,
         From     => From,
         To       => To,
         Cargo    => Cargo,
         Quantity => Quantity,
         Priority => Priority);
   end Transport_Message;

end Athena.Managers.Transportation;
