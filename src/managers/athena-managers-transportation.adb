with Ada.Containers.Doubly_Linked_Lists;

with Athena.Empires;
with Athena.Ships.Designs;
with Athena.Ships.Orders;
with Athena.Technology;

with Minerva.Ship;
with Minerva.Ship_Design;
with Minerva.Empire;

package body Athena.Managers.Transportation is

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Minerva.Ship.Ship_Handle, Minerva.Ship."=");

   type Transportation_Manager is
     new Athena_Manager_Script with null record;

   overriding function Identifier
     (Manager : Transportation_Manager)
      return String
   is ("transportation");

   overriding procedure Create_Orders
     (Manager : Transportation_Manager);

   overriding procedure Process_Message
     (Manager : Transportation_Manager;
      Msg     : Message_Type'Class);

   procedure Get_Transport
     (Manager    : Transportation_Manager'Class;
      From_Star  : Minerva.Star.Star_Handle;
      Capacity   : Non_Negative_Real;
      Ships      : out Ship_Lists.List;
      Remaining  : out Non_Negative_Real);

   type Transport_Message_Type is
     new Message_Type with
      record
         Empire   : Minerva.Empire.Empire_Handle;
         From     : Minerva.Star.Star_Handle;
         To       : Minerva.Star.Star_Handle;
         Cargo    : Minerva.Db.Cargo_Type;
         Quantity : Non_Negative_Real;
         Priority : Integer;
      end record;

   -------------------
   -- Create_Orders --
   -------------------

   overriding procedure Create_Orders
     (Manager : Transportation_Manager)
   is
   begin
      null;
   end Create_Orders;

   ------------------------------------
   -- Default_Transportation_Manager --
   ------------------------------------

   function Default_Transportation_Manager
     return Athena_Manager_Script'Class
   is
   begin
      return Manager : constant Transportation_Manager :=
        Transportation_Manager'
          (Name     => +"transport",
           Empire   => <>,
           Manager => <>,
           Priority => 1050);
   end Default_Transportation_Manager;

   -------------------
   -- Get_Transport --
   -------------------

   procedure Get_Transport
     (Manager    : Transportation_Manager'Class;
      From_Star  : Minerva.Star.Star_Handle;
      Capacity   : Non_Negative_Real;
      Ships      : out Ship_Lists.List;
      Remaining  : out Non_Negative_Real)
   is
      pragma Unreferenced (From_Star);
      Available : Non_Negative_Real := 0.0;

      procedure Check_Available
        (Ship : Minerva.Ship.Ship_Class);

      ---------------------
      -- Check_Available --
      ---------------------

      procedure Check_Available
        (Ship : Minerva.Ship.Ship_Class)
      is
      begin
         if Available >= Capacity then
            return;
         end if;

         if Athena.Ships.Is_Idle (Ship)
           and then Athena.Ships.Total_Cargo_Space (Ship) > 0.0
         then
            Available := Available
              + Athena.Ships.Total_Cargo_Space (Ship);
            Ships.Append (Ship.To_Ship_Handle);
         end if;
      end Check_Available;

   begin
      Ships.Clear;

      for Ship of
        Minerva.Ship.Select_By_Empire_Manager
          (Manager.Manager)
      loop
         Check_Available (Ship);
      end loop;

      Remaining := Real'Max (Capacity - Available, 0.0);
   end Get_Transport;

   ---------------------
   -- Process_Message --
   ---------------------

   overriding procedure Process_Message
     (Manager : Transportation_Manager;
      Msg     : Message_Type'Class)
   is
      Message : constant Transport_Message_Type :=
                  Transport_Message_Type (Msg);
      From    : constant Minerva.Star.Star_Handle := Message.From;
      Ships   : Ship_Lists.List;
      Missing : Non_Negative_Real;
   begin

      Manager.Log
        ("transport " & Image (Message.Quantity)
         & " " & Message.Cargo'Image
         & " from " & Message.From.Name
         & " to " & Message.To.Name);

      Get_Transport
        (Manager    => Manager,
         From_Star  => From,
         Capacity   => Message.Quantity,
         Ships      => Ships,
         Remaining  => Missing);

      if Missing > 0.0 then
         declare
            Transport : constant Minerva.Ship_Design.Ship_Design_Class :=
                          Athena.Empires.Standard_Transport_Design
                            (Manager.Empire);
            Cargo     : constant Non_Negative_Real :=
                          Athena.Ships.Designs.Cargo_Space
                            (Transport,
                             Athena.Empires.Current_Tec_Level
                               (Manager.Empire,
                                Athena.Technology.Get ("cargo")));
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
               --     Fleet    => Minerva.Fleet.Empty_Handle,
               --     Manager  => Manager,
               --     Send_To  =>
               --       Minerva.Star.Get (Order.From),
               --     Priority => Manager.Priority);
            end if;
         end;
      end if;

      declare
         Remaining : Non_Negative_Real := Message.Quantity;
      begin
         for Ship of Ships loop
            if Athena.Ships.Is_Idle (Ship) then
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
                     if not Athena.Ships.Has_Star_Location (Ship, From) then
                        Athena.Ships.Orders.Move_To (Ship, From);
                     end if;

                     Athena.Ships.Orders.Load_Cargo
                       (Ship, Message.Cargo, Loaded - Have);
                  end if;

                  Remaining := Real'Max (Remaining - Loaded - Have, 0.0);

                  Athena.Ships.Orders.Move_To
                    (Ship, Message.To);
                  Athena.Ships.Orders.Unload_Cargo
                    (Ship, Message.Cargo, Loaded + Have);
               end;
            end if;
         end loop;
      end;
   end Process_Message;

   -----------------------
   -- Transport_Message --
   -----------------------

   function Transport_Message
     (Empire   : Minerva.Empire.Empire_Class;
      From     : Minerva.Star.Star_Class;
      To       : Minerva.Star.Star_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real;
      Priority : Integer)
      return Message_Type'Class
   is
   begin
      return Transport_Message_Type'
        (Empire   => Empire.To_Empire_Handle,
         From     => From.To_Star_Handle,
         To       => To.To_Star_Handle,
         Cargo    => Cargo,
         Quantity => Quantity,
         Priority => Priority);
   end Transport_Message;

end Athena.Managers.Transportation;
