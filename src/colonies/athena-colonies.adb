with Athena.Logging;
with Athena.Money;
with Athena.Real_Images;

with Athena.Empires;
with Athena.Stars;

with Athena.Cargo.Commodities;
with Athena.Handles.Commodity;

package body Athena.Colonies is

   Log_Use_Assets : constant Boolean := False;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Log
     (Colony : Athena.Handles.Colony.Colony_Handle;
      Message : String);

   -----------------
   -- Best_Colony --
   -----------------

   function Best_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      Score    : not null access
        function (Colony : Athena.Handles.Colony.Colony_Handle) return Real)
      return Athena.Handles.Colony.Colony_Handle
   is
      Best_Score : Real := Real'First;
      Result     : Athena.Handles.Colony.Colony_Handle :=
                     Athena.Handles.Colony.Empty_Handle;

      procedure Check_Colony
        (Reference : Athena.Handles.Colony_Reference);

      ------------------
      -- Check_Colony --
      ------------------

      procedure Check_Colony
        (Reference : Athena.Handles.Colony_Reference)
      is
         Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                        Athena.Handles.Colony.Get (Reference);
         This_Score : constant Real := Score (Colony);
      begin
         if This_Score > Best_Score then
            Best_Score := This_Score;
            Result := Colony;
         end if;
      end Check_Colony;

   begin
      Owned_By.Iterate_Colonies (Check_Colony'Access);
      return Result;
   end Best_Colony;

   -----------------
   -- Can_Provide --
   -----------------

   function Can_Provide
     (Colony    : Athena.Handles.Colony.Colony_Handle;
      Construct : Non_Negative_Real := 0.0;
      Material  : Non_Negative_Real := 0.0)
      return Boolean
   is
   begin
      if Colony.Construct >= Construct
        and then Colony.Material >= Material
      then
         return True;
      end if;

      if Colony.Construct < Construct then
         return False;
      end if;

      declare
         Used_Construct : constant Non_Negative_Real :=
                            Material / Colony.Star.Resource;
      begin
         return Used_Construct + Construct <= Colony.Construct;
      end;

   end Can_Provide;

   --------------------
   -- Capture_Colony --
   --------------------

   procedure Capture_Colony
     (Colony      : Athena.Handles.Colony.Colony_Handle;
      Captured_By : Athena.Handles.Empire.Empire_Handle)
   is
      Old_Owner : constant Athena.Handles.Empire.Empire_Handle :=
                    Colony.Owner;
   begin
      Colony.Owner.Remove_Colony (Colony.Reference);
      Colony.Set_Owner (Captured_By);
      Colony.Star.Set_Owner (Captured_By.Reference);
      Colony.Owner.Add_Colony (Colony.Reference);
      Athena.Handles.Colony.Colony_Owner_Changed
        (Colony    => Colony,
         Old_Owner => Old_Owner,
         New_Owner => Captured_By);
   end Capture_Colony;

   -----------------
   -- Find_Colony --
   -----------------

   function Find_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      Test     : not null access function
        (Colony : Athena.Handles.Colony.Colony_Handle) return Boolean)
      return Athena.Handles.Colony.Colony_Handle
   is
      function Local_Test
        (Reference : Athena.Handles.Colony_Reference)
         return Boolean
      is (Test (Athena.Handles.Colony.Get (Reference)));

   begin
      return Athena.Handles.Colony.Get
        (Owned_By.Find_Colony (Local_Test'Access));
   end Find_Colony;

   ----------------------
   -- For_All_Colonies --
   ----------------------

   procedure For_All_Colonies
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      Process  : not null access procedure
        (Colony : Athena.Handles.Colony.Colony_Handle))
   is
      procedure Local_Process (Reference : Athena.Handles.Colony_Reference);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process (Reference : Athena.Handles.Colony_Reference) is
      begin
         Process (Athena.Handles.Colony.Get (Reference));
      end Local_Process;

   begin
      Owned_By.Iterate_Colonies (Local_Process'Access);
   end For_All_Colonies;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (At_Star : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Colony.Colony_Handle
   is
   begin
      return Athena.Handles.Colony.Get
        (At_Star.Colony);
   end Get_Colony;

   procedure Load_Cargo_From_Colony
     (From  : Athena.Handles.Colony.Colony_Handle;
      To    : Athena.Cargo.Cargo_Holder_Interface'Class;
      Cargo : Athena.Cargo.Cargo_Interface'Class;
      Max   : Non_Negative_Real)
   is
   begin
      case Cargo.Category is
         when Athena.Cargo.People =>
            declare
               Required_Pop  : constant Non_Negative_Real := Max;
               Available_Pop : constant Non_Negative_Real :=
                                 From.Population;
               Loaded_Pop    : constant Non_Negative_Real :=
                                 Real'Min (Required_Pop,
                                           Available_Pop);
               Remaining_Pop : constant Non_Negative_Real :=
                                 Available_Pop - Loaded_Pop;
            begin
               From.Log ("supplying " & Image (Loaded_Pop) & " people");
               To.Add_Cargo (Cargo, Loaded_Pop);
               From.Set_Population (Remaining_Pop);
            end;

         when Athena.Cargo.Fuel =>
            From.Log ("supplying " & Image (Max) & " fuel");
            To.Add_Cargo (Cargo, Max);

         when Athena.Cargo.Commodity =>
            declare
               Commodity : constant Handles.Commodity.Commodity_Handle :=
                             Athena.Cargo.Commodities.Get_Commodity (Cargo);
               Required  : constant Non_Negative_Real := Max;
               Available : constant Non_Negative_Real :=
                             From.Get_Stock (Commodity);
               Loaded    : constant Non_Negative_Real :=
                             Real'Min (Required, Available);
            begin
               From.Log ("supplying " & Image (Loaded) & " " & Commodity.Tag);
               To.Add_Cargo (Cargo, Loaded);
               From.Remove_Stock (Commodity, Loaded);
            end;

      end case;
   end Load_Cargo_From_Colony;

   ---------
   -- Log --
   ---------

   procedure Log
     (Colony  : Athena.Handles.Colony.Colony_Handle;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Colony.Owner.Name
         & ": colony on "
         & Colony.Star.Name
         & ": "
         & Message);
   end Log;

   --------------------
   -- Nearest_Colony --
   --------------------

   function Nearest_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      To_Star  : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Colony.Colony_Handle
   is
      Distance : Non_Negative_Real := Non_Negative_Real'Last;
      Result   : Athena.Handles.Colony.Colony_Handle :=
                   Athena.Handles.Colony.Empty_Handle;

      procedure Check (Reference : Athena.Handles.Colony_Reference);

      procedure Check (Reference : Athena.Handles.Colony_Reference) is
         Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                        Athena.Handles.Colony.Get (Reference);
         D          : constant Non_Negative_Real :=
                        Athena.Stars.Distance (Colony.Star, To_Star);
      begin
         if D < Distance then
            Distance := D;
            Result := Colony;
         end if;
      end Check;

   begin
      Owned_By.Iterate_Colonies (Check'Access);
      return Result;
   end Nearest_Colony;

   ----------------
   -- New_Colony --
   ----------------

   function New_Colony
     (At_Star  : Athena.Handles.Star.Star_Handle;
      Owner    : Athena.Handles.Empire.Empire_Handle;
      Pop      : Non_Negative_Real;
      Industry : Non_Negative_Real;
      Material : Non_Negative_Real)
      return Athena.Handles.Colony.Colony_Handle
   is
      Colony : constant Athena.Handles.Colony.Colony_Handle :=
                 Athena.Handles.Colony.Create
                   (Star       => At_Star,
                    Owner     => Owner,
                    Pop        => Pop,
                    Industry   => Industry,
                    Material   => Material);
   begin
      Colony.Log ("new colony");
      At_Star.Set_Colony (Colony.Reference);
      At_Star.Set_Owner (Owner.Reference);
      Owner.Add_Colony (Colony.Reference);
      Athena.Handles.Colony.Colony_Owner_Changed
        (Colony    => Colony,
         Old_Owner => Athena.Handles.Empire.Empty_Handle,
         New_Owner => Owner);
      return Colony;
   end New_Colony;

   ----------------------
   -- Produce_Material --
   ----------------------

   procedure Produce_Material
     (Colony   : Athena.Handles.Colony.Colony_Handle'Class;
      Quantity : Non_Negative_Real)
   is
      use Athena.Money;
      Max : Non_Negative_Real := Quantity;
   begin
      Max := Real'Min (Max, Colony.Construct * Colony.Star.Resource);
      --  Max := Real'Min (Max, To_Real (Colony.Empire.Cash));

      declare
         Produced      : constant Non_Negative_Real := Max;
         New_Construct : constant Non_Negative_Real :=
                           Real'Max
                             (Colony.Construct
                              - Produced / Colony.Star.Resource,
                              0.0);
         New_Material  : constant Non_Negative_Real :=
                           Colony.Material + Produced;
      begin
         Colony.Log
           ("ordered "
            & Image (Quantity)
            & " material"
            & "; available construct "
            & Image (Colony.Construct)
            & " resource "
            & Image (Colony.Star.Resource * 100.0) & "%"
            & " cash "
            & Athena.Money.Show (Colony.Owner.Cash)
            & "; produced "
            & Image (Produced)
            & "; new material stock "
            & Image (New_Material));

         Colony.Set_Material (New_Material);
         Colony.Set_Construct (New_Construct);
         Athena.Empires.Pay (Colony.Owner, To_Money (Produced),
                             "material production");
      end;
   exception
      when others =>
         Colony.Log
           ("exception while producing material: max = "
            & Image (Max)
            & "; construct = "
            & Image (Colony.Construct)
            & "; cash = "
            & Athena.Money.Show (Colony.Owner.Cash)
            & "; resource = "
            & Image (Colony.Star.Resource));
         raise;

   end Produce_Material;

   ----------------------------
   -- Unload_Cargo_To_Colony --
   ----------------------------

   procedure Unload_Cargo_To_Colony
     (To    : Athena.Handles.Colony.Colony_Handle;
      From  : Athena.Cargo.Cargo_Holder_Interface'Class;
      Cargo : Athena.Cargo.Cargo_Interface'Class;
      Max   : Non_Negative_Real)
   is
   begin
      case Cargo.Category is
         when Athena.Cargo.People =>
            To.Log ("adding " & Image (Max) & " people");
            To.Set_Population (To.Population + Max);
            From.Remove_Cargo (Cargo, Max);

         when Athena.Cargo.Fuel =>
            To.Log ("adding " & Image (Max) & " fuel");
            From.Remove_Cargo (Cargo, Max);

         when Athena.Cargo.Commodity =>
            declare
               Commodity : constant Handles.Commodity.Commodity_Handle :=
                             Athena.Cargo.Commodities.Get_Commodity (Cargo);
            begin
               To.Log ("adding " & Image (Max) & " " & Commodity.Tag);
               To.Add_Stock (Commodity, Max);
               From.Remove_Cargo (Cargo, Max);
            end;

      end case;
   end Unload_Cargo_To_Colony;

   ----------------
   -- Use_Assets --
   ----------------

   procedure Use_Assets
     (Colony      : Athena.Handles.Colony.Colony_Handle;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0;
      Description : String)
   is
   begin

      if Log_Use_Assets then
         if Construct = 0.0 and then Material = 0.0 then
            Log (Colony, "uses no assets for " & Description);
         elsif Construct = 0.0 then
            Log (Colony,
                 "uses " & Image (Material) & " material for "
                 & Description);
         elsif Material = 0.0 then
            Log (Colony,
                 "uses " & Image (Construct) & " construct for "
                 & Description);
         else
            Log (Colony,
                 "uses " & Image (Construct) & " construct"
                 & " and " & Image (Material) & " material for "
                 & Description);
         end if;
      end if;

      declare
         New_Construct : constant Non_Negative_Real :=
                           Real'Max (Colony.Construct - Construct, 0.0);
         New_Material  : constant Non_Negative_Real :=
                           Real'Max (Colony.Material - Material, 0.0);
      begin
         Colony.Set_Construct (New_Construct);
         Colony.Set_Material (New_Material);
      end;
   end Use_Assets;

end Athena.Colonies;
