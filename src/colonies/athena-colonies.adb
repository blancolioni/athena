with Athena.Identifiers;
with Athena.Logging;
with Athena.Turns;

with Minerva.Colony_Order;

package body Athena.Colonies is

   Log_Use_Assets : constant Boolean := True;

   ---------------
   -- Add_Cargo --
   ---------------

   procedure Add_Cargo
     (Colony   : Colony_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : in out Non_Negative_Real)
   is
      use all type Minerva.Db.Cargo_Type;
   begin
      case Cargo is
         when Colonists =>
            if Colony.Population < 10.0 then
               Quantity := Non_Negative_Real'Min (Quantity, 10.0);
            else
               Quantity :=
                 Non_Negative_Real'Min (Quantity, Colony.Population);
            end if;

            Colony.Update_Colony
              .Set_Population (Colony.Population + Quantity)
              .Done;

         when Industry =>
            if Colony.Industry < 10.0 then
               Quantity := Non_Negative_Real'Min (Quantity, 10.0);
            else
               Quantity :=
                 Non_Negative_Real'Min (Quantity, Colony.Industry);
            end if;

            Colony.Update_Colony
              .Set_Industry (Colony.Industry + Quantity)
              .Done;

         when Material =>
            Colony.Update_Colony
              .Set_Material (Colony.Material + Quantity)
              .Done;
      end case;
   end Add_Cargo;

   -----------------
   -- Best_Colony --
   -----------------

   function Best_Colony
     (Owned_By : Minerva.Empire.Empire_Class;
      Score    : not null access
        function (Colony : Colony_Class) return Real)
      return Colony_Class
   is
      Best_Score  : Non_Negative_Real := 0.0;
      Best_Colony : Minerva.Colony.Colony_Handle;
   begin
      for Colony of
        Minerva.Colony.Select_By_Empire (Owned_By)
      loop
         declare
            This_Score : constant Real := Score (Colony);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best_Colony := Colony.To_Colony_Handle;
            end if;
         end;
      end loop;
      return Best_Colony;
   end Best_Colony;

   --------------------
   -- Build_Industry --
   --------------------

   procedure Build_Industry
     (Colony   : Colony_Class;
      Priority : Integer;
      Quantity : Non_Negative_Real)
   is
   begin
      Athena.Colonies.Log_Colony
        (Colony,
         "order " & Image (Quantity)
         & " industry");
      Minerva.Colony_Order.Create
        (Turn     => Athena.Turns.Current_Turn,
         Empire   => Colony.Empire,
         Priority => Priority,
         Colony   => Colony,
         Category => Minerva.Db.Build_Industry,
         Value    => Quantity);
   end Build_Industry;

   -----------------
   -- Can_Provide --
   -----------------

   function Can_Provide
     (Colony      : Colony_Class;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0)
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
     (Colony    : Colony_Class;
      New_Owner : Minerva.Empire.Empire_Class)
   is
   begin
      Athena.Logging.Log
        (New_Owner.Name & " captures colony on "
         & Colony.Star.Name
         & " from " & Colony.Empire.Name);
      Colony.Update_Colony
        .Set_Empire (New_Owner)
        .Done;
      Colony.Star.Update_Star
        .Set_Owner (New_Owner)
        .Done;
   end Capture_Colony;

   -----------------
   -- Find_Colony --
   -----------------

   function Find_Colony
     (Owned_By : Minerva.Empire.Empire_Class;
      Test     : not null access function
        (Colony : Minerva.Colony.Colony_Class) return Boolean)
      return Minerva.Colony.Colony_Class
   is
   begin
      for Colony of
        Minerva.Colony.Select_By_Empire (Owned_By)
      loop
         if Test (Colony) then
            return Colony;
         end if;
      end loop;
      return Minerva.Colony.Empty_Handle;
   end Find_Colony;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (Star : Minerva.Star.Star_Class)
      return Colony_Class
   is
   begin
      return Minerva.Colony.Get_By_Star (Star);
   end Get_Colony;

   ----------------
   -- Log_Colony --
   ----------------

   procedure Log_Colony
     (Colony  : Colony_Class;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Colony.Empire.Name
         & "/"
         & Colony.Star.Name
         & ": "
         & Message);
   end Log_Colony;

   ----------------
   -- New_Colony --
   ----------------

   procedure New_Colony
     (Star        : Minerva.Star.Star_Class;
      Empire      : Minerva.Empire.Empire_Class;
      Initial_Pop : in out Non_Negative_Real)
   is
      Colony : constant Minerva.Colony.Colony_Handle :=
                 Minerva.Colony.Create
                   (Identifier => Athena.Identifiers.Next_Identifier,
                    Star       => Star,
                    Empire     => Empire,
                    Founded    => Athena.Turns.Current_Turn,
                    Construct  => 0.0,
                    Population => 0.0,
                    Industry   => 0.0,
                    Material   => 0.0);
   begin
      Star.Update_Star
        .Set_Owner (Empire)
        .Done;
      Add_Cargo (Colony, Minerva.Db.Colonists, Initial_Pop);
      Athena.Logging.Log
        (Empire.Name & ": new colony founded at " & Star.Name);
   end New_Colony;

   ----------------------
   -- Produce_Material --
   ----------------------

   procedure Produce_Material
     (Colony   : Colony_Class;
      Quantity : Non_Negative_Real)
   is
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
         Log_Colony
           (Colony,
            "ordered "
            & Image (Quantity)
            & " material"
            & "; available construct "
            & Image (Colony.Construct)
            & " resource "
            & Image (Colony.Star.Resource * 100.0) & "%"
            & "; produced "
            & Image (Produced)
            & "; new material stock "
            & Image (New_Material));

         Colony.Update_Colony
           .Set_Material (New_Material)
           .Set_Construct (New_Construct)
           .Done;
      end;
   exception
      when others =>
         Log_Colony
           (Colony,
            "exception while producing material: max = "
            & Image (Max)
            & "; construct = "
            & Image (Colony.Construct)
            & "; resource = "
            & Image (Colony.Star.Resource));
         raise;

   end Produce_Material;

   ------------------
   -- Remove_Cargo --
   ------------------

   procedure Remove_Cargo
     (Colony   : Colony_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : in out Non_Negative_Real)
   is
      use all type Minerva.Db.Cargo_Type;
   begin
      case Cargo is
         when Colonists =>
            Quantity :=
              Non_Negative_Real'Min (Colony.Population / 2.0, Quantity);
            Colony.Update_Colony
              .Set_Population (Colony.Population - Quantity)
              .Done;
         when Industry =>
            Quantity :=
              Non_Negative_Real'Min (Colony.Industry / 2.0, Quantity);
            Colony.Update_Colony
              .Set_Industry (Colony.Industry - Quantity)
              .Done;
         when Material =>
            Quantity :=
              Non_Negative_Real'Min (Colony.Material, Quantity);
            Colony.Update_Colony
              .Set_Material (Colony.Material - Quantity)
              .Done;
      end case;
   end Remove_Cargo;

   -----------------------
   -- Request_Construct --
   -----------------------

   procedure Request_Construct
     (Colony    : Colony_Class;
      Construct : in out Non_Negative_Real)
   is
   begin
      Construct := Real'Min (Construct, Colony.Construct);
      Colony.Update_Colony
        .Set_Construct (Colony.Construct - Construct)
        .Done;
   end Request_Construct;

   ----------------------
   -- Request_Material --
   ----------------------

   procedure Request_Material
     (Colony   : Colony_Class;
      Material : in out Non_Negative_Real)
   is
      Missing : constant Non_Negative_Real :=
                  Real'Max (0.0, Material - Colony.Material);
   begin
      if Missing > 0.0 then
         declare
            Construct : Non_Negative_Real :=
                          Missing / Colony.Star.Resource;
         begin
            Request_Construct (Colony, Construct);
            if Construct > 0.0 then
               Log_Colony
                 (Colony,
                  "using " & Image (Construct)
                  & " construct to create "
                  & Image (Construct * Colony.Star.Resource)
                  & " material"
                  & "; remaining constuct " & Image (Colony.Construct));
            end if;

            Material :=
              Real'Min (Colony.Material + Construct * Colony.Star.Resource,
                        Material);
            Colony.Update_Colony
              .Set_Material (0.0)
              .Done;
         end;
      else
         Colony.Update_Colony
           .Set_Material (Colony.Material - Material)
           .Done;
      end if;
   end Request_Material;

   ----------------
   -- Use_Assets --
   ----------------

   procedure Use_Assets
     (Colony      : Colony_Class;
      Description : String;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0)
   is
   begin

      if Log_Use_Assets then
         if Construct = 0.0 and then Material = 0.0 then
            Log_Colony (Colony, "uses no assets for " & Description);
         elsif Construct = 0.0 then
            Log_Colony
              (Colony,
               "uses " & Image (Material) & " material for "
               & Description);
         elsif Material = 0.0 then
            Log_Colony
              (Colony,
               "uses " & Image (Construct) & " construct for "
               & Description);
         else
            Log_Colony
              (Colony,
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
         Colony.Update_Colony
           .Set_Construct (New_Construct)
           .Set_Material (New_Material)
           .Done;
      end;
   end Use_Assets;

end Athena.Colonies;
