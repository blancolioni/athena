with WL.String_Maps;

with Tropos.Reader;

with Athena.Handles.Commodity;
with Athena.Handles.Facility;

with Athena.Paths;

package body Athena.Configure.Facilities is

   type Modify_Facility_Procedure is access
     procedure (Facility : Athena.Handles.Facility.Facility_Handle;
                Config   : Tropos.Configuration);

   procedure Add_Input
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration);

   procedure Add_Habitability_Constraint
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration);

   procedure Add_Constant_Constraint
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration);

   procedure Add_Agriculture_Constraint
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration);

   package Modify_Facility_Maps is
     new WL.String_Maps (Modify_Facility_Procedure);

   Add_Constraints : Modify_Facility_Maps.Map;
   Add_Inputs      : Modify_Facility_Maps.Map;

   procedure Load_Modifiers
     with Pre => Add_Constraints.Is_Empty
       and then Add_Inputs.Is_Empty;

   procedure Configure_Facility
     (Config : Tropos.Configuration);

   --------------------------------
   -- Add_Agriculture_Constraint --
   --------------------------------

   procedure Add_Agriculture_Constraint
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration)
   is
   begin
      null;
   end Add_Agriculture_Constraint;

   -----------------------------
   -- Add_Constant_Constraint --
   -----------------------------

   procedure Add_Constant_Constraint
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration)
   is
   begin
      Facility.Add_Constant_Constraint
        (Real (Long_Float'(Config.Value)));
   end Add_Constant_Constraint;

   ---------------------------------
   -- Add_Habitability_Constraint --
   ---------------------------------

   procedure Add_Habitability_Constraint
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration)
   is
      pragma Unreferenced (Config);
   begin
      Facility.Add_Habitability_Constraint;
   end Add_Habitability_Constraint;

   ---------------
   -- Add_Input --
   ---------------

   procedure Add_Input
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Config   : Tropos.Configuration)
   is
   begin
      pragma Assert (Athena.Handles.Commodity.Exists (Config.Config_Name),
                     "in configuration for facility " & Facility.Tag
                     & ": no such input commodity: "
                     & Config.Config_Name);

      Facility.Add_Input
        (Commodity =>
           Athena.Handles.Commodity.Get_By_Tag (Config.Config_Name),
         Quantity  => Real (Long_Float'(Config.Value)));
   end Add_Input;

   ---------------------------
   -- Configure_Facilities --
   ---------------------------

   procedure Configure_Facilities is
   begin
      Load_Modifiers;
      Tropos.Reader.Read_Config
        (Path      => Athena.Paths.Config_File ("facilities"),
         Extension => "facility",
         Configure => Configure_Facility'Access);
   end Configure_Facilities;

   -------------------------
   -- Configure_Facility --
   -------------------------

   procedure Configure_Facility
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Real (Long_Float'(Config.Get (Name))));

      Facility : constant Athena.Handles.Facility.Facility_Handle :=
        Athena.Handles.Facility.Create
          (Tag       => Config.Config_Name,
           Commodity =>
             Athena.Handles.Commodity.Get_By_Tag (Config.Get ("output")),
           Quantity  => Get ("quantity"),
           Employees => Get ("employees"));

      procedure Modify_Facility
        (Using_Map : Modify_Facility_Maps.Map;
         Child     : Tropos.Configuration);

      ---------------------
      -- Modify_Facility --
      ---------------------

      procedure Modify_Facility
        (Using_Map : Modify_Facility_Maps.Map;
         Child     : Tropos.Configuration)
      is
      begin
         for Modify_Config of Child loop
            if Using_Map.Contains (Modify_Config.Config_Name) then
               Using_Map.Element (Modify_Config.Config_Name)
                 (Facility, Modify_Config);
            else
               raise Constraint_Error with
               Modify_Config.Config_Name & " not found in "
                 & Child.Config_Name;
            end if;
         end loop;
      end Modify_Facility;

   begin
      Modify_Facility (Add_Constraints, Config.Child ("constraints"));
      Modify_Facility (Add_Inputs, Config.Child ("inputs"));
   end Configure_Facility;

   --------------------
   -- Load_Modifiers --
   --------------------

   procedure Load_Modifiers is
   begin
      for Commodity of Athena.Handles.Commodity.All_Commodities loop
         Add_Inputs.Insert (Commodity.Tag, Add_Input'Access);
      end loop;
      Add_Constraints.Insert
        ("agriculture", Add_Agriculture_Constraint'Access);
      Add_Constraints.Insert
        ("factor", Add_Constant_Constraint'Access);
      Add_Constraints.Insert
        ("habitability", Add_Habitability_Constraint'Access);
   end Load_Modifiers;

end Athena.Configure.Facilities;
