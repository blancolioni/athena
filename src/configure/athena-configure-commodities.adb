with WL.String_Maps;

with Tropos.Reader;

with Athena.Resources;

with Athena.Handles.Commodity;

with Athena.Paths;

package body Athena.Configure.Commodities is

   type Resource_Constraint_Creator is access
     procedure (Frequency : in out Athena.Resources.Resource_Frequency;
                Config    : Tropos.Configuration);

   procedure Create_Age_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration);

   procedure Create_Composition_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration);

   procedure Create_Hydrosphere_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration);

   procedure Create_Life_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration);

   procedure Create_Zone_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration);

   package Resource_Constraint_Maps is
     new WL.String_Maps (Resource_Constraint_Creator);

   Resource_Constraint_Map : Resource_Constraint_Maps.Map;

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   procedure Load_Resource_Constraints;

   type Frequency_Constraint_Name is
     (Unlimited, Abundant, Common, Uncommon, Rare);

   Standard_Frequencies : constant array
     (Frequency_Constraint_Name, 1 .. 2)
     of Unit_Real :=
       (Unlimited => (0.0, 0.0),
        Abundant  => (0.9, 0.1),
        Common    => (0.5, 0.05),
        Uncommon  => (0.1, 0.01),
        Rare      => (0.01, 0.001));

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities is
   begin
      Load_Resource_Constraints;
      Tropos.Reader.Read_Config
        (Path      => Athena.Paths.Config_File ("commodities"),
         Extension => "commodity",
         Configure => Configure_Commodity'Access);
   end Configure_Commodities;

   -------------------------
   -- Configure_Commodity --
   -------------------------

   procedure Configure_Commodity
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Real (Long_Float'(Config.Get (Name))));

      function Get_Class return Athena.Handles.Commodity.Commodity_Class
      is (Athena.Handles.Commodity.Commodity_Class'Value
          (Config.Get ("class")));

      Commodity : constant Athena.Handles.Commodity.Commodity_Handle :=
        Athena.Handles.Commodity.Create
          (Tag         => Config.Config_Name,
           Density     => Get ("density"),
           Class       => Get_Class,
           Is_Abstract => Config.Get ("abstract"));

   begin
      for Deposit_Config of Config.Child ("deposits") loop
         declare
            Name : constant String := Deposit_Config.Config_Name;
            Constraint : Frequency_Constraint_Name;
         begin
            begin
               Constraint := Frequency_Constraint_Name'Value (Name);
            exception
               when others =>
                  raise Constraint_Error with
                    "no such frequency constraint: " & Name;
            end;

            declare
               Mean : constant Unit_Real :=
                        Standard_Frequencies (Constraint, 1);
               Std_Dev : constant Unit_Real :=
                           Standard_Frequencies (Constraint, 2);
               Frequency : Athena.Resources.Resource_Frequency :=
                             Athena.Resources.Create_Resource_Frequency
                               (Mean, Std_Dev);
            begin
               for Constraint_Config of Deposit_Config loop
                  declare
                     Name : constant String := Constraint_Config.Config_Name;
                  begin
                     if Resource_Constraint_Map.Contains (Name) then
                        Resource_Constraint_Map.Element (Name)
                          (Frequency, Constraint_Config);
                     else
                        raise Constraint_Error with
                        Config.Config_Name
                          & ": no such resource constraint: " & Name;
                     end if;
                  end;
               end loop;
               Commodity.Add_Resource_Frequency (Frequency);
            end;
         end;
      end loop;
   end Configure_Commodity;

   ---------------------------
   -- Create_Age_Constraint --
   ---------------------------

   procedure Create_Age_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration)
   is
   begin
      Athena.Resources.Age_Constraint
        (Frequency, Real (Long_Float'(Config.Value)));
   end Create_Age_Constraint;

   -----------------------------------
   -- Create_Composition_Constraint --
   -----------------------------------

   procedure Create_Composition_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration)
   is
   begin
      Athena.Resources.Composition_Constraint
        (Frequency, Athena.Handles.World_Composition'Value (Config.Value));
   end Create_Composition_Constraint;

   -----------------------------------
   -- Create_Hydrosphere_Constraint --
   -----------------------------------

   procedure Create_Hydrosphere_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration)
   is
   begin
      Athena.Resources.Hydrosphere_Constraint
        (Frequency, Minimum => Real (Long_Float'(Config.Value)));
   end Create_Hydrosphere_Constraint;

   ----------------------------
   -- Create_Life_Constraint --
   ----------------------------

   procedure Create_Life_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration)
   is
   begin
      Athena.Resources.Life_Constraint
        (Frequency, Athena.Handles.Life_Complexity_Type'Value (Config.Value));
   end Create_Life_Constraint;

   ----------------------------
   -- Create_Zone_Constraint --
   ----------------------------

   procedure Create_Zone_Constraint
     (Frequency : in out Athena.Resources.Resource_Frequency;
      Config    : Tropos.Configuration)
   is
   begin
      Athena.Resources.Zone_Constraint
        (Frequency, Athena.Handles.Stellar_Orbit_Zone'Value (Config.Value));
   end Create_Zone_Constraint;

   ---------------------------------
   -- Create_Frequency_Constraint --
   ---------------------------------

   --  function Create_Frequency_Constraint
   --    (Config : Tropos.Configuration)
   --     return Athena.Handles.Commodity.Resource_Constraint
   --  is
   --     Name    : constant Frequency_Constraint_Name :=
   --       Frequency_Constraint_Name'Value (Config.Config_Name);
   --     Mean    : constant Unit_Real := Standard_Frequencies (Name, 1);
   --     Std_Dev : constant Unit_Real := Standard_Frequencies (Name, 2);
   --  begin
   --     return Athena.Handles.Commodity.Frequency_Constraint
   --       (Mean, Std_Dev);
   --  end Create_Frequency_Constraint;

   ------------------------------
   -- Load_Deposit_Constraints --
   ------------------------------

   --  procedure Load_Deposit_Constraints is
   --
   --     procedure Add (Key : String;
   --                    Handler : Deposit_Constraint_Creator);
   --
   --     ---------
   --     -- Add --
   --     ---------
   --
   --     procedure Add (Key     : String;
   --                    Handler : Deposit_Constraint_Creator)
   --     is
   --     begin
   --        Deposit_Constraint_Map.Insert (Key, Handler);
   --     end Add;
   --
   --  begin
   --     for Name in Frequency_Constraint_Name loop
   --        Add (Name'Image, Create_Frequency_Constraint'Access);
   --     end loop;
   --  end Load_Deposit_Constraints;

   -------------------------------
   -- Load_Resource_Constraints --
   -------------------------------

   procedure Load_Resource_Constraints is

      procedure Add (Key : String;
                     Handler : Resource_Constraint_Creator);

      ---------
      -- Add --
      ---------

      procedure Add (Key     : String;
                     Handler : Resource_Constraint_Creator)
      is
      begin
         Resource_Constraint_Map.Insert (Key, Handler);
      end Add;

   begin
      Add ("minimum-age", Create_Age_Constraint'Access);
      Add ("composition", Create_Composition_Constraint'Access);
      Add ("hydrosphere", Create_Hydrosphere_Constraint'Access);
      Add ("life", Create_Life_Constraint'Access);
      Add ("zone", Create_Zone_Constraint'Access);
   end Load_Resource_Constraints;

end Athena.Configure.Commodities;
