with WL.String_Maps;

with Tropos.Reader;

with Athena.Handles.Commodity;

with Athena.Paths;

package body Athena.Configure.Commodities is

   type Deposit_Constraint_Creator is access
     function (Config : Tropos.Configuration)
               return Athena.Handles.Commodity.Resource_Constraint;

   package Deposit_Constraint_Maps is
     new WL.String_Maps (Deposit_Constraint_Creator);

   Deposit_Constraint_Map : Deposit_Constraint_Maps.Map;

   function Create_Frequency_Constraint
     (Config : Tropos.Configuration)
      return Athena.Handles.Commodity.Resource_Constraint;

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   procedure Load_Deposit_Constraints;

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
      Load_Deposit_Constraints;
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
         begin
            if Deposit_Constraint_Map.Contains (Name) then
               Commodity.Add_Deposit_Constraint
                 (Deposit_Constraint_Map.Element (Name) (Deposit_Config));
            else
               raise Constraint_Error with
                 "no such deposit constraint: " & Name;
            end if;
         end;
      end loop;
   end Configure_Commodity;

   ---------------------------------
   -- Create_Frequency_Constraint --
   ---------------------------------

   function Create_Frequency_Constraint
     (Config : Tropos.Configuration)
      return Athena.Handles.Commodity.Resource_Constraint
   is
      Name    : constant Frequency_Constraint_Name :=
        Frequency_Constraint_Name'Value (Config.Config_Name);
      Mean    : constant Unit_Real := Standard_Frequencies (Name, 1);
      Std_Dev : constant Unit_Real := Standard_Frequencies (Name, 2);
   begin
      return Athena.Handles.Commodity.Frequency_Constraint
        (Mean, Std_Dev);
   end Create_Frequency_Constraint;

   ------------------------------
   -- Load_Deposit_Constraints --
   ------------------------------

   procedure Load_Deposit_Constraints is

      procedure Add (Key : String;
                     Handler : Deposit_Constraint_Creator);

      ---------
      -- Add --
      ---------

      procedure Add (Key     : String;
                     Handler : Deposit_Constraint_Creator)
      is
      begin
         Deposit_Constraint_Map.Insert (Key, Handler);
      end Add;

   begin
      for Name in Frequency_Constraint_Name loop
         Add (Name'Image, Create_Frequency_Constraint'Access);
      end loop;
   end Load_Deposit_Constraints;

end Athena.Configure.Commodities;
