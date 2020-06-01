with Tropos.Reader;

with Athena.Handles.Commodity;
with Athena.Handles.Production;

with Athena.Paths;

package body Athena.Configure.Production is

   procedure Configure_Production
     (Config : Tropos.Configuration);

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production is
   begin
      Tropos.Reader.Read_Config
        (Path      => Athena.Paths.Config_File ("economy/production"),
         Extension => "production",
         Configure => Configure_Production'Access);
   end Configure_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Real (Long_Float'(Config.Get (Name))));

      Handle : constant Athena.Handles.Production.Production_Handle :=
                 Athena.Handles.Production.Create
                   (Tag     => Config.Config_Name,
                    Commodity  =>
                      Athena.Handles.Commodity.Get_By_Tag
                        (Config.Get ("commodity")),
                    Employment => Get ("employment"));
   begin

      for Constraint_Config of Config.Child ("constraints") loop
         declare
            Tag : constant String := Constraint_Config.Config_Name;
         begin
            if Tag = "constant" then
               Handle.Add_Constant_Constraint
                 (Real (Long_Float'(Constraint_Config.Value)));
            elsif Tag = "habitability" then
               Handle.Add_Habitability_Constraint;
            else
               raise Constraint_Error with
                 "unknown production constraint: " & Tag;
            end if;
         end;
      end loop;
   end Configure_Production;

end Athena.Configure.Production;
