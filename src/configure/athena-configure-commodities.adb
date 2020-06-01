with Tropos.Reader;

with Athena.Handles.Commodity;

with Athena.Paths;

package body Athena.Configure.Commodities is

   procedure Configure_Commodity
     (Config : Tropos.Configuration);

   ---------------------------
   -- Configure_Commodities --
   ---------------------------

   procedure Configure_Commodities is
   begin
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

   begin
      Athena.Handles.Commodity.Create
        (Tag      => Config.Config_Name,
         Tonnage  => Get ("tonnage"),
         Is_Food  => Config.Get ("food"),
         Is_Water => Config.Get ("water"));
   end Configure_Commodity;

end Athena.Configure.Commodities;
