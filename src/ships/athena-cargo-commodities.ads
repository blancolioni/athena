with Athena.Handles.Commodity;

package Athena.Cargo.Commodities is

   function Has_Commodity
     (Cargo : Cargo_Interface'Class)
      return Boolean;

   function Get_Commodity
     (Cargo : Cargo_Interface'Class)
      return Athena.Handles.Commodity.Commodity_Handle
     with Pre => Has_Commodity (Cargo);

   function Commodity_Cargo
     (Commodity : Athena.Handles.Commodity.Commodity_Handle)
      return Cargo_Interface'Class
     with Pre => not Commodity.Is_Abstract,
     Post => Has_Commodity (Commodity_Cargo'Result);

end Athena.Cargo.Commodities;
