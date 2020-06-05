with Athena.Handles.Empire;

package Athena.Cargo.Colonists is

   function Is_Colonist
     (Cargo : Cargo_Interface'Class)
      return Boolean;

   function Get_Empire
     (Cargo : Cargo_Interface'Class)
      return Athena.Handles.Empire.Empire_Handle
     with Pre => Is_Colonist (Cargo);

   function Colonist_Cargo
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Cargo_Interface'Class
     with Post => Is_Colonist (Colonist_Cargo'Result)
     and then Athena.Handles.Empire."="
       (Empire, Get_Empire (Colonist_Cargo'Result));

end Athena.Cargo.Colonists;
