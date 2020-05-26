private package Athena.Color.Table is

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Athena_Color;
   procedure Add (Name : String;
                  Color : Athena_Color);

end Athena.Color.Table;
