with Athena.Signals;

private package Athena.Signal_Store is

   function Exists (Signal_Name : String) return Boolean;
   function Get (Signal_Name : String) return Athena.Signals.Signal_Type;

   procedure Add
     (Signal_Name : String;
      Signal      : Athena.Signals.Signal_Type);

end Athena.Signal_Store;
