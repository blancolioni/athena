with WL.String_Maps;

package body Athena.Signal_Store is

   package Signal_Maps is
     new WL.String_Maps (Athena.Signals.Signal_Type, Athena.Signals."=");

   Signal_Map : Signal_Maps.Map;

   ---------
   -- Add --
   ---------

   procedure Add
     (Signal_Name : String;
      Signal      : Athena.Signals.Signal_Type)
   is
   begin
      Signal_Map.Insert (Signal_Name, Signal);
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (Signal_Name : String) return Boolean is
   begin
      return Signal_Map.Contains (Signal_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Signal_Name : String) return Athena.Signals.Signal_Type is
   begin
      return Signal_Map.Element (Signal_Name);
   end Get;

end Athena.Signal_Store;
