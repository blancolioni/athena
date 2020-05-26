with Athena.Handles.Ship;

package body Athena.Ships.Create is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      Star        : Athena.Handles.Star.Star_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Name        : String;
      Fleet       : Athena.Handles.Fleet.Fleet_Handle;
      Manager     : Athena.Handles.Manager_Class;
      Destination : Athena.Handles.Star.Star_Handle :=
        Athena.Handles.Star.Empty_Handle)
   is
      Ship : constant Athena.Handles.Ship.Ship_Handle :=
               Athena.Handles.Ship.Create
                 (Name        => Name,
                  Owner       => Empire,
                  Star        => Star,
                  Design      => Design,
                  Fleet       => Fleet.Reference,
                  Manager     => Manager,
                  Destination => Destination,
                  Script      => Design.Default_Script);
   begin
      Star.Add_Ship (Ship.Reference);
      Empire.Add_Ship (Ship.Reference);
   end Create_Ship;

end Athena.Ships.Create;
