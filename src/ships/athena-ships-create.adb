with Athena.Handles.Ship.Actions;

package body Athena.Ships.Create is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      World       : Athena.Handles.World.World_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Name        : String;
      Fleet       : Athena.Handles.Fleet.Fleet_Handle;
      Manager     : Athena.Handles.Manager_Class;
      Destination : Athena.Handles.World.World_Handle :=
        Athena.Handles.World.Empty_Handle)
   is
      Ship : constant Athena.Handles.Ship.Ship_Handle :=
               Athena.Handles.Ship.Create
                 (Name        => Name,
                  Owner       => Empire,
                  World       => World,
                  Design      => Design,
                  Fleet       => Fleet.Reference,
                  Manager     => Manager,
                  Script      => Design.Default_Script);
   begin
      World.Add_Ship (Ship.Reference);
      World.Star.Add_Ship (Ship.Reference);
      Empire.Add_Ship (Ship.Reference);
      if Destination.Has_Element then
         Athena.Handles.Ship.Actions.Move_To_World
           (Ship, Destination);
      end if;
   end Create_Ship;

end Athena.Ships.Create;
