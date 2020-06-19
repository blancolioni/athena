with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.World;
with Athena.Handles.Design;

package Athena.Ships.Create is

   procedure Create_Ship
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      World       : Athena.Handles.World.World_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Name        : String;
      Fleet       : Athena.Handles.Fleet.Fleet_Handle;
      Manager     : Athena.Handles.Manager_Class;
      Destination : Athena.Handles.World.World_Handle :=
        Athena.Handles.World.Empty_Handle);

end Athena.Ships.Create;
