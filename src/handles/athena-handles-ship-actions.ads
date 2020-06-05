with Athena.Cargo;

package Athena.Handles.Ship.Actions is

   procedure Move_To
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
     with Pre => Star.Has_Element;

   function Load_Cargo
     (Cargo    : Athena.Cargo.Cargo_Container)
      return Root_Ship_Action'Class;

   function Unload_Cargo
     (Cargo    : Athena.Cargo.Cargo_Container)
      return Root_Ship_Action'Class;

end Athena.Handles.Ship.Actions;
