package Athena.Handles.Ship.Actions is

   procedure Move_To
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
     with Pre => Star.Has_Element;

   function Load_Cargo
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class;

   function Unload_Cargo
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class;

   function Fill_Cargo
     (Cargo    : Cargo_Class)
      return Root_Ship_Action'Class;

   function Empty_Cargo
     (Cargo    : Cargo_Class)
      return Root_Ship_Action'Class;

end Athena.Handles.Ship.Actions;
