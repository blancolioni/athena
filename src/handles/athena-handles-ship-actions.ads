package Athena.Handles.Ship.Actions is

   function Move_To (Star : Athena.Handles.Star.Star_Handle)
                     return Root_Ship_Action'Class;

   function Load_Cargo
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class;

   function Unload_Cargo
     (Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
      return Root_Ship_Action'Class;

end Athena.Handles.Ship.Actions;
