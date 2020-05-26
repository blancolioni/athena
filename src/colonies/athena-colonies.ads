with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Star;

package Athena.Colonies is

   procedure Produce_Material
     (Colony   : Athena.Handles.Colony.Colony_Handle'Class;
      Quantity : Non_Negative_Real);

   function Can_Provide
     (Colony      : Athena.Handles.Colony.Colony_Handle;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0)
      return Boolean;

   procedure Use_Assets
     (Colony      : Athena.Handles.Colony.Colony_Handle;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0;
      Description : String);

   procedure For_All_Colonies
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      Process  : not null access
        procedure (Colony : Athena.Handles.Colony.Colony_Handle));

   function Find_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      Test     : not null access
        function (Colony : Athena.Handles.Colony.Colony_Handle) return Boolean)
      return Athena.Handles.Colony.Colony_Handle;

   function Best_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      Score    : not null access
        function (Colony : Athena.Handles.Colony.Colony_Handle) return Real)
      return Athena.Handles.Colony.Colony_Handle;

   function Nearest_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Handle;
      To_Star  : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Colony.Colony_Handle;

   function Get_Colony
     (At_Star : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Colony.Colony_Handle;

   procedure Capture_Colony
     (Colony      : Athena.Handles.Colony.Colony_Handle;
      Captured_By : Athena.Handles.Empire.Empire_Handle);

   function New_Colony
     (At_Star  : Athena.Handles.Star.Star_Handle;
      Owner    : Athena.Handles.Empire.Empire_Handle;
      Pop      : Non_Negative_Real;
      Industry : Non_Negative_Real;
      Material : Non_Negative_Real)
      return Athena.Handles.Colony.Colony_Handle;

end Athena.Colonies;
