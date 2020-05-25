with Athena.Handles.Colony;
with Athena.Handles.Star;

package Athena.Stars is

   function Distance
     (From, To : Athena.Handles.Star.Star_Handle)
      return Non_Negative_Real;

   function Get_Colony
     (Of_Star : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Colony.Colony_Handle;

   procedure Set_Colony
     (Star   : Athena.Handles.Star.Star_Handle;
      Colony : Athena.Handles.Colony.Colony_Handle);

   procedure Iterate_Nearest
     (To_Star   : Athena.Handles.Star.Star_Handle;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Star : Athena.Handles.Star.Star_Handle;
                   Distance : Non_Negative_Real));

   function Find_Star
     (With_Name : String)
      return Athena.Handles.Star.Star_Handle;

end Athena.Stars;
