with Athena.Elementary_Functions;

package body Athena.Stars is

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Athena.Handles.Star.Star_Handle)
      return Non_Negative_Real
   is
      use Athena.Elementary_Functions;
   begin
      return Sqrt ((From.X - To.X) ** 2 + (From.Y - To.Y) ** 2);
   end Distance;

   ---------------
   -- Find_Star --
   ---------------

   function Find_Star
     (With_Name : String)
      return Athena.Handles.Star.Star_Handle
   is
   begin
      return Athena.Handles.Star.Get_By_Name (With_Name);
   end Find_Star;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (Of_Star : Athena.Handles.Star.Star_Handle)
      return Athena.Handles.Colony.Colony_Handle
   is
   begin
      return Athena.Handles.Colony.Get (Of_Star.Colony);
   end Get_Colony;

   ---------------------
   -- Iterate_Nearest --
   ---------------------

   procedure Iterate_Nearest
     (To_Star   : Athena.Handles.Star.Star_Handle;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Star : Athena.Handles.Star.Star_Handle;
                   Distance : Non_Negative_Real))
   is
      function Local_Process
        (Star : Athena.Handles.Star.Star_Handle)
         return Boolean;

      -------------------
      -- Local_Process --
      -------------------

      function Local_Process
        (Star : Athena.Handles.Star.Star_Handle)
         return Boolean
      is
      begin
         Process (Star, Distance (Star, To_Star));
         return True;
      end Local_Process;

   begin
      Athena.Handles.Star.Iterate_Nearest_Stars
        (To_Star, Max_Range, Local_Process'Access);
   end Iterate_Nearest;

   ----------------
   -- Set_Colony --
   ----------------

   procedure Set_Colony
     (Star   : Athena.Handles.Star.Star_Handle;
      Colony : Athena.Handles.Colony.Colony_Handle)
   is
   begin
      Star.Set_Colony (Colony.Reference);
      Star.Set_Owner (Colony.Owner.Reference);
   end Set_Colony;

end Athena.Stars;
