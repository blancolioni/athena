with Athena.Updates.Tasks;

package body Athena.Updates.Events is

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Clock  : Athena.Calendar.Time;
      Update : Update_Interface'Class)
   is
   begin
      Athena.Updates.Tasks.Update_Map.Add_Update (Clock, Update);
   end Update_At;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class)
   is
      use type Athena.Calendar.Time;
   begin
      Athena.Updates.Tasks.Update_Map.Add_Update
        (Athena.Calendar.Clock + Wait, Update);
   end Update_With_Delay;

end Athena.Updates.Events;
