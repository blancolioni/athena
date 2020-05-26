with Athena.Calendar;
with Athena.Updates.Tasks;            use Athena.Updates.Tasks;

package body Athena.Updates.Control is

   Start_Time : Ada.Calendar.Time;

   -----------------------------
   -- Execute_Pending_Updates --
   -----------------------------

   procedure Execute_Pending_Updates is
      List  : Update_Lists.List;
      Clock : constant Athena.Calendar.Time :=
        Athena.Calendar.Clock;
   begin
      Update_Map.Get_Updates (Clock, List);

      if not List.Is_Empty then
         Dispatch_Task.Dispatch (List);
      end if;
   end Execute_Pending_Updates;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Start_Time         : out Ada.Calendar.Time;
      Paused             : out Boolean;
      Advance_Per_Second : out Duration)
   is
   begin
      Start_Time := Athena.Updates.Control.Start_Time;
      Update_Task.Current_State (Paused, Advance_Per_Second);
   end Get_Status;

   ---------------
   -- Is_Paused --
   ---------------

   function Is_Paused return Boolean is
      Paused             : Boolean;
      Advance_Per_Second : Duration;
   begin
      Update_Task.Current_State (Paused, Advance_Per_Second);
      return Paused;
   end Is_Paused;

   -------------------
   -- Pause_Updates --
   -------------------

   procedure Pause_Updates is
      Is_Paused : Boolean;
      Advance   : Duration;
   begin
      Update_Task.Current_State (Is_Paused, Advance);
      if not Is_Paused then
         Update_Task.Pause;
      end if;
   end Pause_Updates;

   --------------------
   -- Resume_Updates --
   --------------------

   procedure Resume_Updates is
      Is_Paused : Boolean;
      Advance   : Duration;
   begin
      Update_Task.Current_State (Is_Paused, Advance);
      if Is_Paused then
         Update_Task.Resume;
      end if;
   end Resume_Updates;

   -----------------------
   -- Set_Advance_Speed --
   -----------------------

   procedure Set_Advance_Speed
     (Advance_Per_Second : Duration)
   is
   begin
      Update_Task.Set_Speed (Advance_Per_Second);
   end Set_Advance_Speed;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates is
   begin
      Start_Time := Ada.Calendar.Clock;
      Update_Task.Start;
   end Start_Updates;

   ------------------
   -- Stop_Updates --
   ------------------

   procedure Stop_Updates is
   begin
      Update_Task.Stop;
      Dispatch_Task.Stop;
      Broadcast_Task.Stop;
   end Stop_Updates;

end Athena.Updates.Control;
