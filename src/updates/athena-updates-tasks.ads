with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;

with Athena.Calendar;
with Athena.Signals;

private package Athena.Updates.Tasks is

   package Update_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Update_Interface'Class);

   package Update_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Athena.Calendar.Time,
        Element_Type => Update_Lists.List,
        "<"          => Athena.Calendar."<",
        "="          => Update_Lists."=");

   task Update_Task is
      entry Start;
      entry Stop;
      entry Pause;
      entry Resume;
      entry Current_State
        (Is_Paused     : out Boolean;
         Advance_Speed : out Duration);
      entry Set_Speed
        (Advance_Per_Second : Duration);
   end Update_Task;

   task Broadcast_Task is
      entry Broadcast (Signal : Athena.Signals.Signal_Type);
      entry Stop;
   end Broadcast_Task;

   task Dispatch_Task is
      entry Dispatch (List : Update_Lists.List);
      entry Stop;
   end Dispatch_Task;

   protected Update_Map is

      procedure Add_Update
        (Clock  : Athena.Calendar.Time;
         Update : Update_Interface'Class);

      procedure Get_Updates
        (Clock : Athena.Calendar.Time;
         List  : out Update_Lists.List);

   private

      Map : Update_Maps.Map;

   end Update_Map;

end Athena.Updates.Tasks;
