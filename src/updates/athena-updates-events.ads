with Athena.Calendar;

package Athena.Updates.Events is

   procedure Update_At
     (Clock  : Athena.Calendar.Time;
      Update : Update_Interface'Class);

   procedure Update_With_Delay
     (Wait   : Duration;
      Update : Update_Interface'Class);

end Athena.Updates.Events;
