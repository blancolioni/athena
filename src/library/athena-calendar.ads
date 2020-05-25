package Athena.Calendar is

   type Time is private;

   subtype Year_Number  is Integer range 1 .. 5001;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 30;

   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   Day_Length : constant Duration := Day_Duration'Last;

   subtype Hour_Number     is Natural range 0 .. 23;
   subtype Minute_Number   is Natural range 0 .. 59;
   subtype Second_Number   is Natural range 0 .. 59;
   subtype Second_Duration is Day_Duration range 0.0 .. 1.0;

   function Clock return Time;

   function Year    (Date : Time) return Year_Number;
   function Month   (Date : Time) return Month_Number;
   function Day     (Date : Time) return Day_Number;
   function Seconds (Date : Time) return Day_Duration;

   function Hour       (Date : Time) return Hour_Number;
   function Minute     (Date : Time) return Minute_Number;
   function Second     (Date : Time) return Second_Number;
   function Sub_Second (Date : Time) return Second_Duration;

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);

   procedure Split
     (Seconds    : Day_Duration;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);

   procedure Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration);

   procedure Split
     (Date          : Time;
      Period        : Duration;
      Cycle_Count   : out Natural;
      Partial_Cycle : out Duration);

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time;

   function Time_Of
     (Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Hour        : Hour_Number;
      Minute      : Minute_Number;
      Second      : Second_Number;
      Sub_Second  : Second_Duration := 0.0) return Time;

   function Seconds_Of
     (Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number := 0;
      Sub_Second : Second_Duration := 0.0) return Day_Duration;

   function Max (Left, Right : Time) return Time;
   function Min (Left, Right : Time) return Time;

   function Image
     (Date                  : Time;
      Include_Time_Fraction : Boolean := False)
      return String;

   function Image
     (Elapsed_Time          : Duration;
      Include_Time_Fraction : Boolean := False) return String;

   function From_String
     (Image : String)
      return Time;

   function Start_Year return Year_Number;
   function Zero_Time return Time;
   function Start return Time;

   procedure Set_Clock (Clock : Time);
   procedure Advance (Seconds : Duration);

   function "+" (Left : Time;     Right : Duration) return Time;
   function "+" (Left : Duration; Right : Time)     return Time;
   function "-" (Left : Time;     Right : Duration) return Time;
   function "-" (Left : Time;     Right : Time)     return Duration;

   function "<"  (Left, Right : Time) return Boolean;
   function "<=" (Left, Right : Time) return Boolean;
   function ">"  (Left, Right : Time) return Boolean;
   function ">=" (Left, Right : Time) return Boolean;

   function Hours (Count : Natural) return Duration;
   function Days (Count : Natural) return Duration;
   function Years (Count : Natural) return Duration;

   function Days (Count : Non_Negative_Real) return Duration;
   function Delay_Days (Days_Delay : Non_Negative_Real) return Time;

   Time_Error : exception;

private

   type Time is range -2 ** 63 .. 2 ** 63 - 1;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");

   function Sub_Second (Date : Time) return Second_Duration
   is (0.0);

   function "+" (Left : Time;     Right : Duration) return Time
   is (Left + Time (Right));

   function "+" (Left : Duration; Right : Time)     return Time
   is (Right + Time (Left));

   function "-" (Left : Time;     Right : Duration) return Time
   is (Left - Time (Right));

   function "-" (Left : Time;     Right : Time)     return Duration
   is (Duration (Time'(Left - Right)));

   function Days (Count : Natural) return Duration
   is (Duration (Count) * Day_Length);

   function Days (Count : Non_Negative_Real) return Duration
   is (Duration (Count) * Day_Length);

   function Hours (Count : Natural) return Duration
   is (Duration (Count) * 3_600.0);

   function Years (Count : Natural) return Duration
   is (Count * Days (360));

   function Max (Left, Right : Time) return Time
   is (Time'Max (Left, Right));

   function Min (Left, Right : Time) return Time
   is (Time'Min (Left, Right));

   function Delay_Days (Days_Delay : Non_Negative_Real) return Time
   is (Clock + Days (Days_Delay));

   function Zero_Time return Time is (0);

end Athena.Calendar;
