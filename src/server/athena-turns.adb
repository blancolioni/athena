package body Athena.Turns is

   ------------------
   -- Current_Turn --
   ------------------

   function Current_Turn return Natural is
   begin
      return Minerva.Turn.First_By_Current (True).Turn_Number;
   end Current_Turn;

   ------------------
   -- Current_Turn --
   ------------------

   function Current_Turn return Minerva.Turn.Turn_Class is
   begin
      return Minerva.Turn.First_By_Current (True);
   end Current_Turn;

   ------------------------
   -- Current_Turn_Image --
   ------------------------

   function Current_Turn_Image return String is
      S : constant String := Natural'Image (Current_Turn);
   begin
      return S (S'First + 1 .. S'Last);
   end Current_Turn_Image;

   --------------
   -- Get_Turn --
   --------------

   function Get_Turn (Index : Positive) return Minerva.Turn.Turn_Class is
   begin
      return Minerva.Turn.Get_By_Turn_Number (Index);
   end Get_Turn;

   ---------------
   -- Next_Turn --
   ---------------

   procedure Next_Turn is
      Old_Turn : constant Minerva.Turn.Turn_Class :=
                   Minerva.Turn.First_By_Current (True);
   begin
      Minerva.Turn.Create
        (Turn_Number => Old_Turn.Turn_Number + 1,
         Current     => True);
      Old_Turn.Update_Turn
        .Set_Current (False)
        .Done;
   end Next_Turn;

end Athena.Turns;
