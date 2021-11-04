with Minerva.Turn;

package Athena.Turns is

   function Current_Turn return Natural;
   function Current_Turn_Image return String;
   function Current_Turn return Minerva.Turn.Turn_Class;

   procedure Next_Turn;

   function Get_Turn (Index : Positive) return Minerva.Turn.Turn_Class;

end Athena.Turns;
