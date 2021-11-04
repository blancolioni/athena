with Minerva.Star;

with Minerva.Db;

package Athena.Managers.Transportation is

   function Default_Transportation_Manager
     return Athena_Manager_Script'Class;

   function Transport_Message
     (Empire   : Minerva.Empire.Empire_Class;
      From     : Minerva.Star.Star_Class;
      To       : Minerva.Star.Star_Class;
      Cargo    : Minerva.Db.Cargo_Type;
      Quantity : Non_Negative_Real;
      Priority : Integer)
      return Message_Type'Class;

end Athena.Managers.Transportation;
