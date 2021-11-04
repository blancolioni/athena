with Minerva.Empire;
with Minerva.War;

package Athena.Treaties is

   function At_War
     (Empire_1, Empire_2 : Minerva.Empire.Empire_Class)
      return Boolean;

   function Get_War
     (Empire_1, Empire_2 : Minerva.Empire.Empire_Class)
      return Minerva.War.War_Class;

   procedure Declare_War
     (Aggressor, Defender : Minerva.Empire.Empire_Class);

end Athena.Treaties;
