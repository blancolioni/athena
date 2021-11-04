with Tropos;

with Athena.Color;

package Athena.Empires.Create is

   procedure New_Empire
     (Star      : Minerva.Star.Star_Class;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : Athena.Color.Athena_Color;
      Template  : Tropos.Configuration);

end Athena.Empires.Create;
