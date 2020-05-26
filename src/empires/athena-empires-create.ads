with Tropos;

with Athena.Color;

with Athena.Handles.Star;

package Athena.Empires.Create is

   procedure New_Empire
     (Star      : Athena.Handles.Star.Star_Handle;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : Athena.Color.Athena_Color;
      Template  : Tropos.Configuration);

end Athena.Empires.Create;
