with Tropos;

with Athena.Color;

with Athena.Handles.World;

package Athena.Empires.Create is

   procedure New_Empire
     (World     : Athena.Handles.World.World_Handle;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : Athena.Color.Athena_Color;
      Template  : Tropos.Configuration);

end Athena.Empires.Create;
