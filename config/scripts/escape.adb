procedure Escape is
   Mass_Vector  : Vector;
   Mass         : Real;
   Have_Hostile : Boolean := False;
begin
   for Signal of Situation loop
      if Signal.Hostile then 
         Mass         := Mass + Signal.Mass;
         Mass_Vector  := Mass_Vector + Signal.Mass * (Signal.Position + Signal.Velocity);
         Have_Hostile := True;
      end if;
   end loop;

   if Have_Hostile then 
      Seek (-Mass_Vector / Mass);
   end if;

end Escape;
