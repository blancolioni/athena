procedure Escape is
    Mass_Vector  : Vector;
    Total_Mass   : Real;
    Have_Hostile : Boolean;
    V            : Vector;
    Signal_Loop  : Address;
    Seek_Vector  : Vector with Address => 16#0100#;
    Seek_Enable  : Integer with Address => 16#0101#;

begin

    loop
        loop
            Have_Hostile := False;
            Mass_Vector  := 0;
            Total_Mass   := 0;
            Signal_Loop  := 16#1000#;

            declare
                Flags    : Bits with Address => Signal_Loop;
                Position : Vector with Address => Signal_Loop + 1;
                Velocity : Vector with Address => Signal_Loop + 2;
                Mass     : Real   with Address => Signal_Loop + 3;
            begin
                if not Flags [256] then
                    exit;
                elsif Flags [2#0010011#] then
                    V := Position + Velocity;
                    V := V * Mass;
                    Mass_Vector := Mass_Vector + V;
                    Total_Mass := Total_Mass + Mass;
                    Have_Hostile := True;
                end if;

                Signal_Loop := Signal_Loop + 16;
                if Signal_Loop = 16#2000# then
                    exit;
                end if;
            end;
        end loop;

        if Have_Hostile then
            V := Mass_Vector / Total_Mass;
            Seek_Vector := V;
            Seek_Enable := 1;
        end if;

    end loop;

end Escape;
