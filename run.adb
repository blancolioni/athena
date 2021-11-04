with HAL; use HAL;

procedure Run is

   procedure Usage is
   begin
      Put_Line ("Usage: hac run [ clean | restore ] update [update-count]");
      Set_Exit_Status (1);
   end Usage;

   Next_Arg_Index : Natural := 0;

   function Next_Argument return VString is
   begin
      if Next_Arg_Index < Argument_Count then 
         Next_Arg_Index := Next_Arg_Index + 1;  
         return Argument (Next_Arg_Index);
      else
         return To_VString ("");
      end if;
   end Next_Argument;

   Arg : VString;

begin
   if Argument_Count = 0 then
      Usage;
      return;
   end if;

   Arg := Next_Argument;

   while Arg /= "" loop      
      if Arg = "restore" then
         Copy_File ("athena-init.marlowe", "athena.marlowe");
         Put_Line ("database restored");
      elsif Arg = "update" then
         Arg := Next_Argument;
         if Arg = "" then
            Shell_Execute ("./build/bin/athena-driver --update");
         else
            Shell_Execute ("./build/bin/athena-driver --update --update-count=" & Arg);
         end if;
         return;
      else
         Usage;
         return;
      end if;
      Arg := Next_Argument;
   end loop;
   
end Run;
