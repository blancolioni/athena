with Athena.Logging;
with Athena.Managers;

package body Athena.Empires.Managers is

   -----------------
   -- Run_Manager --
   -----------------

   procedure Run_Manager
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class;
      Name    : String)
   is

   begin
      if Athena.Managers.Exists (Manager.Tag, Name) then
         Athena.Logging.Log (Empire.Name
                             & ": executing manager: "
                             & Manager.Tag
                             & "/"
                             & Name);
         declare
            Script : constant Athena.Managers.Athena_Manager_Script'Class :=
                       Athena.Managers.Get_Manager
                         (Manager.Tag, Name, Empire);
         begin
            Script.Create_Orders;
         end;
      else
         Athena.Logging.Log
           (Empire.Name
            & ": no such manager: "
            & Manager.Tag
            & "/"
            & Name);
      end if;
   end Run_Manager;

end Athena.Empires.Managers;
