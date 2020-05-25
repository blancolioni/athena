package body Athena.Ships.Updates is

   ---------------------
   -- Execute_Actions --
   ---------------------

   procedure Execute_Actions (Ship : Athena.Handles.Ship.Ship_Handle) is
   begin
      while Ship.Has_Actions loop
         declare
            Action : constant Athena.Handles.Ship.Root_Ship_Action'Class :=
                       Ship.First_Action;
         begin
            if not Action.Execute (Ship) then
               exit;
            end if;
            Ship.Delete_First_Action;
         end;
      end loop;
   end Execute_Actions;

end Athena.Ships.Updates;
