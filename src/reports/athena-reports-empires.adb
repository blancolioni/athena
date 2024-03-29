with Athena.Calendar;
with Athena.Money;

with Athena.Ships;

with Athena.Handles.Colony;
with Athena.Handles.Ship;

package body Athena.Reports.Empires is

   function Colony_Report
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Report_Table;

   function Ship_Report
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Report_Table;

   -------------------
   -- Colony_Report --
   -------------------

   function Colony_Report
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Report_Table
   is
      Table : Report_Table;

      procedure Add_Colony (Ref : Athena.Handles.Colony_Reference);

      ----------------
      -- Add_Colony --
      ----------------

      procedure Add_Colony (Ref : Athena.Handles.Colony_Reference) is
         Colony : constant Athena.Handles.Colony.Colony_Handle :=
                    Athena.Handles.Colony.Get (Ref);
      begin
         Add_Row (Table);
         Add_Cell (Table, Colony.Star.Name);
         Add_Cell (Table, Athena.Calendar.Image (Colony.Founded));
         Add_Cell (Table, Image (Real (Colony.Star.Space)));
         Add_Cell (Table, Image (Colony.Star.Resource * 100.0));
         Add_Cell (Table, Image (Colony.Star.Habitability * 100.0));
         Add_Cell (Table, Image (Colony.Population));
         Add_Cell (Table, Image (Colony.Employed));
         Add_Cell (Table, Image (Colony.Industry));
         Add_Cell (Table, Image (Colony.Material));
      end Add_Colony;

   begin
      Add_Heading (Table, "star");
      Add_Heading (Table, "founded");
      Add_Heading (Table, "space");
      Add_Heading (Table, "res");
      Add_Heading (Table, "hab");
      Add_Heading (Table, "pop");
      Add_Heading (Table, "emp");
      Add_Heading (Table, "ind");
      Add_Heading (Table, "mat");
      Empire.Iterate_Colonies (Add_Colony'Access);
      return Table;
   end Colony_Report;

   ------------
   -- Report --
   ------------

   procedure Report
     (Writer : in out Writer_Interface'Class;
      Empire :        Athena.Handles.Empire.Empire_Handle)
   is
   begin
      Writer.Put_Heading (Empire.Name);

      Writer.Put_Line
        ("Date: " & Athena.Calendar.Image (Athena.Calendar.Clock));
      Writer.Put_Line
        ("Cash: " & Athena.Money.Show (Empire.Cash));
      Writer.Put_Line
        ("Debt: " & Athena.Money.Show (Empire.Debt));
      Writer.New_Line;

      Write (Colony_Report (Empire), Writer);
      Write (Ship_Report (Empire), Writer);

   end Report;

   -----------------
   -- Ship_Report --
   -----------------

   function Ship_Report
     (Empire : Athena.Handles.Empire.Empire_Handle)
      return Report_Table
   is
      Table : Report_Table;

      procedure Add_Ship (Ref : Athena.Handles.Ship_Reference);

      ----------------
      -- Add_Colony --
      ----------------

      procedure Add_Ship (Ref : Athena.Handles.Ship_Reference) is
         Ship : constant Athena.Handles.Ship.Ship_Handle :=
                    Athena.Handles.Ship.Get (Ref);
      begin
         Add_Row (Table);
         Add_Cell (Table, Ship.Name);
         Add_Cell (Table, Ship.Design.Name);
         if Ship.Has_Deep_Space_Location then
            Add_Cell (Table, Ship.Origin.Name);
         else
            Add_Cell (Table, Ship.Star_Location.Name);
         end if;
         if Ship.Has_Destination then
            Add_Cell (Table, Ship.Destination.Name);
         else
            Add_Cell (Table, "");
         end if;

         Add_Cell (Table, Ship.Current_Activity);
         Add_Cell (Table, Image (Ship.Progress * 100.0) & "%");
         Add_Cell (Table, Image (Athena.Ships.Tonnage (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Mass (Ship)));
         Add_Cell (Table, Athena.Money.Show
                   (Athena.Ships.Get_Maintenance_Cost (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Get_Jump_Speed (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Get_Impulse_Speed (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Available_Power (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Idle_Power (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Drive_Power (Ship)));
         Add_Cell (Table, Image (Athena.Ships.Jump_Power (Ship)));
         Add_Cell (Table, Image (Ship.Design.Tank_Size));
         Add_Cell (Table, Image (Ship.Design.Free_Space));
         Add_Cell (Table, Image (Ship.Design.Passenger_Berths));
      end Add_Ship;

   begin
      Add_Heading (Table, "name");
      Add_Heading (Table, "design");
      Add_Heading (Table, "location");
      Add_Heading (Table, "destination");
      Add_Heading (Table, "activity");
      Add_Heading (Table, "progress");
      Add_Heading (Table, "tons");
      Add_Heading (Table, "mass");
      Add_Heading (Table, "mnt");
      Add_Heading (Table, "jmp");
      Add_Heading (Table, "imp");
      Add_Heading (Table, "power");
      Add_Heading (Table, "idle");
      Add_Heading (Table, "drives");
      Add_Heading (Table, "jump");
      Add_Heading (Table, "tank");
      Add_Heading (Table, "cargo");
      Add_Heading (Table, "berths");
      Empire.Iterate_Ships (Add_Ship'Access);
      return Table;
   end Ship_Report;

end Athena.Reports.Empires;
