with Ada.Characters.Handling;
with Ada.Text_IO;

package body Athena.Reports is

   type Standard_Writer_Type is
     new Writer_Interface with null record;

   overriding procedure Put
     (Writer : in out Standard_Writer_Type;
      Text   : String);

   overriding procedure New_Line
     (Writer : in out Standard_Writer_Type);

   function Make_Heading (Text : String) return String
                          renames Ada.Characters.Handling.To_Upper;

   --------------
   -- Add_Cell --
   --------------

   procedure Add_Cell (Table : in out Report_Table; Text : String) is
      Row : String_Vectors.Vector renames Table.Rows (Table.Rows.Last);
   begin
      Row.Append (Text);
      Table.Widths (Row.Last_Index) :=
        Natural'Max (Table.Widths (Row.Last_Index), Text'Length + 2);
   end Add_Cell;

   -----------------
   -- Add_Heading --
   -----------------

   procedure Add_Heading
     (Table     : in out Report_Table;
      Heading   : String;
      Alignment : Column_Alignment := Left) is
   begin
      Table.Headings.Append (Heading);
      Table.Widths.Append (Heading'Length + 2);
      Table.Alignment.Append (Alignment);
   end Add_Heading;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row (Table : in out Report_Table) is
   begin
      Table.Rows.Append (String_Vectors.Empty_Vector);
   end Add_Row;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Writer : in out Standard_Writer_Type)
   is
   begin
      Ada.Text_IO.New_Line;
   end New_Line;

   overriding procedure Put
     (Writer : in out Standard_Writer_Type;
      Text   : String)
   is
   begin
      Ada.Text_IO.Put (Text);
   end Put;

   -----------------
   -- Put_Heading --
   -----------------

   procedure Put_Heading
     (Writer : in out Writer_Interface'Class;
      Text   : String)
   is
      Dashes : constant String (Text'Range) := (others => '=');
   begin
      Writer.Put_Line (Dashes);
      Writer.Put_Line (Make_Heading (Text));
      Writer.Put_Line (Dashes);
      Writer.New_Line;
   end Put_Heading;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Writer : in out Writer_Interface'Class;
                       Text   : String)
   is
   begin
      Writer.Put (Text);
      Writer.New_Line;
   end Put_Line;

   ---------------------
   -- Standard_Writer --
   ---------------------

   function Standard_Writer return Writer_Interface'Class is
   begin
      return Result : Standard_Writer_Type;
   end Standard_Writer;

   -----------
   -- Write --
   -----------

   procedure Write
     (Table  : Report_Table;
      Writer : in out Writer_Interface'Class)
   is
      Total_Width : Positive := 1;

      procedure Put_Cell
        (Text      : String;
         Width     : Natural;
         Alignment : Column_Alignment);

      procedure Put_Line_Of_Dashes;

      --------------
      -- Put_Cell --
      --------------

      procedure Put_Cell
        (Text      : String;
         Width     : Natural;
         Alignment : Column_Alignment)
      is
         Padding  : constant Integer := Width - Text'Length;
         Pad_Left : constant Natural :=
                      (case Alignment is
                          when Left => (if Padding < 1 then 0 else 1),
                          when Middle => (if Padding < 1
                                          then 0 else Padding / 2),
                          when Right  => (if Padding < 1 then 0
                                          else Padding - 1));
         Pad_Right : constant Natural :=
                       (if Padding < 1 then 0
                        else Padding - Pad_Left);
         Cell_Text : constant String :=
                       (if Width < Text'Length
                        then Text (Text'First .. Text'First + Width - 1)
                        else Text);

         function Spaces (Count : Natural) return String;

         ------------
         -- Spaces --
         ------------

         function Spaces (Count : Natural) return String is
            S : constant String (1 .. Count) := (others => ' ');
         begin
            return S;
         end Spaces;

      begin
         Writer.Put ("|");
         Writer.Put (Spaces (Pad_Left) & Cell_Text & Spaces (Pad_Right));
      end Put_Cell;

      ------------------------
      -- Put_Line_Of_Dashes --
      ------------------------

      procedure Put_Line_Of_Dashes is
         Dashes : constant String (1 .. Total_Width) := (others => '-');
      begin
         Writer.Put_Line (Dashes);
      end Put_Line_Of_Dashes;

   begin
      for Width of Table.Widths loop
         Total_Width := Total_Width + 1 + Width;
      end loop;

      Put_Line_Of_Dashes;

      for I in 1 .. Table.Headings.Last_Index loop
         declare
            Heading : constant String :=
                        Make_Heading (Table.Headings (I));
         begin
            Put_Cell (Heading, Table.Widths (I), Left);
         end;
      end loop;

      Writer.Put_Line ("|");

      Put_Line_Of_Dashes;

      for Row of Table.Rows loop
         for I in 1 .. Row.Last_Index loop
            Put_Cell (Row (I), Table.Widths (I), Table.Alignment (I));
         end loop;
         Writer.Put_Line ("|");
      end loop;

      Put_Line_Of_Dashes;

   end Write;

end Athena.Reports;
