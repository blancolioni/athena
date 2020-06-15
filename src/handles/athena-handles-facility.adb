with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with WL.String_Maps;

with Athena.Logging;

package body Athena.Handles.Facility is

   type Input_Record is
      record
         Commodity : Commodity_Reference;
         Quantity  : Non_Negative_Real;
      end record;

   type Constraint_Type is (Zone, Habitability, Factor);

   type Constraint_Record (Constraint : Constraint_Type) is
      record
         case Constraint is
            when Zone =>
               Zone_Constraint : Zone_Type;
            when Habitability =>
               null;
            when Factor =>
               Factor_Constraint : Real;
         end case;
      end record;

   package Input_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Input_Record);

   package Constraint_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Constraint_Record);

   type Facility_Record is
      record
         Identifier  : Object_Identifier;
         Tag         : Ada.Strings.Unbounded.Unbounded_String;
         Employees   : Non_Negative_Real;
         Output      : Commodity_Reference;
         Quantity    : Non_Negative_Real;
         Inputs      : Input_Lists.List;
         Constraints : Constraint_Lists.List;
      end record;

   package Facility_Vectors is
     new Ada.Containers.Vectors
       (Real_Facility_Reference, Facility_Record);

   package Facility_Maps is
     new WL.String_Maps (Real_Facility_Reference);

   Vector : Facility_Vectors.Vector;
   Map    : Facility_Maps.Map;

   function Output_Commodity
     (Handle : Facility_Handle)
      return Athena.Handles.Commodity.Commodity_Handle
   is (Athena.Handles.Commodity.Get (Vector (Handle.Reference).Output));

   function Output_Quantity
     (Handle : Facility_Handle)
      return Non_Negative_Real
   is (Vector (Handle.Reference).Quantity);

   function Employment (Handle : Facility_Handle) return Non_Negative_Real
   is (Vector (Handle.Reference).Employees);

   --------------------------------
   -- Add_Agriculture_Constraint --
   --------------------------------

   procedure Add_Agriculture_Constraint
     (Handle : Facility_Handle)
   is
      Rec : Facility_Record renames Vector (Handle.Reference);
   begin
      Rec.Constraints.Append
        (Constraint_Record'
           (Constraint        => Zone,
            Zone_Constraint   => Agricultural));
   end Add_Agriculture_Constraint;

   -----------------------------
   -- Add_Constant_Constraint --
   -----------------------------

   procedure Add_Constant_Constraint
     (Handle : Facility_Handle;
      Value  : Non_Negative_Real)
   is
      Rec : Facility_Record renames Vector (Handle.Reference);
   begin
      Rec.Constraints.Append
        (Constraint_Record'
           (Constraint        => Factor,
            Factor_Constraint => Value));
   end Add_Constant_Constraint;

   ---------------------------------
   -- Add_Habitability_Constraint --
   ---------------------------------

   procedure Add_Habitability_Constraint
     (Handle : Facility_Handle)
   is
      Rec : Facility_Record renames Vector (Handle.Reference);
   begin
      Rec.Constraints.Append
        (Constraint_Record'
           (Constraint        => Habitability));
   end Add_Habitability_Constraint;

   ---------------
   -- Add_Input --
   ---------------

   procedure Add_Input
     (Handle    : Facility_Handle;
      Commodity : Athena.Handles.Commodity.Commodity_Handle'Class;
      Quantity  : Non_Negative_Real)
   is
      Rec : Facility_Record renames Vector (Handle.Reference);
   begin
      Rec.Inputs.Append (Input_Record'
                           (Commodity => Commodity.Reference,
                            Quantity  => Quantity));
   end Add_Input;

   ---------------------
   -- All_Facilities --
   ---------------------

   function All_Facilities return Facility_Array is
   begin
      return Result : Facility_Array (1 .. Natural (Vector.Last_Index)) do
         for I in Result'Range loop
            Result (I) := Get (Facility_Reference (I));
         end loop;
      end return;
   end All_Facilities;

   ------------
   -- Create --
   ------------

   function Create
     (Tag       : String;
      Commodity : Athena.Handles.Commodity.Commodity_Handle;
      Quantity  : Non_Negative_Real;
      Employees : Non_Negative_Real)
      return Facility_Handle
   is
   begin
      Vector.Append
        (Facility_Record'
           (Identifier => Next_Identifier,
            Tag        => +Tag,
            Employees   => Employees,
            Output      => Commodity.Reference,
            Quantity    => Quantity,
            Inputs      => <>,
            Constraints => <>));

      Map.Insert (Tag, Vector.Last_Index);

      return (True, Vector.Last_Index);
   end Create;

   ----------------------
   -- Daily_Production --
   ----------------------

   procedure Daily_Production
     (Handle    : Facility_Handle;
      Id        : Object_Identifier;
      Size      : Non_Negative_Real;
      Context   : Production_Context'Class;
      Employees : Non_Negative_Real;
      Stock     : Athena.Handles.Commodity.Stock_Interface'Class)
   is
      Max : Non_Negative_Real := Size;
      Rec : Facility_Record renames Vector (Handle.Reference);

      procedure Log (Message : String);

      ---------
      -- Log --
      ---------

      procedure Log (Message : String) is
      begin
         Athena.Logging.Log
           (Id & " " & Handle.Short_Name & ": "
            & Message);
      end Log;

   begin

      Log ("starting production: size = " & Image (Size));

      for Input of Rec.Inputs loop
         declare
            Item : constant Athena.Handles.Commodity.Commodity_Handle :=
                     Athena.Handles.Commodity.Get (Input.Commodity);
            Required : constant Non_Negative_Real :=
                         Size * Input.Quantity;
            Available : constant Non_Negative_Real :=
                          Stock.Get_Stock (Item);
            This_Max  : constant Non_Negative_Real :=
                          (if Available >= Required
                           then Size else Size * Available / Required);
         begin
            Log ("input: " & Item.Tag & ": available "
                 & Image (Available)
                 & "; required "
                 & Image (Required)
                 & "; max "
                 & Image (This_Max));

            Max := Real'Min (Max, This_Max);
         end;
      end loop;

      declare
         Required_Workers : constant Non_Negative_Real :=
                              Handle.Employment * Size;
         Employment_Max   : constant Non_Negative_Real :=
                              (if Employees < Required_Workers
                               then Size * Employees / Required_Workers
                               else Size);
      begin
         Log
           ("workers: available "
            & Image (Employees)
            & "; required "
            & Image (Required_Workers)
            & "; max "
            & Image (Employment_Max));

         Max := Real'Min (Max, Employment_Max);
      end;

      Log
        ("calculated maximum production: " & Image (Max));

      for Input of Rec.Inputs loop
         declare
            Item      : constant Athena.Handles.Commodity.Commodity_Handle :=
              Athena.Handles.Commodity.Get (Input.Commodity);
            Available : constant Non_Negative_Real :=
              Stock.Get_Stock (Item);
            Required  : constant Non_Negative_Real :=
              Real'Min (Max * Input.Quantity, Available);
         begin

            Log
              ("consuming " & Image (Required) & " " & Item.Tag);
            Stock.Remove_Stock (Item, Required);
         end;
      end loop;

      declare
         Quantity     : Non_Negative_Real := Max * Rec.Quantity;
      begin
         Log ("output: base quantity " & Image (Quantity));

         for Constraint of Rec.Constraints loop
            case Constraint.Constraint is
               when Zone =>
                  null;
               when Habitability =>
                  Quantity := Quantity * Context.Habitability;
                  Log ("habitability "
                       & Image (Context.Habitability * 100.0)
                       & "%; quantity now " & Image (Quantity));

               when Factor =>
                  Quantity := Quantity * Constraint.Factor_Constraint;

                  Log ("factor "
                       & Image (Constraint.Factor_Constraint)
                       & "% quantity now " & Image (Quantity));
            end case;
         end loop;

         declare
            use type Athena.Handles.Commodity.Commodity_Class;
            Output : constant Athena.Handles.Commodity.Commodity_Handle :=
              Athena.Handles.Commodity.Get (Rec.Output);
         begin
            if Output.Class = Athena.Handles.Commodity.Resource then
               for Item of Context.Available_Resources loop
                  Stock.Add_Stock
                    (Item, Context.Extract_Resource (Item, Quantity));
               end loop;
            else
               Stock.Add_Stock (Output, Quantity);
            end if;
         end;
      end;

   end Daily_Production;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   ----------------
   -- Get_By_Tag --
   ----------------

   function Get_By_Tag (Tag : String) return Facility_Handle is
   begin
      pragma Assert (Map.Contains (Tag), "unknown Facility tag: " & Tag);
      return Get (Map.Element (Tag));
   end Get_By_Tag;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Facility_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Tag), I);
      end loop;
   end Load;

   -----------------------
   -- Required_Quantity --
   -----------------------

   function Required_Quantity
     (Handle    : Facility_Handle;
      Size      : Non_Negative_Real;
      Commodity : Athena.Handles.Commodity.Commodity_Handle)
      return Non_Negative_Real
   is
      Rec : Facility_Record renames Vector (Handle.Reference);
   begin
      for Input of Rec.Inputs loop
         if Input.Commodity = Commodity.Reference then
            return Size * Input.Quantity;
         end if;
      end loop;
      return 0.0;
   end Required_Quantity;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Facility_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------
   -- Tag --
   ---------

   overriding function Tag (Facility : Facility_Handle) return String is
   begin
      return -(Vector (Facility.Reference).Tag);
   end Tag;

end Athena.Handles.Facility;
