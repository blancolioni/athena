with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Athena.Handles.Design_Module;

package body Athena.Handles.Ship is

   package Ship_Action_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Root_Ship_Action'Class);

   package Module_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Module_Reference);

   type Cargo_Array is array (Cargo_Class) of Non_Negative_Real;

   type Ship_Record is
      record
         Identifier  : Object_Identifier;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Alive       : Boolean;
         Experience  : Non_Negative_Real;
         Managed     : Boolean;
         Star        : Star_Reference;
         Owner       : Empire_Reference;
         Design      : Design_Reference;
         Modules     : Module_Lists.List;
         Drives      : Module_Lists.List;
         Jump_Drive  : Module_Reference;
         Tank_Size   : Non_Negative_Real;
         Cargo_Space : Non_Negative_Real;
         Fleet       : Fleet_Reference;
         Manager     : Athena.Handles.Manager_Class;
         Destination : Star_Reference;
         Carrying    : Cargo_Array;
         Progress    : Unit_Real;
         Actions     : Ship_Action_Lists.List;
         Script      : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Ship_Vectors is
     new Ada.Containers.Vectors
       (Real_Ship_Reference, Ship_Record);

   Vector : Ship_Vectors.Vector;

   overriding function Identifier
     (Ship : Ship_Handle)
      return Object_Identifier
   is (Vector (Ship.Reference).Identifier);

   overriding function Name
     (Ship : Ship_Handle)
      return String
   is (-(Vector (Ship.Reference).Name));

   function Design
     (Ship : Ship_Handle)
      return Athena.Handles.Design.Design_Handle
   is (Athena.Handles.Design.Get (Vector (Ship.Reference).Design));

   function Owner
     (Ship : Ship_Handle)
      return Athena.Handles.Empire.Empire_Handle
   is (Athena.Handles.Empire.Get (Vector (Ship.Reference).Owner));

   function Alive
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Alive);

   function Idle
     (Ship : Ship_Handle)
      return Boolean
   is (not Ship.Has_Destination
       and then Vector (Ship.Reference).Actions.Is_Empty);

   function Has_Manager
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Managed);

   function Manager
     (Ship : Ship_Handle)
      return Manager_Class
   is (Vector (Ship.Reference).Manager);

   function Has_Star_Location
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Progress = 0.0);

   function Has_Deep_Space_Location
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Progress > 0.0);

   function Has_Destination
     (Ship : Ship_Handle)
      return Boolean
   is (Vector (Ship.Reference).Destination /= 0);

   function Origin
     (Ship : Ship_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Ship.Reference).Star));

   function Star_Location
     (Ship : Ship_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Ship.Reference).Star));

   function Destination
     (Ship : Ship_Handle)
      return Athena.Handles.Star.Star_Handle
   is (Athena.Handles.Star.Get (Vector (Ship.Reference).Destination));

   function Progress
     (Ship : Ship_Handle)
      return Unit_Real
   is (Vector (Ship.Reference).Progress);

   function Jump_Drive
     (Ship : Ship_Handle)
      return Athena.Handles.Module.Module_Handle
   is (Athena.Handles.Module.Get (Vector (Ship.Reference).Jump_Drive));

   function Current_Cargo
     (Ship  : Ship_Handle;
      Cargo : Cargo_Class)
      return Non_Negative_Real
   is (Vector (Ship.Reference).Carrying (Cargo));

   function Has_Actions (Ship : Ship_Handle) return Boolean
   is (not Vector (Ship.Reference).Actions.Is_Empty);

   function First_Action (Ship : Ship_Handle) return Root_Ship_Action'Class
   is (Vector (Ship.Reference).Actions.First_Element);

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Ship   : Ship_Handle;
      Action : Root_Ship_Action'Class)
   is
   begin
      Vector (Ship.Reference).Actions.Append (Action);
   end Add_Action;

   --------------------
   -- Add_Experience --
   --------------------

   procedure Add_Experience
     (Ship : Ship_Handle;
      XP   : Non_Negative_Real)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Experience := Rec.Experience + XP;
   end Add_Experience;

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Ship        : Ship_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Destination := 0;
      Rec.Progress := 0.0;
   end Clear_Destination;

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String;
      Star        : Athena.Handles.Star.Star_Handle;
      Owner       : Athena.Handles.Empire.Empire_Handle;
      Design      : Athena.Handles.Design.Design_Handle;
      Fleet       : Fleet_Reference;
      Manager     : Athena.Handles.Manager_Class;
      Destination : Athena.Handles.Star.Star_Handle;
      Script      : String)
      return Ship_Handle
   is
      Rec : Ship_Record :=
              Ship_Record'
                (Identifier    => Next_Identifier,
                 Name          => +Name,
                 Alive         => True,
                 Experience    => 0.0,
                 Managed       => True,
                 Star          => Star.Reference,
                 Owner         => Owner.Reference,
                 Design        => Design.Reference,
                 Modules       => <>,
                 Drives        => <>,
                 Jump_Drive    => Null_Module_Reference,
                 Tank_Size     => Design.Tank_Size,
                 Cargo_Space   => Design.Cargo_Space,
                 Carrying      => (others => 0.0),
                 Fleet         => Fleet,
                 Manager       => Manager,
                 Destination   => Destination.Reference,
                 Progress      => 0.0,
                 Actions       => <>,
                 Script        => +Script);

      procedure Add_Design_Module
        (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle);

      -----------------------
      -- Add_Design_Module --
      -----------------------

      procedure Add_Design_Module
        (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle)
      is
         Module : constant Athena.Handles.Module.Module_Handle :=
                    Athena.Handles.Module.Create (Design_Module);
      begin
         Rec.Modules.Append (Module.Reference);
         if Module.Component.Has_Jump
           and then Rec.Jump_Drive = Null_Module_Reference
         then
            Rec.Jump_Drive := Module.Reference;
         end if;
         if Module.Component.Has_Impulse then
            Rec.Drives.Append (Module.Reference);
         end if;
      end Add_Design_Module;

   begin
      Design.Iterate_Design_Modules (Add_Design_Module'Access);
      Vector.Append (Rec);
      return (True, Vector.Last_Index);
   end Create;

   -------------------------
   -- Delete_First_Action --
   -------------------------

   procedure Delete_First_Action (Ship : Ship_Handle) is
   begin
      Vector (Ship.Reference).Actions.Delete_First;
   end Delete_First_Action;

   -----------------
   -- Iterate_All --
   -----------------

   procedure Iterate_All
     (Process : not null access
        procedure (Ship : Ship_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end Iterate_All;

   -----------------------------
   -- Iterate_Maneuver_Drives --
   -----------------------------

   procedure Iterate_Maneuver_Drives
     (Ship    : Ship_Handle;
      Process : not null access
        procedure (Maneuver : Athena.Handles.Module.Module_Handle))
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      for Drive of Rec.Drives loop
         Process (Athena.Handles.Module.Get (Drive));
      end loop;
   end Iterate_Maneuver_Drives;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Ship_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Ship_Vectors.Vector'Write (Stream, Vector);
   end Save;

   -----------------------
   -- Set_Current_Cargo --
   -----------------------

   procedure Set_Current_Cargo
     (Ship     : Ship_Handle;
      Cargo    : Cargo_Class;
      Quantity : Non_Negative_Real)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Carrying (Cargo) := Quantity;
   end Set_Current_Cargo;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship        : Ship_Handle;
      Destination : Athena.Handles.Star.Star_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Destination := Destination.Reference;
      Rec.Progress := 0.0;
   end Set_Destination;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (Ship     : Ship_Handle;
      New_Name : String)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Name := +New_Name;
   end Set_Name;

   ------------------
   -- Set_Progress --
   ------------------

   procedure Set_Progress
     (Ship     : Ship_Handle;
      Progress : Unit_Real)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Progress := Progress;
   end Set_Progress;

   -----------------------
   -- Set_Star_Location --
   -----------------------

   procedure Set_Star_Location
     (Ship : Ship_Handle;
      Star : Athena.Handles.Star.Star_Handle)
   is
      Rec : Ship_Record renames Vector (Ship.Reference);
   begin
      Rec.Star := Star.Reference;
   end Set_Star_Location;

end Athena.Handles.Ship;
