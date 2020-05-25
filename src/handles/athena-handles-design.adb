with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with WL.String_Maps;

package body Athena.Handles.Design is

   package Design_Module_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Design_Module_Reference);

   type Design_Record is
      record
         Name           : Ada.Strings.Unbounded.Unbounded_String;
         Owner          : Empire_Reference;
         Hull           : Hull_Reference;
         Armor          : Hull_Armor_Reference;
         Armor_Points   : Positive;
         Tonnage        : Non_Negative_Real;
         Hull_Points    : Non_Negative_Real;
         Tank_Size      : Non_Negative_Real;
         Cargo_Space    : Non_Negative_Real;
         Firm_Points    : Natural;
         Hard_Points    : Natural;
         Default_Script : Ada.Strings.Unbounded.Unbounded_String;
         Default_Rank   : Positive;
         Design_Modules : Design_Module_Lists.List;
      end record;

   package Design_Vectors is
     new Ada.Containers.Vectors
       (Real_Design_Reference, Design_Record);

   package Design_Maps is
     new WL.String_Maps (Real_Design_Reference);

   Vector : Design_Vectors.Vector;
   Map : Design_Maps.Map;

   function Name
     (Handle : Design_Handle)
      return String
   is (-(Vector (Handle.Reference).Name));

   overriding function Short_Name
     (Design : Design_Handle)
      return String
   is (-(Vector (Design.Reference).Name));

   function Hard_Points
     (Handle : Design_Handle)
      return Natural
   is (Vector (Handle.Reference).Hard_Points);

   function Firm_Points
     (Handle : Design_Handle)
      return Natural
   is (Vector (Handle.Reference).Firm_Points);

   function Cargo_Space
     (Handle : Design_Handle)
      return Non_Negative_Real
   is (Vector (Handle.Reference).Cargo_Space);

   function Tank_Size
     (Handle : Design_Handle)
      return Non_Negative_Real
   is (Vector (Handle.Reference).Tank_Size);

   function Tonnage
     (Handle : Design_Handle)
      return Non_Negative_Real
   is (Vector (Handle.Reference).Tonnage);

   function Default_Script
     (Handle : Design_Handle)
      return String
   is (-(Vector (Handle.Reference).Default_Script));

   function Get_By_Name
     (Name : String)
      return Design_Handle
   is (if Map.Contains (Name) then Get (Map.Element (Name)) else (False, 0));

   -----------------------
   -- Add_Design_Module --
   -----------------------

   procedure Add_Design_Module
     (To_Design     : Design_Handle;
      Design_Module : Athena.Handles.Design_Module.Design_Module_Handle)
   is
      Rec : Design_Record renames Vector (To_Design.Reference);
   begin
      Rec.Design_Modules.Append (Design_Module.Reference);
      Rec.Cargo_Space := Rec.Cargo_Space - Design_Module.Component.Tonnage;
   end Add_Design_Module;

   ------------
   -- Create --
   ------------

   function Create
     (Name           : String;
      Owner          : Athena.Handles.Empire.Empire_Handle;
      Hull           : Athena.Handles.Hull.Hull_Handle;
      Armor          : Athena.Handles.Hull_Armor.Hull_Armor_Handle;
      Armor_Points   : Positive;
      Tonnage        : Non_Negative_Real;
      Hull_Points    : Non_Negative_Real;
      Fuel_Tank      : Non_Negative_Real;
      Firm_Points    : Natural;
      Hard_Points    : Natural;
      Default_Script : String;
      Default_Rank   : Positive)
      return Design_Handle
   is
   begin
      Vector.Append
        (Design_Record'
           (Name           => +Name,
            Owner          => Owner.Reference,
            Hull           => Hull.Reference,
            Armor          => Armor.Reference,
            Armor_Points   => Armor_Points,
            Tonnage        => Tonnage,
            Hull_Points    => Hull_Points,
            Tank_Size      => Fuel_Tank,
            Cargo_Space    =>
              Tonnage - Fuel_Tank
            - Real (Armor_Points) * Armor.Tonnage_Fraction * Tonnage,
            Firm_Points    => Firm_Points,
            Hard_Points    => Hard_Points,
            Default_Script => +Default_Script,
            Default_Rank   => Default_Rank,
            Design_Modules => <>));
      Map.Insert (Name, Vector.Last_Index);

      Ada.Text_IO.Put_Line ("new design: " & Name);

      return Get (Vector.Last_Index);
   end Create;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Design : Design_Handle))
   is
   begin
      for Reference in 1 .. Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end Iterate;

   ----------------------------
   -- Iterate_Design_Modules --
   ----------------------------

   procedure Iterate_Design_Modules
     (Design  : Design_Handle;
      Process : not null access
        procedure
          (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle))
   is
   begin
      for Module of
        Vector (Design.Reference).Design_Modules
      loop
         Process (Athena.Handles.Design_Module.Get (Module));
      end loop;
   end Iterate_Design_Modules;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Design_Vectors.Vector'Read (Stream, Vector);
      for I in 1 .. Vector.Last_Index loop
         Map.Insert (-(Vector (I).Name), I);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Design_Vectors.Vector'Write (Stream, Vector);
   end Save;

end Athena.Handles.Design;
