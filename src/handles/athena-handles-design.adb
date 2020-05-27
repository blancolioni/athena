with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

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
         Armor_Points   : Natural;
         Tonnage        : Non_Negative_Real;
         Mass           : Non_Negative_Real;
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

   function Mass
     (Handle : Design_Handle)
      return Non_Negative_Real
   is (Vector (Handle.Reference).Mass);

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
      Rec.Mass := Rec.Mass + Design_Module.Component.Mass;
      Ada.Text_IO.Put_Line
        ("  module: " & Design_Module.Component.Tag
         & " tonnage " & Image (Design_Module.Component.Tonnage)
         & " mass " & Image (Design_Module.Component.Mass)
         & " power " & Image (Design_Module.Component.Idle_Power_Consumption)
        & "/" & Image (Design_Module.Component.Active_Power_Consumption));
   end Add_Design_Module;

   ------------
   -- Create --
   ------------

   function Create
     (Name           : String;
      Owner          : Athena.Handles.Empire.Empire_Handle;
      Hull           : Athena.Handles.Hull.Hull_Handle;
      Armor          : Athena.Handles.Hull_Armor.Hull_Armor_Handle;
      Armor_Points   : Natural;
      Tonnage        : Non_Negative_Real;
      Hull_Points    : Non_Negative_Real;
      Fuel_Tank      : Non_Negative_Real;
      Firm_Points    : Natural;
      Hard_Points    : Natural;
      Default_Script : String;
      Default_Rank   : Positive)
      return Design_Handle
   is
      Armor_Tonnage : constant Non_Negative_Real :=
                        (if Armor.Has_Element and then Armor_Points > 0
                         then Real (Armor_Points)
                         * Armor.Tonnage_Fraction * Tonnage
                         else 0.0);
      Armor_Mass    : constant Non_Negative_Real :=
                        (if Armor.Has_Element
                         then Armor.Mass * Armor_Tonnage
                         else 0.0);
      Empty_Mass : constant Non_Negative_Real :=
                        Tonnage * Hull.Mass_Fraction + Armor_Mass;
      Free_Space    : constant Non_Negative_Real :=
                        Tonnage - Fuel_Tank - Armor_Tonnage;
   begin
      Ada.Text_IO.Put_Line
        ("design: " & Name
         & ": tonnage "
         & Image (Tonnage)
         & " armor "
         & Image (Armor_Mass)
         & " dry mass "
         & Image (Empty_Mass)
         & " free space "
         & Image (Free_Space));
      Vector.Append
        (Design_Record'
           (Name           => +Name,
            Owner          => Owner.Reference,
            Hull           => Hull.Reference,
            Armor          => Armor.Reference,
            Armor_Points   => Armor_Points,
            Tonnage        => Tonnage,
            Mass           => Empty_Mass,
            Hull_Points    => Hull_Points,
            Tank_Size      => Fuel_Tank,
            Cargo_Space    => Free_Space,
            Firm_Points    => Firm_Points,
            Hard_Points    => Hard_Points,
            Default_Script => +Default_Script,
            Default_Rank   => Default_Rank,
            Design_Modules => <>));
      Map.Insert (Name, Vector.Last_Index);

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
