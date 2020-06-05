with Ada.Streams.Stream_IO;

with Athena.Cargo;

with Athena.Handles.Design_Module;
with Athena.Handles.Empire;
with Athena.Handles.Hull;
with Athena.Handles.Hull_Armor;

package Athena.Handles.Design is

   type Design_Handle is
     new Root_Athena_Handle
   with private;

   function Reference (Handle : Design_Handle) return Design_Reference;
   function Get (Reference : Design_Reference) return Design_Handle;

   function Name
     (Handle : Design_Handle)
      return String;

   function Tonnage
     (Handle : Design_Handle)
      return Non_Negative_Real;

   function Mass
     (Handle : Design_Handle)
      return Non_Negative_Real;

   function Passenger_Berths
     (Handle : Design_Handle)
      return Non_Negative_Real;

   function Free_Space
     (Handle : Design_Handle)
      return Non_Negative_Real;

   function Cargo_Space
     (Handle : Design_Handle;
      Cargo  : Athena.Cargo.Cargo_Category)
      return Non_Negative_Real;

   function Tank_Size
     (Handle : Design_Handle)
      return Non_Negative_Real;

   function Hard_Points
     (Handle : Design_Handle)
      return Natural;

   function Firm_Points
     (Handle : Design_Handle)
      return Natural;

   function Default_Script
     (Handle : Design_Handle)
      return String;

   procedure Iterate_Design_Modules
     (Design : Design_Handle;
      Process : not null access
        procedure
          (Design_Module : Athena.Handles.Design_Module.Design_Module_Handle));

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
      return Design_Handle;

   procedure Add_Design_Module
     (To_Design     : Design_Handle;
      Design_Module : Athena.Handles.Design_Module.Design_Module_Handle)
     with Pre => Design_Module.Component.Tonnage <= To_Design.Free_Space;

   function Get_By_Name
     (Name : String)
      return Design_Handle;

   procedure Iterate
     (Process : not null access
        procedure (Design : Design_Handle));

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Design_Handle is
     new Root_Athena_Handle with
      record
         Reference : Design_Reference := 0;
      end record;

   overriding function Short_Name
     (Design : Design_Handle)
      return String;

   function Reference (Handle : Design_Handle) return Design_Reference
   is (Handle.Reference);

   function Get (Reference : Design_Reference) return Design_Handle
   is (Design_Handle'
         (Has_Element => Reference /= 0,
          Reference   => Reference));

end Athena.Handles.Design;
