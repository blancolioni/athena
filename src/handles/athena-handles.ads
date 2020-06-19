private with Ada.Strings.Unbounded;
private with Athena.Real_Images;

with Athena.Signals;

package Athena.Handles is

   subtype Object_Identifier is String (1 .. 8);

   type Athena_Turn_Number is private;

   function Current_Turn return Athena_Turn_Number;
   function Current_Turn_Image
     return String;

   function File_Name_Turn_Image
     return String;

   procedure Next_Turn;

   type Root_Athena_Handle is
     abstract new Athena.Signals.Signal_Source_Interface
   with private;

   function Has_Element
     (Handle : Root_Athena_Handle'Class)
      return Boolean;

   function Short_Name
     (Handle : Root_Athena_Handle)
      return String
      is abstract;

   function Long_Name
     (Handle : Root_Athena_Handle)
      return String
   is (Root_Athena_Handle'Class (Handle).Short_Name);

   procedure Log
     (Handle  : Root_Athena_Handle'Class;
      Message : String);

   type Has_Name_Interface is interface;

   function Name (Has_Name : Has_Name_Interface) return String is abstract;

   procedure Set_Name (Has_Name : Has_Name_Interface;
                       New_Name : String)
   is abstract;

   type Has_Identifier_Interface is interface;

   function Identifier
     (Has_Identifier : Has_Identifier_Interface)
      return Object_Identifier
      is abstract;

   type Localised_Interface is interface;

   function Tag
     (Localised : Localised_Interface)
      return String
      is abstract;

   function Local_Text
     (Localised : Localised_Interface'Class)
      return String;

   type Manager_Class is
     (Attack_Manager,
      Colonization_Manager,
      Defense_Manager,
      Development_Manager,
      Exploration_Manager,
      Repair_Manager,
      Research_Manager,
      Transport_Manager,
      Upgrade_Manager);

   type Order_Priority is range 1 .. 2000;

   type Colony_Reference is private;

   Null_Colony_Reference : constant Colony_Reference;

   type Commodity_Reference is private;

   Null_Commodity_Reference : constant Commodity_Reference;

   type Component_Reference is private;

   type Design_Reference is private;

   type Design_Module_Reference is private;

   type Empire_Reference is private;

   Null_Empire_Reference : constant Empire_Reference;

   type Encounter_Reference is private;

   type Facility_Reference is private;

   Null_Facility_Reference : constant Facility_Reference;

   type Fleet_Reference is private;

   Null_Fleet_Reference : constant Fleet_Reference;

   type Hull_Reference is private;

   type Hull_Armor_Reference is private;

   type Installation_Reference is private;

   Null_Installation_Reference : constant Installation_Reference;

   type Knowledge_Reference is private;

   Null_Knowledge_Reference : constant Knowledge_Reference;

   type Manager_Reference is private;

   Null_Manager_Reference : constant Manager_Reference;

   type Module_Reference is private;

   Null_Module_Reference : constant Module_Reference;

   type Order_Reference is private;

   Null_Order_Reference : constant Order_Reference;

   type Production_Reference is private;

   Null_Production_Reference : constant Production_Reference;

   type Relationship_Reference is private;

   Null_Relationship_Reference : constant Relationship_Reference;

   type Ship_Reference is private;

   type Ship_Module_Reference is private;

   type Star_Reference is private;

   Null_Star_Reference : constant Star_Reference;

   type Technology_Reference is private;

   Null_Technology_Reference : constant Technology_Reference;

   type War_Reference is private;

   Null_War_Reference : constant War_Reference;

   type World_Reference is private;

   Null_World_Reference : constant World_Reference;

   type Zone_Reference is private;

   Null_Zone_Reference : constant Zone_Reference;

private

   Current_Identifier : Object_Identifier;

   function Next_Identifier
     return Object_Identifier;

   type Athena_Turn_Number is new Natural;

   Turn_Number : Athena_Turn_Number;

   function Current_Turn return Athena_Turn_Number
   is (Turn_Number);

   type Root_Athena_Handle is
     abstract new Athena.Signals.Signal_Source_Interface with
      record
         Has_Element : Boolean := False;
      end record;

   function Has_Element (Handle : Root_Athena_Handle'Class) return Boolean
   is (Handle.Has_Element);

   type Colony_Reference is new Natural;

   subtype Real_Colony_Reference is
     Colony_Reference range 1 .. Colony_Reference'Last;

   Null_Colony_Reference : constant Colony_Reference := 0;

   type Commodity_Reference is new Natural;

   subtype Real_Commodity_Reference is
     Commodity_Reference range 1 .. Commodity_Reference'Last;

   Null_Commodity_Reference : constant Commodity_Reference := 0;

   type Component_Reference is new Natural;

   subtype Real_Component_Reference is
     Component_Reference range 1 .. Component_Reference'Last;

   type Design_Reference is new Natural;

   subtype Real_Design_Reference is
     Design_Reference range 1 .. Design_Reference'Last;

   type Design_Module_Reference is new Natural;

   subtype Real_Design_Module_Reference is
     Design_Module_Reference range 1 .. Design_Module_Reference'Last;

   type Empire_Reference is new Natural;

   subtype Real_Empire_Reference is
     Empire_Reference range 1 .. Empire_Reference'Last;

   Null_Empire_Reference : constant Empire_Reference := 0;

   type Encounter_Reference is new Natural;

   subtype Real_Encounter_Reference is
     Encounter_Reference range 1 .. Encounter_Reference'Last;

   type Facility_Reference is new Natural;

   subtype Real_Facility_Reference is
     Facility_Reference range 1 .. Facility_Reference'Last;

   Null_Facility_Reference : constant Facility_Reference := 0;

   type Fleet_Reference is new Natural;

   subtype Real_Fleet_Reference is
     Fleet_Reference range 1 .. Fleet_Reference'Last;

   Null_Fleet_Reference : constant Fleet_Reference := 0;

   type Hull_Reference is new Natural;

   subtype Real_Hull_Reference is
     Hull_Reference range 1 .. Hull_Reference'Last;

   type Hull_Armor_Reference is new Natural;

   subtype Real_Hull_Armor_Reference is
     Hull_Armor_Reference range 1 .. Hull_Armor_Reference'Last;

   type Installation_Reference is new Natural;

   subtype Real_Installation_Reference is
     Installation_Reference range 1 .. Installation_Reference'Last;

   Null_Installation_Reference : constant Installation_Reference := 0;

   type Knowledge_Reference is new Natural;

   subtype Real_Knowledge_Reference is
     Knowledge_Reference range 1 .. Knowledge_Reference'Last;

   Null_Knowledge_Reference : constant Knowledge_Reference := 0;

   type Manager_Reference is new Natural;

   Null_Manager_Reference : constant Manager_Reference := 0;

   subtype Real_Manager_Reference is
     Manager_Reference range 1 .. Manager_Reference'Last;

   type Module_Reference is new Natural;

   subtype Real_Module_Reference is
     Module_Reference range 1 .. Module_Reference'Last;

   Null_Module_Reference : constant Module_Reference := 0;

   type Order_Reference is new Natural;

   subtype Real_Order_Reference is
     Order_Reference range 1 .. Order_Reference'Last;

   Null_Order_Reference : constant Order_Reference := 0;

   type Production_Reference is new Natural;

   subtype Real_Production_Reference is
     Production_Reference range 1 .. Production_Reference'Last;

   Null_Production_Reference : constant Production_Reference := 0;

   type Relationship_Reference is new Natural;

   subtype Real_Relationship_Reference is
     Relationship_Reference range 1 .. Relationship_Reference'Last;

   Null_Relationship_Reference : constant Relationship_Reference := 0;

   type Ship_Reference is new Natural;

   subtype Real_Ship_Reference is
     Ship_Reference range 1 .. Ship_Reference'Last;

   type Ship_Module_Reference is new Natural;

   subtype Real_Ship_Module_Reference is
     Ship_Module_Reference range 1 .. Ship_Module_Reference'Last;

   type Star_Reference is new Natural;

   subtype Real_Star_Reference is
     Star_Reference range 1 .. Star_Reference'Last;

   Null_Star_Reference : constant Star_Reference := 0;

   type Technology_Reference is new Natural;

   subtype Real_Technology_Reference is
     Technology_Reference range 1 .. Technology_Reference'Last;

   Null_Technology_Reference : constant Technology_Reference := 0;

   type War_Reference is new Natural;

   subtype Real_War_Reference is
     War_Reference range 1 .. War_Reference'Last;

   Null_War_Reference : constant War_Reference := 0;

   type World_Reference is new Natural;

   subtype Real_World_Reference is
     World_Reference range 1 .. World_Reference'Last;

   Null_World_Reference : constant World_Reference := 0;

   type Zone_Reference is new Natural;

   subtype Real_Zone_Reference is
     Zone_Reference range 1 .. Zone_Reference'Last;

   Null_Zone_Reference : constant Zone_Reference := 0;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

end Athena.Handles;
