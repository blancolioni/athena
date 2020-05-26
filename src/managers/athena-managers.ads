private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with Athena.Real_Images;

with Athena.Calendar;
with Athena.Handles;

package Athena.Managers is

   type Message_Type is abstract tagged private;

   type Root_Manager_Type is abstract tagged private;

   function Identifier
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   procedure Create_Orders
     (Manager : in out Root_Manager_Type)
   is abstract;

   procedure Initialize
     (Manager : in out Root_Manager_Type;
      Name    : String;
      Empire  : Athena.Handles.Empire_Reference);

   function Next_Update
     (Manager : Root_Manager_Type'Class)
      return Athena.Calendar.Time;

   procedure Set_Next_Update
     (Manager   : in out Root_Manager_Type'Class;
      Update_At : Athena.Calendar.Time);

   procedure Set_Next_Update_Delay
     (Manager      : in out Root_Manager_Type'Class;
      Update_Delay : Duration);

   procedure Send_Message
     (To      : in out Root_Manager_Type'Class;
      Message : Message_Type'Class);

   function Exists
     (Name   : String)
      return Boolean;

   function Get_Manager
     (Name   : String;
      Empire : Athena.Handles.Empire_Reference)
      return Root_Manager_Type'Class
     with Pre => Exists (Name);

   procedure Log
     (Manager : Root_Manager_Type'Class;
      Message : String);

   procedure Load_Managers;

private

   type Message_Type is abstract tagged
      record
         null;
      end record;

   package Message_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Message_Type'Class);

   type Root_Manager_Type is abstract tagged
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Empire      : Athena.Handles.Empire_Reference;
         Priority    : Athena.Handles.Order_Priority;
         Next_Update : Athena.Calendar.Time;
         Messages    : Message_Lists.List;
      end record;

   function Next_Update
     (Manager : Root_Manager_Type'Class)
      return Athena.Calendar.Time
   is (Manager.Next_Update);

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

end Athena.Managers;
