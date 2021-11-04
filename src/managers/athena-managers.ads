private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with Athena.Real_Images;

with Minerva.Empire;
with Minerva.Empire_Manager;

package Athena.Managers is

   type Message_Type is abstract tagged private;

   type Athena_Manager_Script is abstract tagged private;

   function Identifier
     (Manager : Athena_Manager_Script)
      return String
      is abstract;

   procedure Create_Orders
     (Manager : Athena_Manager_Script)
   is abstract;

   procedure Process_Message
     (Manager : Athena_Manager_Script;
      Message : Message_Type'Class)
   is null;

   procedure Initialize
     (Script  : in out Athena_Manager_Script;
      Manager : Minerva.Empire_Manager.Empire_Manager_Class;
      Name    : String;
      Empire  : Minerva.Empire.Empire_Class);

   function Exists
     (Class  : String;
      Name   : String)
      return Boolean;

   function Get_Manager
     (Class  : String;
      Name   : String;
      Empire : Minerva.Empire.Empire_Class)
      return Athena_Manager_Script'Class
     with Pre => Exists (Class, Name);

   procedure Send_Message
     (Destination : String;
      Empire      : Minerva.Empire.Empire_Class;
      Message     : Message_Type'Class);

   procedure Log
     (Manager : Athena_Manager_Script'Class;
      Message : String);

   procedure Load_Managers;

private

   type Message_Type is abstract tagged
      record
         null;
      end record;

   package Message_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Message_Type'Class);

   type Athena_Manager_Script is abstract tagged
      record
         Manager     : Minerva.Empire_Manager.Empire_Manager_Handle;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Empire      : Minerva.Empire.Empire_Handle;
         Priority    : Integer;
      end record;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

end Athena.Managers;
