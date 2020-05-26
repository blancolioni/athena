with Ada.Streams.Stream_IO;

with Athena.Color;
with Athena.Managers;
with Athena.Money;

with Athena.Handles.Knowledge;
with Athena.Handles.Technology;

package Athena.Handles.Empire is

   type Standard_Empire_Design is
     (Scout, Transport,
      Recon, Fighter, Destroyer, Cruiser, Battleship, Carrier);

   type Empire_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
   with private;

   function Empty_Handle return Empire_Handle;

   function Reference
     (Handle : Empire_Handle)
      return Empire_Reference;

   function Get
     (Reference : Empire_Reference)
      return Empire_Handle;

   function Name (Empire : Empire_Handle) return String;
   function Adjective (Empire : Empire_Handle) return String;
   function Plural (Empire : Empire_Handle) return String;

   function Capital
     (Empire : Empire_Handle)
      return Colony_Reference;

   procedure Set_Capital
     (Empire  : Empire_Handle;
      Capital : Colony_Reference);

   function Color
     (Empire : Empire_Handle)
      return Athena.Color.Athena_Color;

   function Cash
     (Empire : Empire_Handle)
      return Athena.Money.Money_Type;

   function Debt
     (Empire : Empire_Handle)
      return Athena.Money.Money_Type;

   procedure Set_Cash
     (Empire : Empire_Handle;
      Cash   : Athena.Money.Money_Type);

   procedure Set_Debt
     (Empire : Empire_Handle;
      Debt   : Athena.Money.Money_Type);

   function Current_Tec_Level
     (Empire     : Empire_Handle;
      Technology : Athena.Handles.Technology.Technology_Handle)
      return Non_Negative_Real;

   function Current_Tec_Investment
     (Empire     : Empire_Handle;
      Technology : Athena.Handles.Technology.Technology_Handle)
      return Non_Negative_Real;

   procedure Update_Tec
     (Empire         : Empire_Handle;
      Technology     : Athena.Handles.Technology.Technology_Handle;
      New_Level      : Non_Negative_Real;
      New_Investment : Non_Negative_Real);

   function Standard_Design
     (Empire : Empire_Handle;
      Class  : Standard_Empire_Design)
      return Design_Reference;

   function Knowledge
     (Empire : Empire_Handle)
     return Athena.Handles.Knowledge.Knowledge_Handle;

   function Has_Manager
     (Empire  : Empire_Handle;
      Manager : Athena.Handles.Manager_Class)
      return Boolean;

   procedure Run_Manager
     (Empire : Empire_Handle;
      Manager : Athena.Handles.Manager_Class)
     with Pre => Has_Manager (Empire, Manager);

   procedure Set_Manager
     (Empire  : Empire_Handle;
      Manager : Athena.Handles.Manager_Class;
      To      : Athena.Managers.Root_Manager_Type'Class);

   procedure Send_Message
     (Empire  : Empire_Handle;
      To      : Manager_Class;
      Message : Athena.Managers.Message_Type'Class);

   function Get_By_Name
     (Name : String)
      return Empire_Handle;

   function Create_Empire
     (Star      : Star_Reference;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Color     : Athena.Color.Athena_Color;
      Cash      : Athena.Money.Money_Type;
      Debt      : Athena.Money.Money_Type)
      return Empire_Handle;

   procedure Set_Standard_Design
     (Empire : Empire_Handle;
      Class  : Standard_Empire_Design;
      Design : Design_Reference);

   procedure Add_Colony
     (Empire : Empire_Handle;
      Colony : Colony_Reference);

   procedure Remove_Colony
     (Empire : Empire_Handle;
      Colony : Colony_Reference);

   procedure Iterate_Colonies
     (Empire : Empire_Handle;
      Process : not null access
        procedure (Colony : Colony_Reference));

   procedure Add_Ship
     (Empire : Empire_Handle;
      Ship : Ship_Reference);

   procedure Remove_Ship
     (Empire : Empire_Handle;
      Ship : Ship_Reference);

   procedure Iterate_Ships
     (Empire  : Empire_Handle;
      Process : not null access
        procedure (Ship : Ship_Reference));

   procedure Iterate_Managed_Ships
     (Empire  : Empire_Handle;
      Manager : Manager_Class;
      Process : not null access
        procedure (Ship : Ship_Reference));

   procedure Iterate_All
     (Process : not null access
        procedure (Empire  : Empire_Handle));

   function Find_Colony
     (Empire  : Empire_Handle;
      Test    : not null access
        function (Colony : Colony_Reference) return Boolean)
      return Colony_Reference;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Empire_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface with
      record
         Reference : Empire_Reference := 0;
      end record;

   overriding function Short_Name
     (Empire : Empire_Handle)
      return String
   is (Empire.Name);

   overriding function Identifier
     (Empire : Empire_Handle)
      return Object_Identifier;

   function Empty_Handle return Empire_Handle
   is (Empire_Handle'(others => <>));

   function Reference
     (Handle : Empire_Handle)
      return Empire_Reference
   is (Handle.Reference);

   function Get
     (Reference : Empire_Reference)
      return Empire_Handle
   is (Reference /= 0, Reference);

end Athena.Handles.Empire;
