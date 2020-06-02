with Ada.Streams.Stream_IO;

with Athena.Handles.Commodity;

package Athena.Handles.Star is

   type Star_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
   with private;

   function Reference (Handle : Star_Handle) return Star_Reference;
   function Get (Reference : Star_Reference) return Star_Handle;

   function Empty_Handle return Star_Handle;

   function Resource_Quality
     (Handle   : Star_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle)
      return Non_Negative_Real;

   function Extract_Resource
     (Handle   : Star_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle;
      Size     : Non_Negative_Real)
      return Non_Negative_Real;

   function Name
     (Star : Star_Handle)
      return String;

   procedure Set_Name
     (Star     : Star_Handle;
      New_Name : String);

   function Has_Owner
     (Star : Star_Handle)
      return Boolean;

   function Owner
     (Star : Star_Handle)
      return Empire_Reference;

   procedure Set_Owner
     (Star      : Star_Handle;
      New_Owner : Empire_Reference);

   function Has_Colony
     (Star : Star_Handle)
      return Boolean;

   function Colony
     (Star : Star_Handle)
      return Colony_Reference;

   procedure Set_Colony
     (Star   : Star_Handle;
      Colony : Colony_Reference);

   function X
     (Star : Star_Handle)
      return Real;

   function Y
     (Star : Star_Handle)
      return Real;

   function Space
     (Star : Star_Handle)
      return Natural;

   function Resource
     (Star : Star_Handle)
      return Unit_Real;

   function Habitability
     (Star : Star_Handle)
      return Unit_Real;

   function Create
     (X, Y          : Real;
      Name          : String;
      Core_Distance : Non_Negative_Real;
      Space         : Positive;
      Resource      : Unit_Real;
      Habitability  : Unit_Real)
      return Star_Handle;

   function Get_By_Name
     (Name : String)
      return Star_Handle;

   function Find_Star
     (Test : not null access function (Handle : Star_Handle) return Boolean)
      return Star_Handle;

   procedure Iterate_Nearest_Stars
     (To_Star      : Star_Handle;
      Max_Distance : Non_Negative_Real;
      Process      : not null access
        function (Handle : Star_Handle) return Boolean);

   procedure Iterate_Stars
     (Process      : not null access
        procedure (Handle : Star_Handle));

   procedure Iterate_Orbiting_Ships
     (Star         : Star_Handle;
      Process      : not null access
        procedure (Reference : Ship_Reference));

   procedure Add_Ship
     (Star : Star_Handle;
      Ship : Ship_Reference);

   procedure Remove_Ship
     (Star : Star_Handle;
      Ship : Ship_Reference);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Calculate_Distances;

   --  type Star_Cursor is private;
   --
   --  function Has_Element (Cursor : Star_Cursor) return Boolean;
   --
   --  package Star_Iterators is
   --    new Ada.Iterator_Interfaces (Star_Cursor, Has_Element);
   --
   --  function Iterate_Nearest_Stars
   --    (To_Star : Star_Handle;
   --     Max_Distance : Non_Negative_Real)
   --     return Star_Iterators.Forward_Iterator'Class;
   --
private

   --  type Star_Cursor is new Star_Reference;
   --
   --  function Has_Element (Cursor : Star_Cursor) return Boolean
   --  is (Cursor /= 0);
   --
   type Star_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface with
      record
         Reference : Star_Reference;
      end record;

   overriding function Short_Name
     (Star : Star_Handle)
      return String
   is (Star.Name);

   overriding function Identifier
     (Star : Star_Handle)
      return Object_Identifier;

   function Reference (Handle : Star_Handle) return Star_Reference
   is (Handle.Reference);

   function Get (Reference : Star_Reference) return Star_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Star_Handle
   is (False, 0);

end Athena.Handles.Star;
