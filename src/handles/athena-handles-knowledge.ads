with Ada.Containers.Doubly_Linked_Lists;
with Ada.Streams.Stream_IO;

with Athena.Handles.Star;

package Athena.Handles.Knowledge is

   type Known_Ship_Record is
      record
         Owner        : Empire_Reference;
         Hull_Tonnage : Non_Negative_Real;
         Weapons      : Non_Negative_Real;
      end record;

   package Known_Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Known_Ship_Record);

   type Knowledge_Handle is
     new Root_Athena_Handle
   with private;

   function Reference
     (Knowledge : Knowledge_Handle)
      return Knowledge_Reference;

   function Get
     (Knowledge : Knowledge_Reference)
      return Knowledge_Handle;

   function Empty_Handle return Knowledge_Handle;

   function Create
     (Empire : Empire_Reference)
     return Knowledge_Handle;

   function Is_Loaded
     (Handle : Knowledge_Handle)
      return Boolean;

   procedure Load
     (Handle : Knowledge_Handle);

   function Colonizing
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function Visited
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Boolean;

   function Last_Visit
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Athena_Turn_Number;

   function Turns_Since_Last_Visit
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Natural
     with Pre => Knowledge.Visited (Star);

   function Get_Known_Ships
     (Knowledge : Knowledge_Handle;
      At_Star   : Athena.Handles.Star.Star_Handle)
      return Known_Ship_Lists.List;

   procedure Iterate_Neighbours
     (Knowledge : Knowledge_Handle;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Neighbour : Athena.Handles.Star.Star_Handle;
                   Nearest   : Colony_Reference;
                   Stop      : out Boolean));

   procedure Iterate_Threats
     (Knowledge : Knowledge_Handle;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Threat      : Empire_Reference;
                   Threat_Star : Athena.Handles.Star.Star_Handle;
                   Nearest     : Colony_Reference;
                   Stop        : out Boolean));

   procedure Iterate_Uncolonized
     (Knowledge : Knowledge_Handle;
      Process   : not null access
        procedure (Star      : Athena.Handles.Star.Star_Handle;
                   Stop      : out Boolean));

   procedure Set_Colonizing
     (Knowledge  : Knowledge_Handle;
      Star       : Athena.Handles.Star.Star_Handle;
      Colonizing : Boolean);

   procedure Clear_Colonizing
     (Knowledge  : Knowledge_Handle;
      Star       : Athena.Handles.Star.Star_Handle);

   procedure Visit
     (Knowledge  : Knowledge_Handle;
      Star       : Athena.Handles.Star.Star_Handle);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Knowledge_Handle is
     new Root_Athena_Handle with
      record
         Reference : Knowledge_Reference := 0;
      end record;

   overriding function Short_Name
     (Knowledge : Knowledge_Handle)
      return String;

   function Reference (Knowledge : Knowledge_Handle) return Knowledge_Reference
   is (Knowledge.Reference);

   function Get (Knowledge : Knowledge_Reference) return Knowledge_Handle
   is (Knowledge /= 0, Knowledge);

   function Empty_Handle return Knowledge_Handle
   is (False, 0);

   function Turns_Since_Last_Visit
     (Knowledge : Knowledge_Handle;
      Star      : Athena.Handles.Star.Star_Handle)
      return Natural
   is (Natural (Current_Turn - Knowledge.Last_Visit (Star)));

end Athena.Handles.Knowledge;
