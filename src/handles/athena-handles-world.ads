with Ada.Streams.Stream_IO;

with Athena.Calendar;
with Athena.Orbits;
with Athena.Trigonometry;

with Athena.Handles.Commodity;
with Athena.Handles.Star;

package Athena.Handles.World is

   type World_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
     and Athena.Orbits.Orbiting_Interface
   with private;

   function Reference (Handle : World_Handle) return World_Reference;
   function Get (Reference : World_Reference) return World_Handle;

   function Empty_Handle return World_Handle;

   function Resource_Quality
     (Handle   : World_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle)
      return Non_Negative_Real;

   function Extract_Resource
     (Handle   : World_Handle;
      Resource : Athena.Handles.Commodity.Commodity_Handle;
      Size     : Non_Negative_Real)
      return Non_Negative_Real;

   function Star
     (Handle : World_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Name
     (World : World_Handle)
      return String;

   procedure Set_Name
     (World     : World_Handle;
      New_Name : String);

   function Has_Owner
     (World : World_Handle)
      return Boolean;

   function Owner
     (World : World_Handle)
      return Empire_Reference;

   procedure Set_Owner
     (World      : World_Handle;
      New_Owner : Empire_Reference);

   function Has_Colony
     (World : World_Handle)
      return Boolean;

   function Colony
     (World : World_Handle)
      return Colony_Reference;

   procedure Set_Colony
     (World   : World_Handle;
      Colony : Colony_Reference);

   function Space
     (World : World_Handle)
      return Natural;

   function Resource
     (World : World_Handle)
      return Unit_Real;

   function Habitability
     (World : World_Handle)
      return Unit_Real;

   function Create
     (Star           : Athena.Handles.Star.Star_Handle;
      Mass           : Non_Negative_Real;
      Semimajor_Axis : Non_Negative_Real;
      Epoch          : Athena.Calendar.Time;
      Eccentricity   : Unit_Real;
      Inclination    : Athena.Trigonometry.Angle;
      Name           : String;
      Space          : Positive;
      Resource       : Unit_Real;
      Habitability   : Unit_Real)
      return World_Handle;

   function Get_By_Name
     (Name : String)
      return World_Handle;

   function Find_World
     (Test : not null access function (Handle : World_Handle) return Boolean)
      return World_Handle;

   procedure Iterate_Worlds
     (Process      : not null access
        procedure (Handle : World_Handle));

   procedure Iterate_Orbiting_Ships
     (World         : World_Handle;
      Process      : not null access
        procedure (Reference : Ship_Reference));

   procedure Add_Ship
     (World : World_Handle;
      Ship : Ship_Reference);

   procedure Remove_Ship
     (World : World_Handle;
      Ship : Ship_Reference);

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type World_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
     and Athena.Orbits.Orbiting_Interface with
      record
         Reference : World_Reference;
      end record;

   overriding function Short_Name
     (World : World_Handle)
      return String
   is (World.Name);

   overriding function Identifier
     (World : World_Handle)
      return Object_Identifier;

   overriding function Mass
     (World : World_Handle)
      return Non_Negative_Real;

   overriding function Semimajor_Axis
     (World : World_Handle)
      return Non_Negative_Real;

   overriding function Epoch
     (World : World_Handle)
      return Athena.Calendar.Time;

   overriding function Primary
     (World : World_Handle)
      return Athena.Orbits.Massive_Interface'Class;

   overriding function Eccentricity
     (World : World_Handle)
      return Unit_Real;

   overriding function Inclination
     (World : World_Handle)
      return Athena.Trigonometry.Angle;

   function Reference (Handle : World_Handle) return World_Reference
   is (Handle.Reference);

   function Get (Reference : World_Reference) return World_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return World_Handle
   is (False, 0);

end Athena.Handles.World;
