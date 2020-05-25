with Ada.Streams.Stream_IO;

with Athena.Handles.Star;
with Athena.Handles.Empire;

package Athena.Handles.Colony is

   type Colony_Handle is
     new Root_Athena_Handle
   with private;

   function Reference (Colony : Colony_Handle) return Colony_Reference;
   function Get (Colony : Colony_Reference) return Colony_Handle;
   function Empty_Handle return Colony_Handle;

   function Star
     (Colony : Colony_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Population
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Construct
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Industry
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Material
     (Colony : Colony_Handle)
      return Non_Negative_Real;

   function Owner
     (Colony : Colony_Handle)
      return Athena.Handles.Empire.Empire_Handle;

   procedure Set_Population
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Construct
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Industry
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Material
     (Colony   : Colony_Handle;
      Quantity : Non_Negative_Real);

   procedure Set_Owner
     (Colony    : Colony_Handle;
      New_Owner : Athena.Handles.Empire.Empire_Handle);

   function Create
     (Star      : Athena.Handles.Star.Star_Handle;
      Owner     : Athena.Handles.Empire.Empire_Handle;
      Pop       : Non_Negative_Real := 0.0;
      Industry  : Non_Negative_Real := 0.0;
      Material  : Non_Negative_Real := 0.0)
      return Colony_Handle;

   procedure Iterate_All
     (Process : not null access
        procedure (Colony : Colony_Handle));

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Colony_Handle is
     new Root_Athena_Handle with
      record
         Reference : Colony_Reference := 0;
      end record;

   overriding function Short_Name
     (Colony : Colony_Handle)
      return String
   is (Colony.Owner.Name & " colony at " & Colony.Star.Name);

   function Reference (Colony : Colony_Handle) return Colony_Reference
   is (Colony.Reference);

   function Get (Colony : Colony_Reference) return Colony_Handle
   is (Colony /= 0, Colony);

   function Empty_Handle return Colony_Handle
   is (False, 0);

end Athena.Handles.Colony;
