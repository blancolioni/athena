with Ada.Containers.Vectors;

package body Athena.Handles.Installation is

   type Installation_Record is
      record
         Identifier  : Object_Identifier;
         Facility    : Facility_Reference;
         Size        : Non_Negative_Real;
         Stock       : Athena.Handles.Commodity.Stock_Type;
      end record;

   package Installation_Vectors is
     new Ada.Containers.Vectors
       (Real_Installation_Reference, Installation_Record);

   Vector : Installation_Vectors.Vector;

   overriding function Get_Stock
     (Installation : Installation_Handle;
      Commodity    : Athena.Handles.Commodity.Commodity_Handle'Class)
      return Non_Negative_Real
   is (Athena.Handles.Commodity.Get_Stock
       (Vector (Installation.Reference).Stock, Commodity));

   function Facility
     (Installation : Installation_Handle)
      return Athena.Handles.Facility.Facility_Handle
   is (Athena.Handles.Facility.Get (Vector (Installation.Reference).Facility));

   function Size
     (Installation : Installation_Handle)
      return Non_Negative_Real
   is (Vector (Installation.Reference).Size);

   ------------
   -- Create --
   ------------

   function Create
     (Facility : Athena.Handles.Facility.Facility_Handle;
      Size     : Non_Negative_Real)
      return Installation_Handle
   is
   begin
      Vector.Append
        (Installation_Record'
           (Identifier    => Next_Identifier,
            Facility      => Facility.Reference,
            Size          => Size,
            Stock         => <>));
      return (True, Vector.Last_Index);
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Installation_Vectors.Vector'Read (Stream, Vector);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Installation_Vectors.Vector'Write (Stream, Vector);
   end Save;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (Installation : Installation_Handle;
      Commodity    : Athena.Handles.Commodity.Commodity_Handle'Class;
      Quantity     : Non_Negative_Real)
   is
   begin
      Athena.Handles.Commodity.Set_Stock
        (Vector (Installation.Reference).Stock, Commodity, Quantity);
   end Set_Stock;

end Athena.Handles.Installation;
