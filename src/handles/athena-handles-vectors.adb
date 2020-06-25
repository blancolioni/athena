package body Athena.Handles.Vectors is

   type Vector_Iterator is
     new Vector_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access constant Vector;
         Current   : Cursor;
      end record;

   overriding function First
     (Object : Vector_Iterator)
      return Cursor
   is (if Object.Container.Last_Index = Extended_Index'First
       then No_Element
       else (Index_Type'First, Object.Container));

   overriding function Next
     (Object   : Vector_Iterator;
      Position : Cursor)
      return Cursor
   is (if Position.Index < Object.Container.Last_Index - 1
       then (Position.Index + 1, Position.Container)
       else No_Element);

   overriding function Last
     (Object : Vector_Iterator)
      return Cursor
   is (if Object.Container.Last_Index = Extended_Index'First
       then No_Element
       else (Object.Container.Last_Index, Object.Container));

   overriding function Previous
     (Object   : Vector_Iterator;
      Position : Cursor)
      return Cursor
   is (if Position.Index > Index_Type'First
       then (Position.Index - 1, Position.Container)
       else No_Element);

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector;
                     New_Item  : Element_Type;
                     Index     : out Index_Type) is
   begin
      Container.Internal.Append (New_Item, Index);
   end Append;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Vector; Reference : Index_Type)
      return Constant_Reference_Type
   is
   begin
      return (Element =>
                Container.Internal.Constant_Reference (Reference).Element);
   end Constant_Reference;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector; Reference : Index_Type) return Element_Type
   is
   begin
      return Container.Internal.Constant_Reference (Reference);
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return Vector_Iterator'
        (Current => Cursor'(Index     => Index_Type'First,
                            Container => Container'Unchecked_Access),
         Container => Container'Unchecked_Access);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Vector;
      Start     : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return Vector_Iterator'
        (Current   => Start,
         Container => Container'Unchecked_Access);
   end Iterate;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Internal.Last_Index;
   end Last_Index;

   ----------
   -- Read --
   ----------

   procedure Read
     (Container : in out Vector;
      Stream    : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Container.Internal.Read (Stream);
   end Read;

   ------------
   -- Update --
   ------------

   procedure Update
     (Container : in out Vector;
      Index     : Index_Type;
      Proc      : not null access
        procedure (Element : in out Element_Type))
   is
   begin
      Container.Internal.Update (Index, Proc);
   end Update;

   -----------
   -- Write --
   -----------

   procedure Write
     (Container : in out Vector;
      Stream    : Ada.Streams.Stream_IO.Stream_Access)
   is
   begin
      Container.Internal.Write (Stream);
   end Write;

   -------------------------
   -- Synchronized_Vector --
   -------------------------

   protected body Synchronized_Vector is

      ------------
      -- Append --
      ------------

      procedure Append (New_Item : Element_Type;
                        Index    : out Index_Type)
      is
      begin
         Vector.Append (New_Item);
         Index := Vector.Last_Index;
      end Append;

      ------------------------
      -- Constant_Reference --
      ------------------------

      function Constant_Reference
        (Index : Index_Type)
         return Constant_Reference_Type
      is
      begin
         return (Element => Vector.Constant_Reference (Index).Element);
      end Constant_Reference;

      ----------------
      -- Last_Index --
      ----------------

      function Last_Index return Extended_Index is
      begin
         return Vector.Last_Index;
      end Last_Index;

      ----------
      -- Read --
      ----------

      procedure Read (Stream : Ada.Streams.Stream_IO.Stream_Access) is
      begin
         Element_Vectors.Vector'Read (Stream, Vector);
      end Read;

      ------------
      -- Update --
      ------------

      procedure Update
        (Index     : Index_Type;
         Proc      : not null access
           procedure (Element : in out Element_Type))
      is
         Element : Element_Type renames Vector (Index);
      begin
         Proc (Element);
      end Update;

      -----------
      -- Write --
      -----------

      procedure Write (Stream : Ada.Streams.Stream_IO.Stream_Access) is
      begin
         Element_Vectors.Vector'Write (Stream, Vector);
      end Write;

   end Synchronized_Vector;

end Athena.Handles.Vectors;
