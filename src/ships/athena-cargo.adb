with Ada.Strings.Unbounded;

with Athena.Real_Images;

package body Athena.Cargo is

   ---------------
   -- Add_Cargo --
   ---------------

   procedure Add_Cargo
     (Container : in out Cargo_Container;
      Item      : Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      for Element of Container.Categories (Item.Category) loop
         if Element.Cargo.Element = Item then
            Element.Quantity := Element.Quantity + Quantity;
            return;
         end if;
      end loop;
      Container.Categories (Item.Category).Append
        ((Cargo_Holders.To_Holder (Item), Quantity));
   end Add_Cargo;

   ---------------------
   -- Available_Space --
   ---------------------

   function Available_Space
     (Holder   : Cargo_Holder_Interface'Class;
      Category : Cargo_Category)
      return Non_Negative_Real
   is
   begin
      return Holder.Cargo_Space (Category)
        - Holder.Current_Tonnage (Category);
   end Available_Space;

   ---------------------
   -- Content_Summary --
   ---------------------

   function Content_Summary (Cargo : Cargo_Container) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := Null_Unbounded_String;
      First  : Boolean := True;
   begin
      for Category in Cargo_Category loop
         for Item of Cargo.Categories (Category) loop
            if First then
               First := False;
            else
               Result := Result & ";";
            end if;
            Result := Result
              & Athena.Real_Images.Approximate_Image (Item.Quantity)
              & " "
              & Item.Cargo.Element.Tag;
         end loop;
      end loop;

      if First then
         return "empty";
      else
         return To_String (Result);
      end if;
   end Content_Summary;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Container : Cargo_Container;
      Item      : Cargo_Interface'Class)
      return Non_Negative_Real
   is
   begin
      for Element of Container.Categories (Item.Category) loop
         if Element.Cargo.Element = Item then
            return Element.Quantity;
         end if;
      end loop;
      return 0.0;
   end Current_Quantity;

   ---------------------
   -- Empty_Container --
   ---------------------

   function Empty_Container return Cargo_Container is
   begin
      return Cargo_Container'
        (Categories => (others => <>));
   end Empty_Container;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Cargo_Container) return Boolean is
   begin
      return (for all List of Container.Categories => List.Is_Empty);
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Cargo_Container;
      Process   : not null access procedure
        (Cargo : Cargo_Interface'Class;
         Quantity : Non_Negative_Real))
   is
   begin
      for List of Container.Categories loop
         for Item of List loop
            Process (Item.Cargo.Element, Item.Quantity);
         end loop;
      end loop;
   end Iterate;

   ----------
   -- Load --
   ----------

   procedure Load
     (Holder :        Cargo_Holder_Interface'Class;
      From   : in out Cargo_Container'Class)
   is
      Have_Zero : Boolean := False;
   begin
      for Category in Cargo_Category loop
         for Item of From.Categories (Category) loop
            declare
               Space    : constant Non_Negative_Real :=
                            Holder.Available_Space (Category);
               Quantity : constant Non_Negative_Real :=
                            Real'Min (Item.Quantity, Space);
            begin
               if Quantity > 0.0 then
                  Holder.Add_Cargo (Item.Cargo.Element, Quantity);
                  Item.Quantity := Item.Quantity - Quantity;
               else
                  Have_Zero := True;
               end if;
            end;
         end loop;

         if Have_Zero then
            declare
               New_List : Cargo_Lists.List;
            begin
               for Item of From.Categories (Category) loop
                  if Item.Quantity > 0.0 then
                     New_List.Append (Item);
                  end if;
               end loop;
               From.Categories (Category) := New_List;
            end;
         end if;
      end loop;
   end Load;

   ----------
   -- Mass --
   ----------

   function Mass
     (Container : Cargo_Container; Category : Cargo_Category)
      return Non_Negative_Real
   is
   begin
      return Total : Non_Negative_Real := 0.0 do
         for Item of Container.Categories (Category) loop
            Total := Total + Item.Quantity * Item.Cargo.Element.Mass;
         end loop;
      end return;
   end Mass;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Container : Cargo_Container; Category : Cargo_Category)
      return Non_Negative_Real
   is
   begin
      return Total : Non_Negative_Real := 0.0 do
         for Item of Container.Categories (Category) loop
            Total := Total + Item.Quantity;
         end loop;
      end return;
   end Quantity;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Container : Cargo_Container; Cargo : Cargo_Interface'Class)
      return Non_Negative_Real
   is
   begin
      for Item of Container.Categories (Cargo.Category) loop
         if Item.Cargo.Element = Cargo then
            return Item.Quantity;
         end if;
      end loop;
      return 0.0;
   end Quantity;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Container : in out Cargo_Container'Class;
      Holder    :        Cargo_Holder_Interface'Class)
   is
      Have_Zero : Boolean := False;
   begin
      for Category in Cargo_Category loop
         for Item of Container.Categories (Category) loop
            declare
               Have     : constant Non_Negative_Real :=
                            Holder.Current_Quantity (Item.Cargo.Element);
               Quantity : constant Non_Negative_Real :=
                            Real'Min (Item.Quantity, Have);
            begin
               if Quantity > 0.0 then
                  Holder.Add_Cargo (Item.Cargo.Element, Quantity);
                  Item.Quantity := Item.Quantity - Quantity;
               else
                  Have_Zero := True;
               end if;
            end;
         end loop;

         if Have_Zero then
            declare
               New_List : Cargo_Lists.List;
            begin
               for Item of Container.Categories (Category) loop
                  if Item.Quantity > 0.0 then
                     New_List.Append (Item);
                  end if;
               end loop;
               Container.Categories (Category) := New_List;
            end;
         end if;
      end loop;
   end Remove;

   ---------------------
   -- Remove_Capacity --
   ---------------------

   procedure Remove_Capacity
     (Container : in out Cargo_Container'Class;
      Holder    :        Cargo_Holder_Interface'Class)
   is
   begin
      for Category in Cargo_Category loop
         declare
            Capacity : constant Non_Negative_Real :=
                         Holder.Available_Space (Category);
            Total    : Non_Negative_Real := 0.0;
         begin
            if Capacity = 0.0 then
               Container.Categories (Category).Clear;
            else
               for Item of Container.Categories (Category) loop
                  Total := Total + Item.Cargo.Element.Tonnage * Item.Quantity;
               end loop;

               if Total > Capacity then
                  for Item of Container.Categories (Category) loop
                     Item.Quantity := Item.Quantity * Capacity / Total;
                  end loop;
               end if;
            end if;
         end;
      end loop;
   end Remove_Capacity;

   ------------------
   -- Remove_Cargo --
   ------------------

   procedure Remove_Cargo
     (Container : in out Cargo_Container;
      Item      : Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is
   begin
      for Element of Container.Categories (Item.Category) loop
         if Element.Cargo.Element = Item then
            Element.Quantity := Element.Quantity - Quantity;
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "no " & Item.Tag & " in " & Container.Content_Summary;
   end Remove_Cargo;

   ------------------
   -- Remove_Cargo --
   ------------------

   procedure Remove_Cargo
     (From  : in out Cargo_Container;
      Cargo : Cargo_Container'Class)
   is
      Have_Zero : Boolean := False;
   begin
      for Category in Cargo_Category loop
         for Item of From.Categories (Category) loop
            declare
               Quantity : constant Non_Negative_Real :=
                            Real'Min (Item.Quantity,
                                      Cargo.Current_Quantity
                                        (Item.Cargo.Element));
            begin
               Have_Zero := Have_Zero or else Quantity = 0.0;
               Item.Quantity := Item.Quantity - Quantity;
            end;
         end loop;

         if Have_Zero then
            declare
               New_List : Cargo_Lists.List;
            begin
               for Item of From.Categories (Category) loop
                  if Item.Quantity > 0.0 then
                     New_List.Append (Item);
                  end if;
               end loop;
               From.Categories (Category) := New_List;
            end;
         end if;
      end loop;
   end Remove_Cargo;

   -------------
   -- Tonnage --
   -------------

   function Tonnage
     (Container : Cargo_Container; Category : Cargo_Category)
      return Non_Negative_Real
   is
   begin
      return Total : Non_Negative_Real := 0.0 do
         for Item of Container.Categories (Category) loop
            Total := Total + Item.Quantity * Item.Cargo.Element.Tonnage;
         end loop;
      end return;
   end Tonnage;

   ----------------
   -- Total_Mass --
   ----------------

   function Total_Mass (Container : Cargo_Container) return Non_Negative_Real
   is
   begin
      return Total : Non_Negative_Real := 0.0 do
         for List of Container.Categories loop
            for Item of List loop
               Total := Total + Item.Quantity * Item.Cargo.Element.Mass;
            end loop;
         end loop;
      end return;
   end Total_Mass;

   -------------------
   -- Total_Tonnage --
   -------------------

   function Total_Tonnage
     (Container : Cargo_Container) return Non_Negative_Real
   is
   begin
      return Total : Non_Negative_Real := 0.0 do
         for List of Container.Categories loop
            for Item of List loop
               Total := Total + Item.Quantity * Item.Cargo.Element.Tonnage;
            end loop;
         end loop;
      end return;
   end Total_Tonnage;

end Athena.Cargo;
