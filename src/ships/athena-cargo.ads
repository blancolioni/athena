private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

package Athena.Cargo is

   type Cargo_Category is (Commodity, Fuel, People);

   type Cargo_Interface is interface;

   function Tag
     (Cargo : Cargo_Interface)
      return String
      is abstract;

   function Tonnage
     (Cargo : Cargo_Interface)
      return Non_Negative_Real
      is abstract;

   function Mass
     (Cargo : Cargo_Interface)
      return Non_Negative_Real
      is abstract;

   function Category
     (Cargo : Cargo_Interface)
      return Cargo_Category
      is abstract;

   type Cargo_Holder_Interface is interface;

   function Cargo_Space
     (Holder   : Cargo_Holder_Interface;
      Category : Cargo_Category)
      return Non_Negative_Real
      is abstract;

   function Current_Tonnage
     (Holder   : Cargo_Holder_Interface;
      Category : Cargo_Category)
      return Non_Negative_Real
      is abstract;

   function Available_Space
     (Holder   : Cargo_Holder_Interface'Class;
      Category : Cargo_Category)
      return Non_Negative_Real;

   function Current_Quantity
     (Holder : Cargo_Holder_Interface;
      Item   : Cargo_Interface'Class)
      return Non_Negative_Real
      is abstract;

   procedure Add_Cargo
     (Holder    : Cargo_Holder_Interface;
      Item      : Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is abstract;

   procedure Remove_Cargo
     (Holder    : Cargo_Holder_Interface;
      Item      : Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
   is abstract;

   type Cargo_Container is tagged private;

   function Empty_Container return Cargo_Container;

   function Current_Quantity
     (Container : Cargo_Container;
      Item      : Cargo_Interface'Class)
      return Non_Negative_Real;

   procedure Add_Cargo
     (Container : in out Cargo_Container;
      Item      : Cargo_Interface'Class;
      Quantity  : Non_Negative_Real);

   procedure Remove_Cargo
     (Container : in out Cargo_Container;
      Item      : Cargo_Interface'Class;
      Quantity  : Non_Negative_Real)
     with Pre => Quantity <= Container.Current_Quantity (Item);

   procedure Remove_Cargo
     (From  : in out Cargo_Container;
      Cargo : Cargo_Container'Class);

   function Content_Summary
     (Cargo : Cargo_Container)
     return String;

   procedure Load
     (Holder    : Cargo_Holder_Interface'Class;
      From      : in out Cargo_Container'Class);

   procedure Remove
     (Container : in out Cargo_Container'Class;
      Holder    : Cargo_Holder_Interface'Class);
   --  any cargo held by Holder is removed from Container

   procedure Remove_Capacity
     (Container : in out Cargo_Container'Class;
      Holder    : Cargo_Holder_Interface'Class);
   --  container contents lowered according to capacity of holder

   procedure Iterate
     (Container : Cargo_Container;
      Process   : not null access
        procedure (Cargo : Cargo_Interface'Class;
                   Quantity : Non_Negative_Real));

   function Is_Empty
     (Container : Cargo_Container)
      return Boolean;

   function Total_Tonnage
     (Container : Cargo_Container)
      return Non_Negative_Real;

   function Total_Mass
     (Container : Cargo_Container)
      return Non_Negative_Real;

   function Tonnage
     (Container : Cargo_Container;
      Category  : Cargo_Category)
      return Non_Negative_Real;

   function Mass
     (Container : Cargo_Container;
      Category  : Cargo_Category)
      return Non_Negative_Real;

   function Quantity
     (Container : Cargo_Container;
      Category  : Cargo_Category)
      return Non_Negative_Real;

   function Quantity
     (Container : Cargo_Container;
      Cargo     : Cargo_Interface'Class)
      return Non_Negative_Real;

private

   package Cargo_Holders is
     new Ada.Containers.Indefinite_Holders (Cargo_Interface'Class);

   type Cargo_Record is
      record
         Cargo    : Cargo_Holders.Holder;
         Quantity : Non_Negative_Real;
      end record;

   package Cargo_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cargo_Record);

   type Category_Cargo_Lists is
     array (Cargo_Category) of Cargo_Lists.List;

   type Cargo_Container is tagged
      record
         Categories : Category_Cargo_Lists;
      end record;

end Athena.Cargo;
