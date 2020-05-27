with Ada.Streams.Stream_IO;

with Athena.Money;

package Athena.Handles.Component is

   type Component_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
     and Localised_Interface
   with private;

   function Empty_Handle
      return Component_Handle;

   function Reference
     (Handle : Component_Handle)
      return Component_Reference;

   function Get
     (Reference : Component_Reference)
      return Component_Handle;

   function Tonnage
     (Component : Component_Handle)
      return Non_Negative_Real;

   function Mass
     (Component : Component_Handle)
      return Non_Negative_Real;

   function Price
     (Component : Component_Handle)
      return Athena.Money.Price_Type;

   function Fuel_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real;

   function Idle_Power_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real;

   function Active_Power_Consumption
     (Component : Component_Handle)
      return Non_Negative_Real;

   function Has_Jump
     (Component : Component_Handle)
      return Boolean;

   function Jump
     (Component : Component_Handle)
      return Non_Negative_Real
     with Pre => Component.Has_Jump;

   function Has_Impulse
     (Component : Component_Handle)
      return Boolean;

   function Impulse
     (Component : Component_Handle)
      return Non_Negative_Real
     with Pre => Component.Has_Impulse;

   function Has_Power_Output
     (Component : Component_Handle)
      return Boolean;

   function Power_Output
     (Component : Component_Handle)
      return Non_Negative_Real
     with Pre => Component.Has_Power_Output;

   function Has_Berths
     (Component : Component_Handle)
      return Boolean;

   function Berths
     (Component : Component_Handle)
      return Non_Negative_Real;

   function Get_By_Tag
     (Tag : String)
      return Component_Handle'Class;

   procedure Load
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Save
     (Stream : Ada.Streams.Stream_IO.Stream_Access);

private

   type Component_Handle is
     new Root_Athena_Handle
     and Has_Identifier_Interface
     and Localised_Interface with
      record
         Reference : Component_Reference := 0;
      end record;

   overriding function Identifier
     (Component : Component_Handle)
      return Object_Identifier;

   overriding function Tag
     (Component : Component_Handle)
      return String;

   overriding function Short_Name
     (Component : Component_Handle)
      return String
   is (Component.Tag);

   type Root_Component_Record is abstract tagged
      record
         Identifier        : Object_Identifier;
         Tag               : Ada.Strings.Unbounded.Unbounded_String;
         Tonnage           : Non_Negative_Real;
         Mass              : Non_Negative_Real;
         Price             : Athena.Money.Price_Type;
         Fuel_Consumption  : Non_Negative_Real;
         Idle_Power        : Non_Negative_Real;
         Active_Power      : Non_Negative_Real;
         Berths            : Non_Negative_Real;
      end record;

   function Jump
     (Rec : Root_Component_Record)
      return Non_Negative_Real
   is (0.0);

   function Impulse
     (Rec : Root_Component_Record)
      return Non_Negative_Real
   is (0.0);

   function Power_Output
     (Rec : Root_Component_Record)
      return Non_Negative_Real
   is (0.0);

   procedure Add_Component
     (Component : Root_Component_Record'Class);

   function Get
     (Reference : Component_Reference)
      return Component_Handle
   is (Reference /= 0, Reference);

   function Reference
     (Handle : Component_Handle)
      return Component_Reference
   is (Handle.Reference);

   function Empty_Handle
     return Component_Handle
   is (False, 0);

end Athena.Handles.Component;
