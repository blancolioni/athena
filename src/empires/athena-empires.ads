with Athena.Money;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Design;
with Athena.Handles.Ship;
with Athena.Handles.Star;
with Athena.Handles.Technology;

package Athena.Empires is

   procedure Add_Investment
     (Empire     : Athena.Handles.Empire.Empire_Handle;
      Technology : Athena.Handles.Technology.Technology_Handle;
      Construct  : Non_Negative_Real);

   procedure Pay
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      Amount      : Athena.Money.Money_Type;
      Description : String);

   procedure Earn
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      Amount      : Athena.Money.Money_Type;
      Description : String);

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.Star.Star_Handle;

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.Colony.Colony_Handle;

   function Find_Ship_With_Name
     (Owner : Athena.Handles.Empire.Empire_Handle;
      Name  : String)
      return Athena.Handles.Ship.Ship_Handle;

end Athena.Empires;
