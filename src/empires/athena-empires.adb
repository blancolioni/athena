with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Real_Images;

package body Athena.Empires is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   --------------------
   -- Add_Investment --
   --------------------

   procedure Add_Investment
     (Empire     : Athena.Handles.Empire.Empire_Handle;
      Technology : Athena.Handles.Technology.Technology_Handle;
      Construct  : Non_Negative_Real)
   is
      use Athena.Elementary_Functions;
      Old_Level  : constant Non_Negative_Real :=
                     Empire.Current_Tec_Level (Technology);
      New_Level  : constant Non_Negative_Real :=
                     Log (2.0 ** (Old_Level - 1.0) + Construct / 3000.0)
                     / Log (2.0)
                     + 1.0;
      New_Investment : constant Non_Negative_Real :=
                         Empire.Current_Tec_Investment (Technology)
                         + Construct;
   begin

      Athena.Logging.Log
        (Empire.Name
         & " invests " & Image (Construct)
         & " construct on " & Technology.Tag & " technology "
         & " and increases from " & Image (Old_Level)
         & " to " & Image (New_Level));

      Empire.Update_Tec
        (Technology     => Technology,
         New_Level      => New_Level,
         New_Investment => New_Investment);
   end Add_Investment;

   -------------
   -- Capital --
   -------------

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.Star.Star_Handle
   is
   begin
      return Athena.Handles.Colony.Get (Of_Empire.Capital).World.Star;
   end Capital;

   -------------
   -- Capital --
   -------------

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.Colony.Colony_Handle
   is
   begin
      return Athena.Handles.Colony.Get (Of_Empire.Capital);
   end Capital;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      Amount      : Athena.Money.Money_Type;
      Description : String)
   is
      use type Athena.Money.Money_Type;
      New_Cash : constant Athena.Money.Money_Type :=
                   Empire.Cash + Amount;
   begin
      Athena.Logging.Log
        (Empire.Name & ": earn " & Athena.Money.Show (Amount)
         & " for " & Description
         & ": cash now " & Athena.Money.Show (New_Cash));

      Empire.Set_Cash (New_Cash);
   end Earn;

   ---------
   -- Pay --
   ---------

   procedure Pay
     (Empire      : Athena.Handles.Empire.Empire_Handle;
      Amount      : Athena.Money.Money_Type;
      Description : String)
   is
      use type Athena.Money.Money_Type;
      New_Cash : Athena.Money.Money_Type := Empire.Cash;
      New_Debt : Athena.Money.Money_Type := Empire.Debt;
   begin
      if Amount >= New_Cash then
         New_Debt := New_Debt + (Amount - New_Cash);
         New_Cash := Athena.Money.Zero;
      else
         New_Cash := New_Cash - Amount;
      end if;

      Athena.Logging.Log
        (Empire.Name & ": pay " & Athena.Money.Show (Amount)
         & " for " & Description
         & ": remaining cash " & Athena.Money.Show (New_Cash)
         & "; debt " & Athena.Money.Show (New_Debt));

      Empire.Set_Cash (New_Cash);
      Empire.Set_Debt (New_Debt);

   end Pay;

end Athena.Empires;
