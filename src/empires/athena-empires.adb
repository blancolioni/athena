with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Real_Images;
with Athena.Turns;

with Minerva.Empire_Capital;
with Minerva.Empire_Manager;
with Minerva.Empire_Tec;
with Minerva.Star_Knowledge;
with Minerva.System_Designs;

package body Athena.Empires is

   Log_Cash : constant Boolean := True;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   --------------------
   -- Add_Investment --
   --------------------

   procedure Add_Investment
     (Empire     : Minerva.Empire.Empire_Class;
      Technology : Minerva.Technology.Technology_Class;
      Construct  : Non_Negative_Real)
   is
      use Athena.Elementary_Functions;
      Empire_Tec : constant Minerva.Empire_Tec.Empire_Tec_Handle :=
                     Minerva.Empire_Tec.Get_By_Empire_Tec
                       (Empire, Technology);
      Old_Level  : constant Non_Negative_Real :=
                     Empire_Tec.Level;
      New_Level  : constant Non_Negative_Real :=
                     Log (2.0 ** (Old_Level - 1.0) + Construct / 300.0)
                     / Log (2.0)
                     + 1.0;
      New_Investment : constant Non_Negative_Real :=
                         Empire_Tec.Investment + Construct;
   begin

      if Log_Cash then
         Athena.Logging.Log
           (Empire.Name
            & " invests " & Image (Construct)
            & " construct on " & Technology.Tag & " technology "
            & " and increases from " & Image (Old_Level)
            & " to " & Image (New_Level));
      end if;

      Empire_Tec.Update_Empire_Tec
        .Set_Level (New_Level)
        .Set_Investment (New_Investment)
        .Done;

   end Add_Investment;

   -------------
   -- Capital --
   -------------

   function Capital
     (Of_Empire : Minerva.Empire.Empire_Class)
      return Minerva.Star.Star_Class
   is
   begin
      return Minerva.Empire_Capital.Get_By_Empire (Of_Empire).Star;
   end Capital;

   -------------
   -- Capital --
   -------------

   function Capital
     (Of_Empire : Minerva.Empire.Empire_Class)
      return Minerva.Colony.Colony_Handle
   is
   begin
      return Minerva.Colony.Get_By_Star
        (Capital (Of_Empire));
   end Capital;

   -----------------------
   -- Current_Tec_Level --
   -----------------------

   function Current_Tec_Level
     (Empire     : Minerva.Empire.Empire_Class;
      Technology : Minerva.Technology.Technology_Class)
      return Non_Negative_Real
   is
   begin
      return Minerva.Empire_Tec.Get_By_Empire_Tec
        (Empire, Technology)
        .Level;
   end Current_Tec_Level;

   ---------------------
   -- Disable_Manager --
   ---------------------

   procedure Disable_Manager
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class)
   is
      Empire_Manager : constant Minerva.Empire_Manager.Empire_Manager_Handle :=
                         Minerva.Empire_Manager.Get_By_Empire_Manager
                           (Empire, Manager);
   begin
      if Empire_Manager.Has_Element
        and then Empire_Manager.Enabled
      then
         Empire_Manager.Update_Empire_Manager
           .Set_Enabled (False)
           .Done;
      end if;
   end Disable_Manager;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Empire      : Minerva.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String)
   is
      use type Athena.Money.Money_Type;
      New_Cash : constant Athena.Money.Money_Type :=
                   Empire.Cash + Amount;
   begin
      if Log_Cash then
         Athena.Logging.Log
           (Empire.Name & ": earn " & Athena.Money.Show (Amount)
            & " for " & Description
            & ": cash now " & Athena.Money.Show (New_Cash));
      end if;

      Empire.Update_Empire
        .Set_Cash (New_Cash)
        .Done;
   end Earn;

   --------------------
   -- Enable_Manager --
   --------------------

   procedure Enable_Manager
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class)
   is
      Empire_Manager : constant Minerva.Empire_Manager.Empire_Manager_Handle :=
                         Minerva.Empire_Manager.Get_By_Empire_Manager
                           (Empire, Manager);
   begin
      if Empire_Manager.Has_Element
        and then not Empire_Manager.Enabled
      then
         Empire_Manager.Update_Empire_Manager
           .Set_Enabled (True)
           .Done;
      else
         Minerva.Empire_Manager.Create
           (Empire  => Empire,
            Manager => Manager,
            Enabled => True,
            Script  => "");
      end if;
   end Enable_Manager;

   ---------
   -- Pay --
   ---------

   procedure Pay
     (Empire      : Minerva.Empire.Empire_Class;
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

      if Log_Cash then
         Athena.Logging.Log
           (Empire.Name & ": pay " & Athena.Money.Show (Amount)
            & " for " & Description
            & ": remaining cash " & Athena.Money.Show (New_Cash)
            & "; debt " & Athena.Money.Show (New_Debt));
      end if;

      Empire.Update_Empire
        .Set_Cash (New_Cash)
        .Set_Debt (New_Debt)
        .Done;

   end Pay;

   ------------------------
   -- Set_Manager_Script --
   ------------------------

   procedure Set_Manager_Script
     (Empire  : Minerva.Empire.Empire_Class;
      Manager : Minerva.Manager.Manager_Class;
      Script  : String)
   is
      Empire_Manager : constant Minerva.Empire_Manager.Empire_Manager_Handle :=
                         Minerva.Empire_Manager.Get_By_Empire_Manager
                           (Empire, Manager);
   begin
      if Empire_Manager.Has_Element then
         Empire_Manager.Update_Empire_Manager
           .Set_Script (Script)
           .Done;
      else
         Minerva.Empire_Manager.Create
           (Empire  => Empire,
            Name    => Script,
            Manager => Manager,
            Enabled => False,
            Script  => Script);
      end if;
   end Set_Manager_Script;

   --------------------------------
   -- Standard_Battleship_Design --
   --------------------------------

   function Standard_Battleship_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Battleship;
   end Standard_Battleship_Design;

   -----------------------------
   -- Standard_Carrier_Design --
   -----------------------------

   function Standard_Carrier_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Carrier;
   end Standard_Carrier_Design;

   -----------------------------
   -- Standard_Cruiser_Design --
   -----------------------------

   function Standard_Cruiser_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Cruiser;
   end Standard_Cruiser_Design;

   ------------------------------
   -- Standard_Defender_Design --
   ------------------------------

   function Standard_Defender_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Defender;
   end Standard_Defender_Design;

   -------------------------------
   -- Standard_Destroyer_Design --
   -------------------------------

   function Standard_Destroyer_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Destroyer;
   end Standard_Destroyer_Design;

   ---------------------------
   -- Standard_Recon_Design --
   ---------------------------

   function Standard_Recon_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Recon;
   end Standard_Recon_Design;

   ---------------------------
   -- Standard_Scout_Design --
   ---------------------------

   function Standard_Scout_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Scout;
   end Standard_Scout_Design;

   -------------------------------
   -- Standard_Transport_Design --
   -------------------------------

   function Standard_Transport_Design
     (Empire : Minerva.Empire.Empire_Class)
      return Minerva.Ship_Design.Ship_Design_Class
   is
   begin
      return Minerva.System_Designs.Get_By_Empire (Empire).Transport;
   end Standard_Transport_Design;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Empire : Minerva.Empire.Empire_Class;
      Star   : Minerva.Star.Star_Class)
   is
      Knowledge : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
                    Minerva.Star_Knowledge.Get_By_Star_Knowledge
                      (Star, Empire);
      Colony    : constant Minerva.Colony.Colony_Class :=
                    Minerva.Colony.Get_By_Star (Star);
      Pop       : constant Non_Negative_Real :=
                    (if Colony.Has_Element then Colony.Population else 0.0);
      Ind       : constant Non_Negative_Real :=
                    (if Colony.Has_Element then Colony.Industry else 0.0);
   begin
      if Knowledge.Has_Element then
         Knowledge.Update_Star_Knowledge
           .Set_Owner (Star.Owner)
           .Set_Last_Visit (Athena.Turns.Current_Turn)
           .Set_Last_Pop (Pop)
           .Set_Last_Ind (Ind)
           .Set_Visited (True)
           .Done;
      else
         Minerva.Star_Knowledge.Create
           (Star       => Star,
            Empire     => Empire,
            Owner      => Star.Owner,
            Last_Visit => Athena.Turns.Current_Turn,
            Last_Pop   => Pop,
            Last_Ind   => Ind,
            Visited    => True,
            Colonizing => False);
      end if;
   end Visit;

   -------------
   -- Visited --
   -------------

   function Visited
     (Empire : Minerva.Empire.Empire_Class;
      Star   : Minerva.Star.Star_Class)
      return Boolean
   is
      Knowledge : constant Minerva.Star_Knowledge.Star_Knowledge_Class :=
                    Minerva.Star_Knowledge.Get_By_Star_Knowledge
                      (Star, Empire);
   begin
      return Knowledge.Has_Element
        and then Knowledge.Visited;
   end Visited;

end Athena.Empires;
