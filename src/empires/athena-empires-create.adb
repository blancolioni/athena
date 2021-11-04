with Athena.Identifiers;
with Athena.Money;
with Athena.Ships.Create;
with Athena.Turns;

with Minerva.Colony;
with Minerva.Empire;
with Minerva.Empire_Capital;
with Minerva.Empire_Manager;
with Minerva.Empire_Tec;
with Minerva.Fleet;
with Minerva.Ship_Design;
with Minerva.Star_Knowledge;
with Minerva.System_Designs;

package body Athena.Empires.Create is

   ----------------
   -- New_Empire --
   ----------------

   procedure New_Empire
     (Star      : Minerva.Star.Star_Class;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : Athena.Color.Athena_Color;
      Template  : Tropos.Configuration)
   is
      Init : constant Tropos.Configuration :=
        Template.Child ("init");
      Ships : constant Tropos.Configuration :=
                Template.Child ("ships");

      function Get (Name : String) return Real
      is (Real (Long_Float'(Init.Get (Name, 0.0))));

      function Design
        (Tag : String)
         return Minerva.Ship_Design.Ship_Design_Class;

      ------------
      -- Design --
      ------------

      function Design
        (Tag : String)
         return Minerva.Ship_Design.Ship_Design_Class
      is
         Config : constant Tropos.Configuration :=
                    Template
                      .Child ("standard-designs")
                      .Child (Tag);
         Name   : constant String :=
                    (if Config.Child_Count > 0
                     then Config.Value
                     else "");
         Result : constant Minerva.Ship_Design.Ship_Design_Class :=
                    Minerva.Ship_Design.First_By_Name (Name);
      begin
         return Result;
      end Design;

      Empire         : constant Minerva.Empire.Empire_Handle :=
                         Minerva.Empire.Create
                           (Identifier => Athena.Identifiers.Next_Identifier,
                            Name       => Name,
                            Plural     => Plural,
                            Adjective  => Adjective,
                            Cash       =>
                              Athena.Money.To_Money (Get ("cash")),
                            Debt       =>
                              Athena.Money.To_Money (Get ("debt")),
                            Rgb        =>
                              Athena.Color.To_Natural (Color));

   begin

      Star.Update_Star
        .Set_Name (Capital)
        .Set_Owner (Empire)
        .Done;

      Minerva.Empire_Capital.Create
        (Empire => Empire,
         Star   => Star);

      declare
         procedure Set_Manager
           (Name : String);

         -----------------
         -- Set_Manager --
         -----------------

         procedure Set_Manager
           (Name : String)
         is
         begin
            Set_Manager_Script
              (Empire, Minerva.Manager.Get_By_Tag (Name), Name);
            Enable_Manager
              (Empire  => Empire,
               Manager => Minerva.Manager.Get_By_Tag (Name));
         end Set_Manager;

      begin
         Set_Manager ("attack");
         Set_Manager ("colonize");
         Set_Manager ("defend");
         Set_Manager ("develop");
         Set_Manager ("explore");
         Set_Manager ("research");
         Set_Manager ("transport");
         Set_Manager ("upgrade");
      end;

      for Technology of Minerva.Technology.Scan_By_Tag loop
         Minerva.Empire_Tec.Create
           (Empire     => Empire,
            Technology => Technology,
            Investment => 0.0,
            Level      => 1.0);
      end loop;

      Minerva.Colony.Create
        (Identifier => Athena.Identifiers.Next_Identifier,
         Star       => Star,
         Empire     => Empire,
         Founded    => Athena.Turns.Current_Turn,
         Construct  => Get ("construct"),
         Population => Get ("pop"),
         Industry   => Get ("ind"),
         Material   => Get ("material"));

      Minerva.Star_Knowledge.Create
        (Star       => Star,
         Empire     => Empire,
         Owner      => Empire,
         Last_Visit => Athena.Turns.Current_Turn,
         Last_Pop   => Get ("pop"),
         Last_Ind   => Get ("ind"),
         Visited    => True,
         Colonizing => False);

      Minerva.System_Designs.Create
        (Empire     => Empire,
         Scout      => Design ("scout"),
         Recon      => Design ("recon"),
         Transport  => Design ("transport"),
         Defender   => Design ("defender"),
         Destroyer  => Design ("destroyer"),
         Cruiser    => Design ("cruiser"),
         Battleship => Design ("battleship"),
         Carrier    => Design ("carrier"));

      for Ship_Config of Ships loop
         declare
            Design : constant Minerva.Ship_Design.Ship_Design_Class :=
                       Minerva.Ship_Design.First_By_Name
                         (Ship_Config.Config_Name);
            Manager : constant Minerva.Manager.Manager_Class :=
                        Design.Default_Manager;
         begin
            for I in 1 .. Ship_Config.Value loop
               Athena.Ships.Create.Create_Ship
                 (Empire      => Empire,
                  Star        => Star,
                  Design      => Design,
                  Fleet       => Minerva.Fleet.Empty_Handle,
                  Manager     =>
                    Minerva.Empire_Manager.Get_By_Empire_Manager
                      (Empire, Manager),
                  Name        => Design.Name,
                  Destination => Minerva.Star.Empty_Handle);
            end loop;
         end;
      end loop;

   end New_Empire;

end Athena.Empires.Create;
