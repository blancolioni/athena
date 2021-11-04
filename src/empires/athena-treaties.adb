with Athena.Turns;

with Minerva.Relationship;
with Minerva.Turn;

package body Athena.Treaties is

   ------------
   -- At_War --
   ------------

   function At_War
     (Empire_1, Empire_2 : Minerva.Empire.Empire_Class) return Boolean
   is
      Relationship : constant Minerva.Relationship.Relationship_Class :=
                       Minerva.Relationship.Get_By_Relationship
                         (Empire_1, Empire_2);
   begin
      return Relationship.Has_Element
        and then Relationship.War;
   end At_War;

   -----------------
   -- Declare_War --
   -----------------

   procedure Declare_War
     (Aggressor, Defender : Minerva.Empire.Empire_Class)
   is
      To_Defender : constant Minerva.Relationship.Relationship_Class :=
                      Minerva.Relationship.Get_By_Relationship
                        (Aggressor, Defender);
      To_Aggressor : constant Minerva.Relationship.Relationship_Class :=
                       Minerva.Relationship.Get_By_Relationship
                         (Defender, Aggressor);
   begin
      if To_Defender.Has_Element then
         To_Defender.Update_Relationship
           .Set_War (True)
           .Done;
      else
         Minerva.Relationship.Create
           (From    => Aggressor,
            To      => Defender,
            Opinion => 0,
            War     => True,
            Hostile => False,
            Allied  => False,
            Trade   => False);
      end if;
      if To_Aggressor.Has_Element then
         To_Aggressor.Update_Relationship
           .Set_War (True)
           .Done;
      else
         Minerva.Relationship.Create
           (From    => Defender,
            To      => Aggressor,
            Opinion => 0,
            War     => True,
            Hostile => False,
            Allied  => False,
            Trade   => False);
      end if;

      Minerva.War.Create
        (Attacker => Aggressor,
         Defender => Defender,
         Start    => Athena.Turns.Current_Turn,
         Finish   => Minerva.Turn.Empty_Handle);

   end Declare_War;

   -------------
   -- Get_War --
   -------------

   function Get_War
     (Empire_1, Empire_2 : Minerva.Empire.Empire_Class)
      return Minerva.War.War_Class
   is
   begin
      if Minerva.War.Is_War
        (Empire_1, Empire_2, Minerva.Turn.Empty_Handle)
      then
         return Minerva.War.Get_By_War
           (Empire_1, Empire_2, Minerva.Turn.Empty_Handle);
      else
         return Minerva.War.Get_By_War
           (Empire_2, Empire_1, Minerva.Turn.Empty_Handle);
      end if;
   end Get_War;

end Athena.Treaties;
