with Athena.Logging;

with Athena.Handles.Relationship;

package body Athena.Treaties is

   function Get_Relationship
     (From_Empire, To_Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.Relationship.Relationship_Handle;

   ------------
   -- At_War --
   ------------

   function At_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
      return Boolean
   is
   begin
      return Get_Relationship (E1, E2).War;
   end At_War;

   -----------------
   -- Declare_War --
   -----------------

   procedure Declare_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
   is
      R_To        : constant Athena.Handles.Relationship.Relationship_Handle :=
                      Get_Relationship (E1, E2);
      R_From      : constant Athena.Handles.Relationship.Relationship_Handle :=
                      Get_Relationship (E2, E1);
      New_Opinion : constant Integer :=
                      R_From.Opinion - 100;
   begin
      R_To.Set_War (True);
      R_From.Set_War (True);
      R_From.Set_Opinion (New_Opinion);

      Athena.Handles.War.Create
        (Attacker => E1,
         Defender => E2);

      Athena.Logging.Log
        (E1.Name & " declared war on " & E2.Name);
   end Declare_War;

   ----------------------
   -- Get_Relationship --
   ----------------------

   function Get_Relationship
     (From_Empire, To_Empire : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.Relationship.Relationship_Handle
   is
   begin
      return Athena.Handles.Relationship.Find_Relationship
        (From_Empire, To_Empire);
   end Get_Relationship;

   -------------
   -- Get_War --
   -------------

   function Get_War
     (E1, E2 : Athena.Handles.Empire.Empire_Handle)
      return Athena.Handles.War.War_Handle
   is
      War : constant Athena.Handles.War.War_Handle :=
              Athena.Handles.War.Find_War (E1, E2);
   begin
      if not War.Has_Element then
         Athena.Logging.Log
           ("warning: encounter but no war between "
            & E1.Name & " and " & E2.Name);
      end if;

      return War;
   end Get_War;

end Athena.Treaties;
