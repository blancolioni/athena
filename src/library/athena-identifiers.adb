with Athena.Db.Next_Identifier;

package body Athena.Identifiers is

   Template : constant Object_Identifier := "0AA00AA0";

   protected Identifier_Source is

      procedure Next_Identifier (Id : out Object_Identifier);

   private
      Current : Athena.Db.Next_Identifier_Reference :=
        Athena.Db.Null_Next_Identifier_Reference;

   end Identifier_Source;

   -----------------------
   -- Identifier_Source --
   -----------------------

   protected body Identifier_Source is

      ---------------------
      -- Next_Identifier --
      ---------------------

      procedure Next_Identifier (Id : out Object_Identifier) is

         function Inc (Ch : in out Character) return Boolean
           with Pre => Ch in 'A' .. 'Z' | '0' .. '9';

         ---------
         -- Inc --
         ---------

         function Inc (Ch : in out Character) return Boolean is
         begin
            if Ch = 'Z' then
               Ch := 'A';
               return True;
            elsif Ch = '9' then
               Ch := '0';
               return True;
            else
               Ch := Character'Succ (Ch);
               return False;
            end if;
         end Inc;

         use Athena.Db;
         Next_Id : Object_Identifier;

      begin
         if Current = Null_Next_Identifier_Reference then
            Current :=
              Athena.Db.Next_Identifier.First_Reference_By_Top_Record
                (Athena.Db.R_Next_Identifier);
         end if;

         if Current = Null_Next_Identifier_Reference then
            Current :=
              Athena.Db.Next_Identifier.Create
                (Template);
         end if;

         Next_Id := Athena.Db.Next_Identifier.Get (Current).Next;
         Id := Next_Id;

         for Ch of reverse Next_Id loop
            exit when not Inc (Ch);
         end loop;

         Athena.Db.Next_Identifier.Update_Next_Identifier (Current)
           .Set_Next (Next_Id)
           .Done;

      end Next_Identifier;
   end Identifier_Source;

   ---------------------
   -- Next_Identifier --
   ---------------------

   function Next_Identifier return Object_Identifier is
   begin
      return Id : Object_Identifier do
         Identifier_Source.Next_Identifier (Id);
      end return;
   end Next_Identifier;

end Athena.Identifiers;
