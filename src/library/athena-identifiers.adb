with Minerva.Db;
with Minerva.Next_Identifier;

package body Athena.Identifiers is

   Template : constant Object_Identifier := "0AA00AA0";

   protected Identifier_Source is

      procedure Next_Identifier (Id : out Object_Identifier);

   private
      Current : Minerva.Next_Identifier.Next_Identifier_Handle;
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

         Next_Id : Object_Identifier;

      begin
         if not Current.Has_Element then
            declare
               use Minerva.Next_Identifier;
               Class : constant Next_Identifier_Class :=
                         First_By_Top_Record
                           (Minerva.Db.R_Next_Identifier);
            begin
               if Class.Has_Element then
                  Current := Class.To_Next_Identifier_Handle;
               else
                  Current := Create (Template);
               end if;
            end;
         end if;

         Next_Id := Current.Next;
         Id := Next_Id;

         for Ch of reverse Next_Id loop
            exit when not Inc (Ch);
         end loop;

         Current.Update_Next_Identifier
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
