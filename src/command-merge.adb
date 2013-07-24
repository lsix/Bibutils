-------------------------------------------------------------------------------
-- Copyright Lancelot SIX                                                    --
--                                                                           --
-- lancelot@lancelotsix.com                                                  --
--                                                                           --
-- This software is governed by the CeCILL license under French law and      --
-- abiding by the rules of distribution of free software.  You can  use,     --
-- modify and/ or redistribute the software under the terms of the CeCILL    --
-- license as circulated by CEA, CNRS and INRIA at the following URL         --
-- "http://www.cecill.info".                                                 --
--                                                                           --
-- As a counterpart to the access to the source code and  rights to copy,    --
-- modify and redistribute granted by the license, users are provided only   --
-- with a limited warranty  and the software's author,  the holder of the    --
-- economic rights,  and the successive licensors  have only  limited        --
-- liability.                                                                --
--                                                                           --
-- In this respect, the user's attention is drawn to the risks associated    --
-- with loading,  using,  modifying and/or developing or reproducing the     --
-- software by the user in light of its specific status of free software,    --
-- that may mean  that it is complicated to manipulate,  and  that  also     --
-- therefore means  that it is reserved for developers  and  experienced     --
-- professionals having in-depth computer knowledge. Users are therefore     --
-- encouraged to load and test the software's suitability as regards their   --
-- requirements in conditions enabling the security of their systems and/or  --
-- data to be ensured and,  more generally, to use and operate it in the     --
-- same conditions as regards security.                                      --
--                                                                           --
-- The fact that you are presently reading this means that you have had      --
-- knowledge of the CeCILL license and that you accept its terms.            --

with Ada.Text_IO; use Ada.Text_IO;
with Command_Line_Parser;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Bibentry;
with Bibliography_Library;
with Bibliography_Library_Merge;
with Ada.Strings.Less_Case_Insensitive;

package body Command.Merge is
   Cmd_Parser   : Command_Line_Parser.Command_Line_Parser_Type;
   Out_Path     : Ada.Strings.Unbounded.Unbounded_String;

   function Get_Name   (Cmd : Merge_Command_Type) return String is
      pragma Unreferenced (Cmd);
   begin
      return Cmd_Name;
   end Get_Name;

   procedure Print_Help (Cmd : Merge_Command_Type) is
      pragma Unreferenced (Cmd);
   begin
      Put_Line("Merge -o <result_file.bib> <source_file> [other_sources]");
   end Print_Help;

   ----------------------------------------------------------------------------
   -- Execute
   procedure Execute(Cmd : in out Merge_Command_Type) is
      pragma Unreferenced(Cmd);

      function Decide_between_two_entries(Left, Right : Bibentry.Bibentry_Type) return
        Bibliography_Library_Merge.Strict_Choice_Type is

         c : Character;
      begin
         Put_Line("There is an ambiguity between a:" &
                    Bibentry.Get_Bibtex_Key(Left) & " and b:" &
                    Bibentry.Get_Bibtex_Key(Right));
         New_Line;
         Put_Line("a:");
         Put_Line(Bibentry.To_String(Left));
         New_Line;
         Put_Line("b:");
         Put_Line(Bibentry.To_String(Right));
         New_Line;
         Put("Use a(a), b(b) or both(e) : ");
         Ada.Text_IO.Get(c);
         while c /= 'a' and c /= 'b' and c /= 'e' loop
            Put_Line("Invalid answer. Please retry");
            Put("Use a(a), b(b) or both(e) : ");
            Ada.Text_IO.Get(c);
         end loop;
         case c is
         when 'a' =>
            return Bibliography_Library_Merge.Left;
         when 'b' =>
            return Bibliography_Library_Merge.Right;
         when 'e' =>
            return Bibliography_Library_Merge.Both;
            when others =>
               raise Program_Error with "Invalid state";
         end case;
      end Decide_between_two_entries;

      function Comp(Left, Right : Bibentry.Bibentry_Type) return Boolean is
      begin
         return Ada.Strings.Less_Case_Insensitive(Bibentry.Get_Bibtex_Key(Left),
                                                  Bibentry.Get_Bibtex_Key(Right));
      end Comp;

      procedure Merge is new Bibliography_Library_Merge.Strinct_Append(Decide_Fct => Decide_between_two_entries,
                                                                       "<"        => Comp);
      procedure Sort is new Bibliography_Library.Sort("<" => Comp);

      Acc : Bibliography_Library.Bibliography_Library_Type;
      Oth : Bibliography_Library.Bibliography_Library_Type;
   begin
      Command_Line_Parser.Parse(Cmd_Parser);

      Put_Line("Merging into " & Ada.Strings.Unbounded.To_String(Out_Path));
      Put(ASCII.HT & "Merging " & Ada.Command_Line.Argument(1));
      Bibliography_Library.Load_From_Bibtex_File(Acc,
                                                 Ada.Command_Line.Argument(1));
      Put_Line(" (" & Natural'Image(Bibliography_Library.Length(Acc)) & ")");
      Sort(Acc);
      for i in 2..Ada.Command_Line.Argument_Count loop
         Put(ASCII.HT & "Merging " & Ada.Command_Line.Argument(i));
         Bibliography_Library.Load_From_Bibtex_File(Oth,
                                                    Ada.Command_Line.Argument(i));
         Sort(Oth);
         Merge(Accum => Acc,
               Other => Oth);
         Put_Line(" (" & Natural'Image(Bibliography_Library.Length(Oth)) & " -> " & Natural'Image(Bibliography_Library.Length(Acc)) & ")");
         Bibliography_Library.Clear(Oth);
      end loop;

      Bibliography_Library.Save_To_Bibtex_File(Acc,
                                               Ada.Strings.Unbounded.To_String(Out_Path));
   end Execute;

   -- Callback used to set the out file path
   procedure Set_Out_File_Path(c : Character; Pth : String) is
   begin
      if c = 'o' then
         Out_Path := Ada.Strings.Unbounded.To_Unbounded_String(Pth);
      end if;
   end Set_Out_File_Path;

   procedure Init is
   begin
      Command_Line_Parser.Register_Callback(Parser        => Cmd_Parser,
                                            Argument_Name => 'o',
                                            Callback      => Set_Out_File_Path'Access,
                                            Help_Msg      => "File to store the result",
                                            Required      => true);
      Register_Command(Instance'Access);
   end Init;

end Command.Merge;
