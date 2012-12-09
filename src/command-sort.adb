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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed.Less_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Bibentry;
with Bibliography_Library;
with Config;
with In_Out_Arg_Helper;
with Command_Line_Parser;

package body Command.Sort is
   Cmd_Parser : Command_Line_Parser.Command_Line_Parser_Type;

   pragma Warnings(Off, "formal parameter ""Cmd"" is not referenced");
   function Get_Name(Cmd : Sort_Type) return String is
   begin
      return Command_Name;
   end Get_Name;

   procedure Print_Help(Cmd : Sort_Type) is
      use Ada.Text_IO;
      use Ada.Characters.Latin_1;
      use Ada.Strings.Unbounded;
   begin
      Put_Line(Config.Name & " " & Command_Name &
                 " <arguments>");
      New_Line;
      Put_Line("Allowed arguments are:");
      for c of Command_Line_Parser.Get_Registered_Arguments(Cmd_Parser) loop
         Put_Line(HT & "-" & c & HT & Command_Line_Parser.Get_Help_Message(Cmd_Parser, c));
      end loop;
   end Print_Help;

   procedure Execute(Cmd : Sort_Type) is
      use Ada.Strings.Unbounded;

      Bibfile : Bibliography_Library.Bibliography_Library_Type;

      function Compare_By_Key(Left, Right : Bibentry.Bibentry_Type'Class) return Boolean is

      begin
         return Ada.Strings.Fixed.Less_Case_Insensitive(Left.Get_Bibtex_Key, Right.Get_Bibtex_Key);
      end Compare_By_Key;

      procedure Sort_By_Key is new Bibliography_Library.Sort(Compare_By_Key);
   begin
      Command_Line_Parser.Parse(Cmd_Parser);


      Bibfile.Load_From_Bibtex_File(To_String(In_Out_Arg_Helper.arg_spec.input_path));

      Sort_By_Key(Bibfile);

      Bibfile.Save_To_Bibtex_File(To_String(In_Out_Arg_Helper.arg_spec.output_path));
   end Execute;
   pragma Warnings(On, "formal parameter ""Cmd"" is not referenced");

begin
   declare
      Cmd : Sort_Type;
   begin
      -- Initialize arguments
      Command_Line_Parser.Register_Callback(Parser        => Cmd_Parser,
                                            Argument_Name => 'i',
                                            Callback      => In_Out_Arg_Helper.Parse_M_I'Access,
                                            Help_Msg      => "Input file path",
                                            Required      => true);

      Command_Line_Parser.Register_Callback(Parser        => Cmd_Parser,
                                            Argument_Name => 'o',
                                            Callback      => In_Out_Arg_Helper.Parse_M_O'Access,
                                            Help_Msg      => "Output file path",
                                            Required      => false);

      Command_Line_Parser.Register_Callback(Parser        => Cmd_Parser,
                                            Argument_Name => 'e',
                                            Callback      => In_Out_Arg_Helper.Parse_M_E'Access,
                                            Help_Msg      => "Edit input file path in place",
                                            Required      => false);

      Register_Command(Cmd);
   end;
end Command.Sort;
