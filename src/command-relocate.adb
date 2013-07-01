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
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;
with Bibentry;
with Bibliography_Library;
with Command_Line_Parser;
with Config;
with File_Attribute_Helper;
with In_Out_Arg_Helper;

package body Command.Relocate is
   Cmd_Parser   : Command_Line_Parser.Command_Line_Parser_Type;
   New_Lib_Path : Ada.Strings.Unbounded.Unbounded_String;

   function Get_Name(Cmd : Relocate_Command_Type) return String is
      pragma Unreferenced (Cmd);
   begin
      return "relocate";
   end Get_Name;


   procedure Print_Help(Cmd : Relocate_Command_Type)is
   begin
      Ada.Text_IO.Put_Line(Config.Name & " " & Get_Name(Cmd) & " <options>");
      Ada.Text_IO.Put_Line(Ada.Characters.Latin_1.HT & "Updates the file attribute of each entry.");
      Ada.Text_IO.New_Line;
      for arg of Command_Line_Parser.Get_Registered_Arguments(Cmd_Parser) loop
         Ada.Text_IO.Put(Ada.Characters.Latin_1.HT & "-" & arg);
         Ada.Text_IO.Put(Ada.Characters.Latin_1.HT & Command_Line_Parser.Get_Help_Message(Cmd_Parser, arg));
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Help;


   procedure Execute(Cmd : in out Relocate_Command_Type) is
      	pragma Unreferenced (Cmd);
	package BE renames Bibentry;
      	Bib : Bibliography_Library.Bibliography_Library_Type;
   begin
      Command_Line_Parser.Parse(Cmd_Parser);

      Bibliography_Library.Load_From_Bibtex_File(Bib,
                                                 To_String(In_Out_Arg_Helper.arg_spec.input_path));

      for i in 0..(Bibliography_Library.Length(Bib)-1) loop
         declare
            Ref : constant Bibliography_Library.Bibtexentry_Ref := Bibliography_Library.Reference(Bib, i);
         begin

            if BE.Has_Bibtex_Property(Ref, "file") then
               declare
                  File_Att : constant String := BE.Get_Bibtex_Property(Ref, "file");
               begin
                  if not File_Attribute_Helper.Is_Valid_File_Attribute(File_Att) then
                     Put_Line("Invalid file property for bibtex entry " & BE.Get_Bibtex_Key(Ref));
                  else
                     declare
                        New_Pth : constant String := File_Attribute_Helper.Find_File_Match(File_Attribute_Helper.Get_Path(File_Att),
                                                                                  To_String(New_Lib_Path));
                     begin
                        BE.Set_Bibtex_Property(Ref, "file",
                          File_Attribute_Helper.Build_File_Attribute(File_Path   => New_Pth,
                                                                     File_Format => File_Attribute_Helper.Get_File_Format(File_Att)));
                        Put_Line(BE.Get_Bibtex_Key(Ref) & ".file -> " & New_Pth);
                     exception
                        when File_Attribute_Helper.Matching_File_Not_Found =>
                           Put_Line("Unable to find new file location for " & BE.Get_Bibtex_Key(Ref));
                     end;
                  end if;
               end;
            else
               Put_Line("No file attribute for entry " & BE.Get_Bibtex_Key(Bibliography_Library.Reference(Bib, i)));
            end if;
         end;
      end loop;

      Bibliography_Library.Save_To_Bibtex_File(Bib,
                                               To_String(In_Out_Arg_Helper.arg_spec.output_Path));

   end Execute;

   -- function called when the library path option is detected on the command
   -- line. The parameter c is the name of the argument met, and is not
   -- necessarilly referenced.
   procedure Set_Lib_Path(c : Character; Pth : String) is
      pragma Unreferenced (c);
   begin
      New_Lib_Path := Ada.Strings.Unbounded.To_Unbounded_String(Pth);
   end Set_Lib_Path;

   procedure Init is
   begin
      -- Prepare arguments
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

      Command_Line_Parser.Register_Callback(Parser        => Cmd_Parser,
                                            Argument_Name => 'n',
                                            Callback      => Set_Lib_Path'Access,
                                            Help_Msg      => "Path where is located the library",
                                            Required      => true);

      Command.Register_Command(Instance'Access);

   end Init;

end Command.Relocate;
