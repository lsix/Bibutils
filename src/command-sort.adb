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
with Ada.Command_Line;
with Ada.Strings.Fixed.Less_Case_Insensitive;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Argument_Helper;
with Bibentry;
with Bibliography_Library;
with Config;

package body Command.Sort is

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
      for v of Argument_Helper.In_Out_Arguments loop
         Put_Line(HT & To_String(v.Arg_Spec) & HT &
                    To_String(v.Arg_Desc));
      end loop;

   end Print_Help;

   procedure Execute(Cmd : Sort_Type) is
      use Ada.Strings.Unbounded;

      In_Out  : Argument_Helper.In_Out_Spec;
      Bibfile : Bibliography_Library.Bibliography_Library_Type;

      function Compare_By_Key(Left, Right : Bibentry.Bibentry_Type'Class) return Boolean is

      begin
         return Ada.Strings.Fixed.Less_Case_Insensitive(Left.Get_Bibtex_Key, Right.Get_Bibtex_Key);
      end Compare_By_Key;

      procedure Sort_By_Key is new Bibliography_Library.Sort(Compare_By_Key);
   begin
      Argument_Helper.Parse_In_Out_Arguments(In_Out);

      Bibfile.Load_From_Bibtex_File(To_String(In_Out.input_path));

      Sort_By_Key(Bibfile);

      Bibfile.Save_To_Bibtex_File(To_String(In_Out.output_path));
   end Execute;

begin
   declare
      Cmd : Sort_Type;
   begin
      Register_Command(Cmd);
   end;
end Command.Sort;
