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
with Ada.Text_IO;

package body Command.Help is

   function Get_Name(Cmd : Help_Command_Type) return String is
      pragma Unreferenced (Cmd);
   begin
      return Command_Name;
   end Get_Name;

   procedure Print_Help(Cmd : Help_Command_Type) is
      pragma Unreferenced (Cmd);
      package IO renames Ada.Text_IO;
   begin
      IO.Put_Line("bibutil help <cmd>");
      IO.Put_Line(Ada.Characters.Latin_1.HT & "Prints the help for the cmd command.");
   end Print_Help;

   procedure Execute(Cmd : in out Help_Command_Type) is
      package CL renames Ada.Command_Line;
   begin
      if CL.Argument_Count = 2 then
         if Commands_Registry.Contains(CL.Argument(2)) then
            Commands_Registry.Reference(CL.Argument(2)).all.Print_Help;
         else
            Ada.Text_IO.Put_Line("Unknown command " & '"' & CL.Argument(2) & '"' & "." );
         end if;
      else
         Ada.Text_IO.Put_Line("Incorrect use of the help command.");
         Print_Help(Cmd);
      end if;
   end Execute;

   procedure Init is
   begin
      Register_Command( Instance'Access );
   end Init;
end Command.Help;
