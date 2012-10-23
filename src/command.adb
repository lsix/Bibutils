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
with Ada.Text_IO;

package body Command is

   procedure Execute_Command(Cmd_Name : String) is
   begin
      Commands_Registry.Element(Cmd_Name).Execute;
   end Execute_Command;

   procedure Register_Command(Cmd : Command_Type'Class) is
   begin
      Commands_Registry.Insert(Cmd.Get_Name, Cmd);
   end Register_Command;

   procedure Print_Program_Help is
      package IO renames Ada.Text_IO;
      use Ada.Characters.Latin_1;

   begin
      IO.Put_Line("bibutil <cmd> [<cmd_specific_args>]");
      IO.New_Line;
      IO.Put_Line("Availalble commands are :");
      for e of Commands_Registry loop
         IO.Put_Line(HT & e.Get_Name);
      end loop;
   end Print_Program_Help;

end Command;
