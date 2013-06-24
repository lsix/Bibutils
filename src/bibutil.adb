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

with Ada.Command_Line;
with Ada.Command_Line.Remove;
with Command;

with Command.Help;
with Command.Sort;
with Command.Relocate;


procedure Bibutil is
begin
   -- Set up commands
   Command.Help.Init;
   Command.Sort.Init;
   Command.Relocate.Init;

   -- At least one argument, the first one should be the command
   if Ada.Command_Line.Argument_Count >= 1 then
      declare
         cmd : constant String := Ada.Command_Line.Argument(1);
      begin
         -- When a command is ran, it only sees its arguments not
         -- the ones passed to the main program
         Ada.Command_Line.Remove.Remove_Argument(1);
         Command.Execute_Command(Cmd);
      end;
   else
      Command.Print_Program_Help;
   end if;

end Bibutil;
