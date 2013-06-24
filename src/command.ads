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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash;
with Ada.Characters.Handling;

package Command is

   -- Execute a command called Cmd_Name
   -- Each command have to register itself during
   -- ellaboration to be accessible here
   procedure Execute_Command(Cmd_Name : String);

   procedure Print_Program_Help;

private
   ----------------------------------------------------------------------------
   -- Basic interface each command must conform to
   type Command_Type is abstract tagged limited null record;
   type Command_Type_Access is access all Command_Type'Class;

   function  Get_Name  (Cmd : Command_Type) return String is abstract
     with Post'Class =>
       (for all c of Get_Name'Result =>
         Ada.Characters.Handling.Is_Alphanumeric(c));
   -- Returns the name of a command
   procedure Print_Help(Cmd : Command_Type) is abstract;
   -- Prints a help message on the terminal describing how to use the
   -- command
   procedure Execute   (Cmd : in out Command_Type) is abstract;
   -- Executes the command. Might change the internal state of the
   -- command

   function "="(Left, Right : Command_Type'Class) return Boolean;


   function "="(Left, Right : Command_Type_Access) return Boolean;

   ----------------------------------------------------------------------------
   -- used for type registration
   package Cmd_Map is new
     Ada.Containers.Indefinite_Hashed_Maps(Key_Type        => String,
                                           Element_Type    => Command_Type_Access,
                                           Hash            => Ada.Strings.Hash,
                                           Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
                                           "="             => "=");

   Commands_Registry : Cmd_Map.Map := Cmd_Map.Empty_Map;

   procedure Register_Command(Cmd : Command_Type_Access);

end Command;
