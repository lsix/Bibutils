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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;

package Command_Line_Parser is

   type List_Of_Arguments is array(Natural range <>) of Character;

   -- Callbacks called when a particular argument is reached
   type Simple_Command_Line_Callback is access
     procedure(Argument_Name : Character);
   -- Header of the procedures that have to be called when an argument
   -- without value is reached.

   type Command_Line_Callback is access
     procedure(Argument_Name : Character;
               Value         : String);
   -- Header of the procedure that have to be called when an argument
   -- with a value is reached.

   type Command_Line_Parser_Type is private;

   ----------------------------------------------------------------------------
   --                                                                        --
   --                                                                        --
   --                             Main API                                   --
   --                                                                        --
   --                                                                        --
   ----------------------------------------------------------------------------

   procedure Register_Callback(Parser        : in out Command_Line_Parser_Type;
                               Argument_Name : Character;
                               Callback      : Simple_Command_Line_Callback;
                               Help_Msg      : String := "";
                               Required      : Boolean := false)
   with Precondition => not(Argument_Is_Registered(Parser, Argument_Name)),
   Postcondition => Argument_Is_Registered(Parser, Argument_Name);

   procedure Register_Callback(Parser        : in out Command_Line_Parser_Type;
                               Argument_Name : Character;
                               Callback      : Command_Line_Callback;
                               Help_Msg      : String := "";
                               Required      : Boolean := false)
   with Precondition => not(Argument_Is_Registered(Parser, Argument_Name)),
   Postcondition => Argument_Is_Registered(Parser, Argument_Name);

   procedure Parse(Parser : in out Command_Line_Parser_Type);
   -- Parse the arguments passed to the program. This procedure call has side
   -- effects on command line and removes the arguments from the original
   -- command line.
   -- All the remaining arguments are left.

   function Argument_Is_Registered(Parser        : Command_Line_Parser_Type;
                                   Argument_Name : Character) return Boolean;
   -- Says if a callback is registered for a particular argument.

   function Get_Registered_Arguments(Parser : Command_Line_Parser_Type)
                                     return List_Of_Arguments
   with Postcondition => (for all c of Get_Registered_Arguments'Result => Argument_Is_Registered(Parser, c)) and
   not (for some c of Get_Registered_Arguments'Result => not Argument_Is_Registered(Parser, c));
   -- Return the list of registered of arguments

   function Get_Help_Message(Parser : Command_Line_Parser_Type;
                             Argument_Name : Character) return String;
   -- Gives the message registered for the argument Argument_Name

   Missing_Required_Argument  : exception;
   Missing_Value_For_Argument : exception;
private
   package Simple_Callback_Maps is new
     Ada.Containers.Ordered_Maps(Key_Type     => Character,
                                 Element_Type => Simple_Command_Line_Callback,
                                 "<"          => "<",
                                 "="          => "=");

   package Callback_Maps is new
     Ada.Containers.Ordered_Maps(Key_Type     => Character,
                                 Element_Type => Command_Line_Callback,
                                 "<"          => "<",
                                 "="          => "=");

   package Bool_Maps is new
     Ada.Containers.Ordered_Maps(Key_Type     => Character,
                                 Element_Type => Boolean,
                                 "<"          => "<",
                                 "="          => "=");

   package Msg_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps(Key_Type     => Character,
                                            Element_Type => String,
                                            "<"          => "<",
                                            "="          => "=");

   type Command_Line_Parser_Type is
      record
         Parsed_Options   : Bool_Maps.Map;
         -- Holds informations about the arguments that have been met.
         -- If an argument is not required, it is assumed to be parsed.

         messages         : Msg_Maps.Map;
         Simple_Callbacks : Simple_Callback_Maps.Map;
         Callbacks        : Callback_Maps.Map;
      end record;

end Command_Line_Parser;
