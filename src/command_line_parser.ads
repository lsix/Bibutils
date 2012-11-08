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
                                     return List_Of_Arguments;
--   with Postcondition => (for all c of Get_Registered_Arguments'Result => Argument_Is_Registered(Parser, c)) and
--   not (for some c of Get_Registered_Arguments'Result => Argument_Is_Registered(Parser, c));
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
