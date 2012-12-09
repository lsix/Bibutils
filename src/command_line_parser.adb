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
with Ada.Exceptions;

package body Command_Line_Parser is

   procedure Register_Callback(Parser        : in out Command_Line_Parser_Type;
                               Argument_Name : Character;
                               Callback      : Simple_Command_Line_Callback;
                               Help_Msg      : String := "";
                               Required      : Boolean := false) is
   begin
      Parser.Simple_Callbacks.Insert(Argument_Name, Callback);
      Parser.messages.Insert(Argument_Name, Help_Msg);
      Parser.Parsed_Options.Insert(Argument_Name, not required);
   end Register_Callback;

   procedure Register_Callback(Parser        : in out Command_Line_Parser_Type;
                               Argument_Name : Character;
                               Callback      : Command_Line_Callback;
                               Help_Msg      : String := "";
                               Required      : Boolean := false) is
   begin
      Parser.Callbacks.Insert(Argument_Name, Callback);
      Parser.messages.Insert(Argument_Name, Help_Msg);
      Parser.Parsed_Options.Insert(Argument_Name, not required);
   end Register_Callback;

   procedure Parse(Parser : in out Command_Line_Parser_Type) is
      use Ada.Command_Line;

      curr_arg : Natural := 1;
      Curs     : Bool_Maps.Cursor := Parser.Parsed_Options.First;
   begin
      while curr_arg <= Argument_Count loop
         declare
            arg : constant String := Argument(curr_arg);
         begin
            if arg'Last - arg'First = 1
              and then arg(arg'First) = '-'
              and then Parser.Parsed_Options.Contains(arg(arg'Last))
            then
               if Parser.Simple_Callbacks.Contains(arg(arg'Last)) then
                  Parser.Simple_Callbacks(arg(arg'Last)).all(arg(arg'Last));
                  Ada.Command_Line.Remove.Remove_Argument(curr_arg);
                  Parser.Parsed_Options(arg(arg'Last)) := true;
               else
                  if curr_arg = Argument_Count then
                     Ada.Exceptions.Raise_Exception(E       => Missing_Value_For_Argument'Identity,
                                                    Message => "Missing value for argument '" & arg & "'");
                  end if;
                  Parser.Callbacks(arg(arg'Last)).all(arg(arg'Last), Argument(curr_arg+1));
                  Ada.Command_Line.Remove.Remove_Argument(curr_arg);
                  Ada.Command_Line.Remove.Remove_Argument(curr_arg);
                  Parser.Parsed_Options(arg(arg'Last)) := true;
               end if;
            else
               curr_arg := Natural'Succ(curr_arg);
            end if;
         end;
      end loop;

      -- Checks that all the required arguments are there
      while Bool_Maps.Has_Element(Curs) loop
         if not Bool_Maps.Element(Curs) then
            Ada.Exceptions.Raise_Exception(Missing_Required_Argument'Identity,
                                           "Required argument '-" & Bool_Maps.Key(Curs) &"' is missing");
         end if;
         Bool_Maps.Next(Curs);
      end loop;
   end Parse;

   function Argument_Is_Registered(Parser        : Command_Line_Parser_Type;
                                   Argument_Name : Character) return Boolean is
   begin
      return Parser.Parsed_Options.Contains(Argument_Name);
   end Argument_Is_Registered;

   function Get_Registered_Arguments(Parser : Command_Line_Parser_Type)
                                     return List_Of_Arguments is
      ret  : List_Of_Arguments(1..Natural(Parser.Parsed_Options.Length));
      I    : Natural := 1;
      Curs : Bool_Maps.Cursor := Parser.Parsed_Options.First;
   begin
      while Bool_Maps.Has_Element(Curs) loop
         ret(i) := Bool_Maps.Key(Curs);
         I := Natural'Succ(I);
         BOol_Maps.Next(Curs);
      end loop;
      return ret;
   end Get_Registered_Arguments;

   function Get_Help_Message(Parser : Command_Line_Parser_Type;
                             Argument_Name : Character) return String is
   begin
      return Parser.messages(Argument_Name);
   end Get_Help_Message;

end Command_Line_Parser;


