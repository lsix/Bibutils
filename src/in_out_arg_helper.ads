with Ada.Strings.Unbounded;
with Command_Line_Parser;

use Ada.Strings.Unbounded;

package In_Out_Arg_Helper is

   type In_Out_Arg is
      record
         input_path  : Unbounded_String := Null_Unbounded_String;
         output_Path : Unbounded_String := Null_Unbounded_String;
      end record;

   arg_spec : In_Out_Arg;

   procedure Parse_M_I(c    : Character;
                       Path : String);

   procedure Parse_M_O(c    : Character;
                       Path : String);

   procedure Parse_M_E(c : Character);

end In_Out_Arg_Helper;
