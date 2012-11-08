with Ada.Text_IO;

package body In_Out_Arg_Helper is

   procedure Parse_M_I(c    : Character;
                       Path : String) is
   begin
      arg_spec.input_path := To_Unbounded_String(Path);
   end Parse_M_I;

   procedure Parse_M_O(c    : Character;
                       Path : String) is
   begin
      arg_spec.output_Path := To_Unbounded_String(Path);
   end Parse_M_O;

   procedure Parse_M_E(c : Character) is
   begin
      arg_spec.output_Path := arg_spec.input_path;
   end Parse_M_E;

end In_Out_Arg_Helper;
