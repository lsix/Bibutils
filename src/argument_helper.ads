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

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

-- Provides helpers to parse command line arguments
package Argument_Helper is

   ----------------------------------------------------------------------------
   --                                                                        --
   --  Elements to parse arguments related to input and output arguments of  --
   --  the program                                                           --
   --    -i is used to speficy the source file                               --
   --    -o is used to specify the output file                               --
   --    -e is used to edit the source file in place                         --
   --                                                                        --
   ----------------------------------------------------------------------------

   type In_Out_Spec is
      record
         input_path          : Unbounded_String;
         output_path         : Unbounded_String;
      end record;

   type In_Out_Arg_Spec is
      record
         Arg_Spec : Unbounded_String;
         Arg_Desc : Unbounded_String;
      end record;

   type In_Out_Args_Type is array (Natural range <>) of In_Out_Arg_Spec;

   In_Out_Arguments : constant In_Out_Args_Type :=
     In_Out_Args_Type'
       (
        In_Out_Arg_Spec'(
          Arg_Spec => To_Unbounded_String("-i <source_file>"),
          Arg_Desc => To_Unbounded_String("Speficy the input bibtex source file.")
         ),
        In_Out_Arg_Spec'(
          Arg_Spec => To_Unbounded_String("-o <output_file>"),
          Arg_Desc => To_Unbounded_String("Specify the output bibtex file.")
         ),
        In_Out_Arg_Spec'(
          Arg_Spec => To_Unbounded_String("-e"),
          Arg_Desc => To_Unbounded_String("Edit the source file in place. -i must be called before -e.")
         )
       );

   -- Threat the -i, -o and -e options
   -- Caution, this procedure call destroys the consumed command line
   -- arguments
   procedure Parse_In_Out_Arguments(Res : out In_Out_Spec);

   Invalid_Arguments : exception;
end Argument_Helper;
