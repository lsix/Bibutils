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

with Ada.Command_Line.Remove;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
package body Argument_Helper is

   procedure Parse_In_Out_Arguments(Res : out In_Out_Spec) is
      needs_val : Boolean := false;
      pred_arg  : Unbounded_String := Null_Unbounded_String;
      i         : Natural := 1;
   begin
      -- reset all the values
      Res := In_Out_Spec'(input_path          => Null_Unbounded_String,
                          output_path         => Null_Unbounded_String);

      while i <= Ada.Command_Line.Argument_Count loop
         if needs_val then
            if pred_arg = "-i" then
               Res.input_path := To_Unbounded_String(Ada.Command_Line.Argument(i));
            elsif pred_arg = "-o" then
               Res.output_path := To_Unbounded_String(Ada.Command_Line.Argument(i));
            end if;

            -- remove the two last seend args
            i := Natural'Pred(i);
            pred_arg := Null_Unbounded_String;
            Ada.Command_Line.Remove.Remove_Argument(i);
            Ada.Command_Line.Remove.Remove_Argument(i);
            needs_val := false;
         else
            if Ada.Command_Line.Argument(i) = "-i" or
              Ada.Command_Line.Argument(i) = "-o" then
               pred_arg := To_Unbounded_String(Ada.Command_Line.Argument(i));
               needs_val := true;
               i := Natural'Succ(i);
            elsif Ada.Command_Line.Argument(i) = "-e" then
               -- It is impossible to use -e unless -i have been used
               if Res.input_path = Null_Unbounded_String then
                  Ada.Exceptions.Raise_Exception(Invalid_Arguments'Identity,
                                                "Use -i before you can use -e");
               else
                  Res.output_path := Res.input_path;
               end if;
               Ada.Command_Line.Remove.Remove_Argument(i);
            else
               i := Natural'Succ(i);
            end if;
         end if;
      end loop;

   end Parse_In_Out_Arguments;


end Argument_Helper;
