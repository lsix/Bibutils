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

with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Strings_Utils;

package body Bibliography_Library_Merge is

   procedure Strict_Merge(Result      : out Bibliography_Library.Bibliography_Library_Type;
                          Left, Right : in  Bibliography_Library.Bibliography_Library_Type) is
      Lft_Idx   : Natural       := 0;
      Rgt_Idx   : Natural       := 0;
      Left_Elt  : Bibentry.Bibentry_Type := Bibliography_Library.Element(Left, Lft_Idx);
      Right_Elt : Bibentry.Bibentry_Type := Bibliography_Library.Element(Right, Rgt_Idx);

      procedure Next_Right is
      begin
         Rgt_Idx := Rgt_Idx + 1;
         if Rgt_Idx < Bibliography_Library.Length(Right) then
            Right_Elt := Bibliography_Library.Element(Right, Rgt_Idx);
         end if;
      end Next_Right;
      procedure Next_Left is
      begin
         Lft_Idx := Lft_Idx + 1;
         if Lft_Idx < Bibliography_Library.Length(Left) then
            Left_Elt := Bibliography_Library.Element(Left, Lft_Idx);
         end if;
      end Next_Left;

      decide_res : Strict_Choice_Type;
   begin

      -- Consume simultaneously left and wright
      while Lft_Idx < Bibliography_Library.Length(Left) and Rgt_Idx < Bibliography_Library.Length(Right) loop
         if Ada.Strings.Equal_Case_Insensitive(Bibentry.Get_Bibtex_Key(Left_Elt),
                                               Bibentry.Get_Bibtex_Key(Right_Elt))
         then
            Bibliography_Library.Append(Result, Left_Elt);
            Next_Left;
            Next_Right;
         elsif Strings_Utils.Is_Begining_Of_Case_Insensitive(Bibentry.Get_Bibtex_Key(Left_Elt),
                                                             Bibentry.Get_Bibtex_Key(Right_Elt)) or
           Strings_Utils.Is_Begining_Of_Case_Insensitive(Bibentry.Get_Bibtex_Key(Left_Elt),
                                                         Bibentry.Get_Bibtex_Key(Right_Elt)) then

            decide_res := Decide_Fct(Left_Elt, Right_Elt);
            if decide_res = Bibliography_Library_Merge.Left then
               Bibliography_Library.Append(Result, Left_Elt);
               Next_Left;
            elsif decide_res = Bibliography_Library_Merge.Right then
               Bibliography_Library.Append(Result, Right_Elt);
               Next_Right;
            else -- use both
               if "<"(Left_Elt, Right_Elt) then
                  Bibliography_Library.Append(Result, Left_Elt);
                  Bibliography_Library.Append(Result, Right_Elt);
               else
                  Bibliography_Library.Append(Result, Right_Elt);
                  Bibliography_Library.Append(Result, Left_Elt);
               end if;
               Next_Right;
               Next_Left;
            end if;
         elsif Ada.Strings.Less_Case_Insensitive(Bibentry.Get_Bibtex_Key(Left_Elt),
                                                 Bibentry.Get_Bibtex_Key(Right_Elt)) then
            Bibliography_Library.Append(Result, Left_Elt);
            Next_Left;
         else
            Bibliography_Library.Append(Result, Right_Elt);
            Next_Right;
         end if;
      end loop;

      -- Flush the last non empty list
      while Lft_Idx < Bibliography_Library.Length(Left) loop
         Bibliography_Library.Append(Result, Left_Elt);
         Next_Left;
      end loop;
      while Rgt_Idx < Bibliography_Library.Length(Right) loop
         Bibliography_Library.Append(Result, Right_Elt);
         Next_Right;
      end loop;
   end Strict_Merge;

   procedure Strinct_Append(Accum : in out Bibliography_Library.Bibliography_Library_Type;
                            other : in     Bibliography_Library.Bibliography_Library_Type) is
      Tmp : Bibliography_Library.Bibliography_Library_Type;

      procedure Local_Merge is new Strict_Merge(Decide_Fct, "<" );
   begin
      Local_Merge(Tmp, Accum, other);
      Accum := Tmp;
   end Strinct_Append;

end Bibliography_Library_Merge;
