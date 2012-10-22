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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

package body Character_Utils is
   function Is_Alpha(Char : Character) return Boolean is
   begin
      return Ada.Characters.Handling.Is_Letter(Char);
   end;

   function Is_Alphanumeric(Char : Character) return Boolean is
   begin
      return Ada.Characters.Handling.Is_Alphanumeric(Char);
   end Is_Alphanumeric;

   function Is_White_Space(Char : Character) return Boolean is
   begin
      return (
        (Char = ' ') or
          (Char = Ada.Characters.Latin_1.HT) or
          (Char = Ada.Characters.Latin_1.CR) or
          (Char = Ada.Characters.Latin_1.LF)
       );
   end Is_White_Space;

end Character_Utils;
