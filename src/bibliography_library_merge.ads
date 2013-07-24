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
--ame conditions as regards security.                                        --
--                                                                           --
-- The fact that you are presently reading this means that you have had      --
-- knowledge of the CeCILL license and that you accept its terms.            --

with Bibliography_Library;
with Bibentry;

-------------------------------------------------------------------------------
-- Library merging utilities                                                 --
--                                                                           --
package Bibliography_Library_Merge is

   type Strict_Choice_Type is (Left, Both, Right);

   generic
      with function Decide_Fct(Left, Wright : Bibentry.Bibentry_Type)
                               return Strict_Choice_Type;
      with function "<"(Left, Write: Bibentry.Bibentry_Type) return Boolean;
   procedure Strict_Merge(Result      : out Bibliography_Library.Bibliography_Library_Type;
                          Left, Right : in  Bibliography_Library.Bibliography_Library_Type);
--     with
--     Precondition  =>
--       (for all i in 1..Bibliography_Library.Length(Left)  => (Bibliography_Library.Element(Left,  i-1) < Bibliography_Library.Element(Left, i))) and
--       (for all i in 1..Bibliography_Library.Length(Right) => (Bibliography_Library.Element(Right, i-1) < Bibliography_Library.Element(Right, i))) and
--       Bibliography_Library.Length(Left) > 0 and Bibliography_Library.Length(Right) > 0,
--     Postcondition =>
--       (for all i in 1..Bibliography_Library.Length(Result)  => (Bibliography_Library.Element(Result, i-1) < Bibliography_Library.Element(Result, i)));


   -- Merge two libraries into one new library
   -- A strict algorithm is used (two entries are identical if they have
   -- the same bibtexkey and the same string representation.
   -- If they share the same bibtexkey and different string representations,
   -- of if one bibtexkey is the begining of the other, then Decide_Fct is
   -- called to determine which one of the two entries must be kept.

   generic
      with function Decide_Fct(Left, Wright : Bibentry.Bibentry_Type) return Strict_Choice_Type;
      with function "<"(Left, Write: Bibentry.Bibentry_Type) return Boolean;
   procedure Strinct_Append(Accum : in out Bibliography_Library.Bibliography_Library_Type;
                            other : in Bibliography_Library.Bibliography_Library_Type);
     -- Accum must be sorted according to Compare before and after the fct
     -- call
--   with
--     Precondition  => (for all i in 1..Bibliography_Library.Length(Accum) => (Bibliography_Library.Element(Accum, i-1) < Bibliography_Library.Element(Accum, i))),
--     Postcondition => (for all i in 1..Bibliography_Library.Length(Accum) => (Bibliography_Library.Element(Accum, i-1) < Bibliography_Library.Element(Accum, i)));
   -- Similar to Strict_Merge but add all the entries of other into Accum.

end Bibliography_Library_Merge;
