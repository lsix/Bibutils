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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body File_Attribute_Helper is
   escape_char : constant Character := '\';
   column_char : constant Character := ':';

   function Is_Valid_File_Attribute(Str : String) return Boolean is
      nb_of_column   : Natural := 0;
      pred_is_escape : Boolean := false;
      is_valid       : Boolean;
   begin
      is_valid := Str(Str'First) = column_char;
      if is_valid then -- avoid unnessary operations
         for i in Str'Range loop
            if pred_is_escape then
               pred_is_escape := false;
            else
               if Str(i) = escape_char then
                  pred_is_escape := true;
               elsif Str(i) = column_char then
                  nb_of_column := Natural'Succ(nb_of_column);
               end if;
            end if;
         end loop;

         is_valid := is_valid and (nb_of_column = 2);
      end if;

      return is_valid;
   end Is_Valid_File_Attribute;

   -- Return the path contained in an attribute value
   function Get_Path(Str : String) return String is
      use Ada.Strings.Unbounded;

      pred_is_escape : boolean := false;
      pth            : Unbounded_String;

      curr           : Natural := Str'First;
   begin
      -- Go until the first ':'
      loop
         if pred_is_escape then
            pred_is_escape := false;
         else
            if Str(curr) = escape_char then
               pred_is_escape := true;
            end if;

            exit when (not(pred_is_escape) and (Str(curr) = column_char));
         end if;
         curr := Natural'Succ(curr);
      end loop;
      -- found first separator, start at the first following position
      curr := Natural'Succ(curr);
      loop
         if Str(curr) = escape_char then
            if pred_is_escape then
               Append(pth, escape_char);
            end if;
            pred_is_escape := not pred_is_escape;
         else
            exit when (not(pred_is_escape) and (Str(curr) = column_char));
            Append(pth, Str(curr));
            pred_is_escape := false;
         end if;
         curr := Natural'Succ(curr);
      end loop;

      return To_String(pth);
   end Get_Path;


   -- Return the file type encoded in the file attribute
   function Get_File_Format(Str : String) return String is
      use Ada.Strings.Unbounded;

      pred_is_escape : boolean := false;
      tpe            : Unbounded_String;

      curr           : Natural := Str'First;
   begin
      -- The format contains :<some_path>:<the_format>
      -- thus we have to move forward two ':'
      for i in 1..2 loop
         -- find the ':' which separate the path and the stype
         loop
            if pred_is_escape then
               pred_is_escape := false;
            else
               if Str(curr) = escape_char then
                  pred_is_escape := true;
               else
                  exit when Str(curr) = column_char;
               end if;

            end if;
            curr := Natural'Succ(curr);
         end loop;
         curr := Natural'Succ(curr);
      end loop;

      while curr <= Str'Last loop
         Append(tpe, Str(curr));
         curr := Natural'Succ(curr);
      end loop;

      return To_String(tpe);
   end Get_File_Format;

   function Build_File_Attribute(File_Path   : String;
                                 File_Format : String) return String is
      use Ada.Strings.Unbounded;
      bld : Unbounded_String := Null_Unbounded_String;

      procedure Append_Escaped(Dest : in out Unbounded_String;
                               Src  : String) is
         pragma Inline(Append_Escaped);
      begin
         for i in Src'Range loop
            if ((Src(i) = escape_char) or
                  (Src(i) = column_char)) then
               Append(Dest, escape_char);
            end if;
            Append(Dest, Src(i));
         end loop;

      end Append_Escaped;

   begin
      Append(bld, column_char);
      Append_Escaped(bld, File_Path);
      Append(bld, column_char);
      Append_Escaped(bld, File_Format);
      return To_String(bld);
   end Build_File_Attribute;

   function Guess_Path_Separator return Character is
      use Ada.Strings.Fixed;

      pth          : constant String  := Ada.Directories.Current_Directory;
      nb_slash     : constant Natural := Count(pth, "/");
      nb_backslash : constant Natural := Count(pth, "\");

      ret : character := '_';
   begin
      -- TODO do it properly, this gives no waranty at all
      if nb_slash > 0 and nb_backslash = 0 then
         ret := '/';
      elsif nb_slash = 0 and nb_backslash > 0 then
         ret := '\';
      else
         raise Error_Determining_File_System_Type;
      end if;

      return ret;
   end Guess_Path_Separator;

   -- Finds the clothest math of Original_Path within
   -- Lookup_Path directory.
   function Find_File_Match(Original_Path : String;
                            Lookup_Path   : String) return String is
      use Ada.Strings.Unbounded;

      Sep_Pos : Natural := Original_Path'Last;

      found : Boolean := false;
      path_sep : constant character := Guess_Path_Separator;
      trailing_part : Unbounded_String := Null_Unbounded_String;
      try_path : Unbounded_String;
      Lookup : Unbounded_String := To_Unbounded_String(Lookup_Path);

      function Is_Pth_Sep(c : Character) return Boolean is
      begin
         return ((c = '/') or (c = '\'));
      end Is_Pth_Sep;

   begin
      -- Ensure that there is a trailing slash at the end of the search path
      if Index(Lookup, "" & path_sep) /= Length(Lookup) then
         Append(Lookup, path_sep);
      end if;

      -- Search for a match until we find it
      while not found and Sep_Pos >= Original_Path'First loop
         while Sep_Pos >= Original_Path'First and then not Is_Pth_Sep(Original_Path(Sep_Pos)) loop
            Insert(trailing_part, 1, "" & Original_Path(Sep_Pos));
            Sep_Pos := Natural'Pred(Sep_Pos);
         end loop;

         try_path := Lookup;
         Append(try_path, trailing_part);

         begin
            if Ada.Directories.Exists(To_String(try_path)) then
               found := true;
            else
               -- prepare for next search
               Sep_Pos := Natural'Pred(Sep_Pos);
               Insert(trailing_part, 1, "" & path_sep);
            end if;
         exception
            when Ada.IO_Exceptions.Name_Error =>
               found := false;
         end;
      end loop;

      if not(found) then
         raise Matching_File_Not_Found;
      end if;

      return To_String(try_path);
   end Find_File_Match;

end File_Attribute_Helper;
