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

with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Text_IO.Text_Streams;
with Ada.Text_IO;

package body Bibliography_Library is

   procedure Load_From_Bibtex_File(Lib  : out Bibliography_Library_Type;
                                   Path : String) is
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;

      Bib_src    : File_Type;
      Bib_Stream : Stream_Access;
   begin
      Open(File => Bib_src,
        Mode => Ada.Text_IO.In_File,
        Name => Path);
      Bib_Stream := Ada.Text_IO.Text_Streams.Stream(Bib_src);

      lib := Bibliography_Library_Type'Input(Bib_Stream);

      Close(Bib_src);
   end Load_From_Bibtex_File;

   procedure Save_To_Bibtex_File(Lib  : Bibliography_Library_Type;
                                 Path : String) is
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;
      use Ada.Directories;

      Bib_File   : File_Type;
      Bib_Stream : Stream_Access;
   begin
      if Ada.Directories.Exists(Path) then
         Open(File => Bib_File,
              Mode => Out_File,
              Name => Path);
      else
         Create(File => Bib_File,
                Mode => Out_File,
                Name => Path);
      end if;

      Bib_Stream := Stream(Bib_File);
      Bibliography_Library_Type'Output(Bib_Stream, Lib);
      Close(File => Bib_File);
   end Save_To_Bibtex_File;

   procedure Append(Lib  : in out Bibliography_Library_Type;
                    Item : Bibentry_Type'Class) is
   begin
      Lib.Content.Append(Item);
   end Append;

   function Length(Lib : Bibliography_Library_Type)
                   return Natural is
   begin
      return Natural(Lib.Content.Length);
   end Length;

   function Element(Lib : Bibliography_Library_Type;
                    Pos : Natural)
                    return Bibentry_Type'Class is
   begin
      return Lib.Content.Element(Pos);
   end Element;

   function Element(Lib : Bibliography_Library_Type;
                    Key : String)
                    return Bibentry_Type'Class is
      Curs  : Bibentry_Vector.Cursor := Lib.Content.First;
      Found : Boolean := false;
      idx   : Natural;
   begin
      -- TODO refactor
      while Bibentry_Vector.Has_Element(Curs) and not(Found) loop
         if Bibentry_Vector.Element(Curs).Get_Bibtex_Key = Key then
            Found := true;
            Idx   := Bibentry_Vector.To_Index(Curs);
         end if;
         Bibentry_Vector.Next(Curs);
      end loop;

      return Lib.Content.Element(Idx);
   end Element;

   function Reference(Lib : in out Bibliography_Library_Type;
                      Pos : Natural)
                      return Bibtexentry_Ref is
   begin
      return Bibtexentry_Ref'(Data => Lib.Content.Reference(Pos).Element);
   end Reference;

   function Reference(Lib : in out Bibliography_Library_Type;
                      Key : String)
                      return Bibtexentry_Ref is
      Curs  : Bibentry_Vector.Cursor := Lib.Content.First;
      Found : Boolean := false;
      idx   : Natural;
   begin
      -- TODO refactor
      while Bibentry_Vector.Has_Element(Curs) and not(Found) loop
         if Bibentry_Vector.Element(Curs).Get_Bibtex_Key = Key then
            Found := true;
            Idx   := Bibentry_Vector.To_Index(Curs);
         end if;
         Bibentry_Vector.Next(Curs);
      end loop;

      return Bibtexentry_Ref'(Data => Lib.Content.Reference(Idx).Element);
   end Reference;

   procedure Remove(Lib : in out Bibliography_Library_Type;
                     Pos : Natural) is
   begin
      Lib.Content.Delete(Pos);
   end Remove;

   procedure Remove(Lib : in out Bibliography_Library_Type;
                    Key : String) is
      Curs  : Bibentry_Vector.Cursor := Lib.Content.First;
      Found : Boolean := false;
      idx   : Natural;
   begin
      -- TODO refactor
      while Bibentry_Vector.Has_Element(Curs) and not(Found) loop
         if Bibentry_Vector.Element(Curs).Get_Bibtex_Key = Key then
            Found := true;
            Idx   := Bibentry_Vector.To_Index(Curs);
         end if;
         Bibentry_Vector.Next(Curs);
      end loop;

      Lib.Content.Delete(Idx);
   end Remove;

   function Contains_Key(Lib : Bibliography_Library_Type;
                         key : String)
                         return Boolean is
      Curs  : Bibentry_Vector.Cursor := Lib.Content.First;
      Found : Boolean := false;
   begin
      -- TODO refactor
      while Bibentry_Vector.Has_Element(Curs) and not(Found) loop
         if Bibentry_Vector.Element(Curs).Get_Bibtex_Key = Key then
            Found := true;
         end if;
         Bibentry_Vector.Next(Curs);
      end loop;

      return Found;
   end Contains_Key;

   procedure Sort(Lib : in out Bibliography_Library_Type) is
      package Real_Sorting is new Bibentry_Vector.Generic_Sorting("<");
   begin
      Real_Sorting.Sort(Lib.Content);
   end Sort;

   function To_String(Lib : Bibliography_Library_Type)
                      return String is
      use Ada.Strings.Unbounded;
      Str_const : Unbounded_String := Null_Unbounded_String;

      Curs : Bibentry_Vector.Cursor := Lib.Content.First;
   begin
      while Bibentry_Vector.Has_Element(Curs) loop
         Append(Str_const, Bibentry_Vector.Element(Curs).To_String);
         Bibentry_Vector.Next(Curs);
         if Bibentry_Vector.Has_Element(Curs) then
            Append(Str_const, Ada.Characters.Latin_1.LF);
            Append(Str_const, Ada.Characters.Latin_1.CR);
         end if;
      end loop;
      return To_String(Str_Const);
   end To_String;

   procedure Print(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Bibliography_Library_Type) is
      use Ada.Streams;
      New_Line : constant Stream_Element_Array := (Stream_Element'Val(Character'Pos(Ada.Characters.Latin_1.CR)),
                                         Stream_Element'Val(Character'Pos(Ada.Characters.Latin_1.LF)));
   begin
      for Ent of Item.Content loop
         Bibentry_Type'Output(Stream, Ent);
         Stream.Write(New_Line);
      end loop;

   end Print;

   function Read(Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                 return Bibliography_Library_Type is
      ret : Bibliography_Library_Type;
      ent : Bibentry_Type;

      curr_ln_number : Natural := 1;
      curr_cr_number : Natural := 1;
   begin
      loop
         ent := Bibentry.Read(Stream, curr_ln_number, curr_cr_number);
         ret.Append(Item => ent);
      end loop;
   exception
         -- Only way to detect the end is to reach the end of the stream
      when Bibentry.End_Of_Stream_Reached =>
         return ret;
   end Read;

   procedure Adjust(Obj : in out Bibliography_Library_Type) is
   begin
      Obj.Content := Obj.Content.Copy;
   end Adjust;

   procedure Finalize(Obj : in out Bibliography_Library_Type) is
   begin
      Obj.Content.Clear;
   end Finalize;

end Bibliography_Library;
