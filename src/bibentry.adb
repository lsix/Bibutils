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
with Ada.Exceptions;
with Ada.Strings.Unbounded;                          use Ada.Strings.Unbounded;
with Ada.Strings;
with Character_Utils;

package body Bibentry is

   -- Set the type of bibentry (article, inproceeding, phdthesis,...)
   procedure Set_Bibtex_Type(Bibentry  : in out Bibentry_Type;
                             Type_Name : String) is
   begin
      Bibentry.Entry_Type := To_Unbounded_String(Type_Name);
   end Set_Bibtex_Type;

   -- Get the name of the bibentry
   function Get_Bibtex_Type(Bibentry : Bibentry_Type)
                            return String is
   begin
      return To_String(Bibentry.Entry_Type);
   end Get_Bibtex_Type;

   -- Set the bibtex key of the bibentry bibliography
   -- entry element
   procedure Set_Bibtex_Key(Bibentry : in out Bibentry_Type;
                            Key      : String) is
   begin
      Bibentry.Key := To_Unbounded_String(Key);
   end Set_Bibtex_Key;

   -- Get the bibtex key of the bibentry bibliography
   -- element
   function Get_Bibtex_Key(bibentry : Bibentry_Type)
                           return String is
   begin
      return To_String(Bibentry.Key);
   end Get_Bibtex_Key;

   -- Set the property Prop_Name for the bibentry bibliography
   -- entry.
   procedure Set_Bibtex_Property(Bibentry  : in out Bibentry_Type;
                                 Prop_Name : String;
                                 Prop_Val  : String) is
      use Properties_Map;
      us_prop_key : Unbounded_String := To_Unbounded_String(Prop_Name);
      us_prop_val : Unbounded_String := To_Unbounded_String(Prop_Val);
   begin
      if Bibentry.properties.Contains(us_prop_key) then
         Bibentry.properties.Replace(Key      => us_prop_key,
                                     New_Item => us_prop_val);
      else
         Bibentry.properties.Insert(Key      => us_prop_key,
                                    New_Item => us_prop_val);
      end if;
   end Set_Bibtex_Property;

   -- Returns the value of the property designated by
   -- propname in the bibliography entry bibentry
   function Get_Bibtex_Property(Bibentry  : Bibentry_Type;
                                Prop_Name : String)
                                return String is
      us_prop_key : Unbounded_String := To_Unbounded_String(Prop_Name);
   begin
      return To_String(Bibentry.properties.Element(us_prop_key));
   end Get_Bibtex_Property;

   -- Tels wether or not the property Prop_Name is defined
   -- (has a value) in the Bibentry bibliography entry.
   function Has_Bibtex_Property(Bibentry  : Bibentry_Type;
                                Prop_Name : String)
                       return Boolean is
   begin
      return Bibentry.properties.Contains(To_Unbounded_String(Prop_Name));
   end Has_Bibtex_Property;

   -- Retourne l'ensemble des propriétés définies pour une entrée
   -- bibliographique
   function Get_Bibtex_Property_Names(Bibentry : Bibentry_Type)
                                      return Bibentry_Prop_Name_Array is
      ret  : Bibentry_Prop_Name_Array(1..Natural(Bibentry.properties.Length));
      i    : Natural := 1;
      Curs : Properties_Map.Cursor := Bibentry.properties.First;
   begin
      while Properties_Map.Has_Element(Curs) loop
         ret(i) := To_String(Properties_Map.Element(Curs));
         Properties_Map.Next(Curs);
         i := Natural'Succ(i);
      end loop;

      return ret;
   end Get_Bibtex_Property_Names;

   function To_String(Bibentry : Bibentry_Type) return String is
      use Ada.Strings;
      use Ada.Strings.Unbounded;

      procedure New_Line(Str : in out Unbounded_String) is
      begin
         Append(Str, Ada.Characters.Latin_1.CR);
         Append(Str, Ada.Characters.Latin_1.LF);
      end New_Line;

      String_rep : Unbounded_String;
      Curs : Properties_Map.Cursor := Bibentry.properties.First;

   begin
      String_rep := To_Unbounded_String("@");
      Append(String_Rep, Trim(Bibentry.Entry_Type, Both));
      Append(String_Rep, "{");
      Append(String_rep, Trim(Bibentry.Key, Both));
      Append(String_rep, ",");
      New_Line(String_Rep);

      while Properties_Map.Has_Element(Curs) loop
         Append(String_Rep, Ada.Characters.Latin_1.HT);
         Append(String_Rep, Trim(Properties_Map.Key(Curs), Both));
         Append(String_Rep, " = {");
         Append(String_Rep, Trim(Properties_Map.Element(Curs), Both));
         Append(String_Rep, "}");
         Properties_Map.Next(Curs);
         if Properties_Map.Has_Element(Curs) then
            Append(String_Rep, ",");
         end if;
         New_Line(String_rep);
      end loop;
      Append(String_Rep, "}");
      New_Line(String_Rep);

      return To_String(String_Rep);
   end To_String;

   -- I/O related operations
   procedure Print(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Bibentry_Type) is
      use Ada.Streams;
      Str_rep : String := To_String(Item);
      Buffer  : Ada.Streams.Stream_Element_Array(Ada.Streams.Stream_Element_Offset(Str_rep'First)..Ada.Streams.Stream_Element_Offset(Str_rep'Last));
   begin
      for i in Str_rep'Range loop
         Buffer(Ada.Streams.Stream_Element_Offset(i))
           := Ada.Streams.Stream_Element(Character'Pos(Str_rep(Positive(i))));
      end loop;

      Stream.Write(Buffer);
   end Print;

   function Read(Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                 return Bibentry_Type is
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      use Character_Utils;

      Bib_Entry   : Bibentry_Type;
      Curr_Char   : Character;
      Accumulator : Unbounded_String := Null_Unbounded_String;
      Prop_Name   : Unbounded_String := Null_Unbounded_String;

      procedure Skip_Line(Stream : not null access Ada.Streams.Root_Stream_Type'Class) is
         pragma Inline(Skip_Line);
         Buffer  : Ada.Streams.Stream_Element_Array(1..1);
         Nb_Read : Ada.Streams.Stream_Element_Offset;
      begin
         -- TODO regarder le cas windows ou on a \r\n
         while Curr_Char /= Ada.Characters.Latin_1.LF loop
            Stream.Read(Buffer, Nb_Read);
            if Natural(Nb_Read) = 1 then
               Curr_Char := Character'Val(Buffer(1));
            else
               raise End_Of_Stream_Reached;
            end if;
         end loop;
      end Skip_Line;

      function Get_Char(Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                        return Character is
         pragma Inline(Get_Char);
         Buffer  : Ada.Streams.Stream_Element_Array(1..1);
         Nb_Read : Ada.Streams.Stream_Element_Offset;
      begin
         Stream.all.Read(Buffer, Nb_Read);
         if Natural(Nb_Read) = 1 then
            Curr_Char := Character'Val(Buffer(1));
         else
            raise End_Of_Stream_Reached;
         end if;
         if Curr_Char = '%' then
            Skip_Line(Stream);
            Curr_Char := Get_Char(Stream);
         end if;

         return Curr_Char;
      end Get_Char;

      procedure Skip_Blank is
         pragma Inline(Skip_Blank);
      begin
         while Is_White_Space(Curr_Char) loop
            Curr_Char := Get_Char(Stream);
         end loop;
      end Skip_Blank;

      function Read_Braket_Enclosed_Value return String is
         Acc : Unbounded_String := Null_Unbounded_String;
         Nb_Op_Brak : Natural := 1;
      begin
         -- Curr_Char
         if Curr_Char /= '{' then
            Raise_Exception(Invalid_Entry'Identity, "'{' expected, '"&Curr_Char&"' found");
         end if;
         loop
            Curr_Char := Get_Char(Stream);
            if Curr_Char = '{' then
               Nb_Op_Brak := Natural'Succ(Nb_Op_Brak);
            elsif Curr_Char = '}' then
               Nb_Op_Brak := Natural'Pred(Nb_Op_Brak);
            end if;
            exit when Nb_Op_Brak = 0;
            Append(Acc, Curr_Char);
         end loop;
         Curr_Char := Get_Char(Stream);
         return To_String(Acc);
      end Read_Braket_Enclosed_Value;

      function Read_Db_Quoted_Value return String is
      begin
         return "";
      end Read_Db_Quoted_Value;


   begin
      -- Trim stream until to remove all blanks until a "@" is reached.
      Curr_Char := Get_Char(Stream);

      Skip_Blank;

      -- Commence par un '@'
      if not(Curr_Char = '@') then
         Raise_Exception(Invalid_Entry'Identity, "'@' expected, '"&Curr_Char&"' found");
      end if;

      Curr_Char := Get_Char(Stream);
      while Is_Alpha(Curr_Char) loop
         Append(Accumulator, Curr_Char);
         Curr_Char := Get_Char(Stream);
      end loop;

      Bib_Entry.Set_Bibtex_Type(To_String(Accumulator));
      Accumulator := Null_Unbounded_String;

      Skip_Blank;
      if not(Curr_Char = '{') then
         raise Invalid_Entry;
      end if;
      Skip_Blank;

      Curr_Char := Get_Char(Stream);
      while Is_Alpha(Curr_Char) or (Curr_Char in '0'..'9') loop
         Append(Accumulator, Curr_Char);
         Curr_Char := Get_Char(Stream);
      end loop;
      Bib_Entry.Set_Bibtex_Key(To_String(Accumulator));
      Accumulator := Null_Unbounded_String;
      Skip_Blank;
      if not(Curr_Char = ',') then
         Raise_Exception(Invalid_Entry'Identity, "',' expected, '"&Curr_Char&"' found");
      end if;

      Curr_Char := Get_Char(Stream);
      loop
         Skip_Blank;
         -- Lecture de chacune des proprietes
         Append(Accumulator, Curr_Char);
         Curr_Char := Get_Char(Stream);
         while Is_Alpha(Curr_Char) loop
            Append(Accumulator, Curr_Char);
            Curr_Char := Get_Char(Stream);
         end loop;

         Skip_Blank;
         if Curr_Char /= '=' then
            Raise_Exception(Invalid_Entry'Identity, "'=' expected, '"&Curr_Char&"' found");
         end if;
         Curr_Char := Get_Char(Stream);
         Skip_Blank;

         if Curr_Char = '{' then
            Bib_Entry.Set_Bibtex_Property(Prop_Name => To_String(Accumulator),
                                          Prop_Val  => Read_Braket_Enclosed_Value);
         else -- Curr_Char = '"' then
            -- TODO manage those values that can contains references to Strings
            Bib_Entry.Set_Bibtex_Property(Prop_Name => To_String(Accumulator),
                                          Prop_Val  => Read_Braket_Enclosed_Value);
            raise Invalid_Entry;
         end if;
         Accumulator := Null_Unbounded_String;
         Skip_Blank;

         exit when Curr_Char = '}';

         if Curr_Char /= ',' then
            raise Invalid_Entry;
         end if;
         Curr_Char := Get_Char(Stream);
      end loop;
      Skip_Blank;

      return Bib_Entry;
   end Read;

end Bibentry;
