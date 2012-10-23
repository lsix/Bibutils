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

with Ada.Containers.Ordered_Maps;
with Ada.Streams;
with Ada.Strings.Unbounded;

package Bibentry is

   -- Contient les noms des propriétés d'une entrée bibliographique
   type Bibentry_Prop_Name_Array is array (Natural range <>) of String(1..50);

   type Bibentry_Type is tagged private;

   -- Set the type of bibentry (article, inproceeding, phdthesis,...)
   procedure Set_Bibtex_Type(Bibentry  : in out Bibentry_Type;
                             Type_Name : String);

   -- Get the name of the bibentry
   function Get_Bibtex_Type(Bibentry : Bibentry_Type)
     return String;

   -- Set the bibtex key of the bibentry bibliography
   -- entry element
   procedure Set_Bibtex_Key(Bibentry : in out Bibentry_Type;
                            Key      : String);

   -- Get the bibtex key of the bibentry bibliography
   -- element
   function Get_Bibtex_Key(bibentry : Bibentry_Type)
                           return String;

   -- Set the property Prop_Name for the bibentry bibliography
   -- entry.
   procedure Set_Bibtex_Property(Bibentry  : in out Bibentry_Type;
                                 Prop_Name : String;
                                 Prop_Val  : String);

   -- Tels wether or not the property Prop_Name is defined
   -- (has a value) in the Bibentry bibliography entry.
   function Has_Bibtex_Property(Bibentry  : Bibentry_Type;
                                Prop_Name : String)
                                return Boolean;

   -- Returns the value of the property designated by
   -- propname in the bibliography entry bibentry
   function Get_Bibtex_Property(Bibentry  : Bibentry_Type;
                                Prop_Name : String)
                                return String
   with Precondition => Has_Bibtex_Property(Bibentry, Prop_Name);

   -- Retourne l'ensemble des propriétés définies pour une entrée
   -- bibliographique
   function Get_Bibtex_Property_Names(Bibentry : Bibentry_Type)
                                      return Bibentry_Prop_Name_Array;

   function To_String(Bibentry : Bibentry_Type) return String;

   -- I/O related operations
   procedure Print(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Bibentry_Type);

   function Read(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                 curr_line_number : in out Natural;
                 curr_char_number : in out Natural) return Bibentry_Type;

   function Read(Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                 return Bibentry_Type;

   End_Of_Stream_Reached : exception;
   Invalid_Entry         : exception;

   for Bibentry_Type'Output use Print;
   for Bibentry_Type'Input use Read;

private
   package Properties_Map is new
     Ada.Containers.Ordered_Maps(Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
                                 Element_Type => Ada.Strings.Unbounded.Unbounded_String,
                                 "<"          => Ada.Strings.Unbounded."<",
                                 "="          => Ada.Strings.Unbounded."=");

   type Bibentry_Type is tagged
      record
         Key        : Ada.Strings.Unbounded.Unbounded_String;
         Entry_Type : Ada.Strings.Unbounded.Unbounded_String;
         properties : Properties_Map.Map;
      end record;

end Bibentry;
