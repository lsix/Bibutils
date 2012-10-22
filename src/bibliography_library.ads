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

with Ada.Containers.Indefinite_Vectors;
with Ada.Streams;
with Bibentry;                               use Bibentry;


package Bibliography_Library is

   type Bibtexentry_Ref(Data : not null access Bibentry_Type'Class)is limited null record
   with Implicit_Dereference => Data;

   type Bibliography_Library_Type is tagged private;

   ----------------------------------------------------------------------------
   --                                                                        --
   --                            IO related operations                       --
   --                                                                        --
   ----------------------------------------------------------------------------

   -- Loads a library from a .bib file
   procedure Load_From_Bibtex_File(Lib  : out Bibliography_Library_Type;
                                   Path : String);

   -- Save a library to a .bib file
   procedure Save_To_Bibtex_File(Lib  : Bibliography_Library_Type;
                                 Path : String);

   ----------------------------------------------------------------------------
   --                                                                        --
   --                            Library manipulation                        --
   --                                                                        --
   ----------------------------------------------------------------------------

   -- Append a reference entry into the library
   -- No element in the library can already have the same key
   procedure Append(Lib  : in out Bibliography_Library_Type;
                    Item : Bibentry_Type'Class)
   with Precondition => not(Lib.Contains_Key(Item.Get_Bibtex_Key));

   -- Get the number of elements stored inside the library
   function Length(Lib : Bibliography_Library_Type)
     return Natural;

   -- Retreive an element by its number
   function Element(Lib : Bibliography_Library_Type;
                    Pos : Natural)
                    return Bibentry_Type'Class
     with Precondition => Pos < Lib.Length;

   -- Retreive an element by its key
   function Element(Lib : Bibliography_Library_Type;
                    Key : String)
                    return Bibentry_Type'Class
     with Precondition => Lib.Contains_Key(Key);

   -- Get a reference on an element by its number
   function Reference(Lib : in out Bibliography_Library_Type;
                      Pos : Natural)
                      return Bibtexentry_Ref
     with Precondition => Pos < Lib.Length;

   -- Get a reference on a element by its key
   function Reference(Lib : in out Bibliography_Library_Type;
                      Key : String)
                      return Bibtexentry_Ref
     with Precondition => Lib.Contains_Key(Key);

   -- Remove an element given its id in the library
   procedure Remove(Lib : in out Bibliography_Library_Type;
                    Pos : Natural)
     with Precondition => Pos < Lib.Length;

   -- Remove an element given its bibtex key
   procedure Remove(Lib : in out Bibliography_Library_Type;
                    Key : String)
   with Precondition => Lib.Contains_Key(Key);

   -- Says if the library contains an elemement with key key
   function Contains_Key(Lib : Bibliography_Library_Type;
                         key : String)
     return Boolean;

   -- Re-orders the library according to a "<" function
   generic
      with function "<"(Left, Right : Bibentry_Type'Class) return Boolean;
   procedure Sort(Lib : in out Bibliography_Library_Type);

   -- Returns a string representation of the bibliography library
   function To_String(Lib : Bibliography_Library_Type)
     return String;

   -- IO Related operations
   procedure Print(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : Bibliography_Library_Type);

   function Read(Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Bibliography_Library_Type;

   for Bibliography_Library_Type'Output use Print;
   for Bibliography_Library_Type'Input use Read;

private
   package Bibentry_Vector is new
     Ada.Containers.Indefinite_Vectors(Index_Type   => Natural,
                                       Element_Type => Bibentry_Type'Class,
                                       "="          => "=");

   type Bibliography_Library_Type is tagged
      record
         Content : Bibentry_Vector.Vector := Bibentry_Vector.Empty_Vector;
      end record;

end Bibliography_Library;
