(* :Title: EventList *)

(* :Summary: Functions for EventList *)

(* :Author: Bo C. Herlin *)

(* :Licence: GPL

Copyright (C) 2004  Bo C. Herlin

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)

(* :Contact: bo@gcab.net *)

(* :Context: Musica2`EventList` *)

(* :History:
  2005-01-16  bch :  rewrote EventListToMidi as Midi[...], deleted MidiToEventList
  2004-09-13  bch :  Musica and Musica2 dont seem to get along very well.
  2004-08-27  bch :  added message ToDo
  2004-08-26  bch :  added some help/usage-text
  2004-08-10  bch :  changed MidiToEventList to call MidiSetState
  2004-08-06  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`EventList`",
  {
    "Musica`",
    "Musica`EventLists`MIDI`IO`",
    "Musica`EventLists`MIDI`Messages`",
    "Musica`EventLists`MIDI`Transformations`",
    "Musica2`Midi`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Musica2`Midi`Event,
  Musica2`Midi`Midi,
  Musica2`Midi`Track
  ];

Begin["`Private`"]

(* todo: handle all channel events, not just notes and meta *)
Musica2`Midi`Event[Musica`Event[tick_Integer,MetaEvent[t_Integer,d_]]] := Musica2`Midi`Event[{tick,{{EventTypeMeta,t},d}}]
Musica2`Midi`Event[Musica`Event[tick_Integer,NoteOn[p_Integer,v_,c_]]] := Musica2`Midi`Event[{tick,{{EventTypeNoteOn,c-1},{p,v}}}]
Musica2`Midi`Event[Musica`Event[tick_Integer,NoteOff[p_Integer,v_,c_]]] := Musica2`Midi`Event[{tick,{{EventTypeNoteOff,c-1},{p,v}}}]

Musica2`Midi`Track[Musica`Track[e__Musica`Event]] := Musica2`Midi`Track[Musica2`Midi`Event /@ {e}]

Midi[e_?EventListQ] := 
  Module[{x = ConvertEventList[e,EventList[MIDI, Musica`EventLists`MIDI`Messages`Tick, {}, Null]], o},
    (* get all opts to pass to the Midi object *)
    o = {
        FileFormat -> (MIDIFileFormat /. x[[0, 4]]),
        Musica2`Midi`TPQ -> (Musica`EventLists`MIDI`Messages`TPQ /. x[[0, 4]]),
        TimeUnit -> Musica2`Midi`Tick
        };
    (* get rid of the Head and convert each Track *)
    Midi[Musica2`Midi`Track /@ ReplacePart[x,List,0], Sequence @@ o]
    ]
  
End[]

Protect[
  Musica2`Midi`Event,
  Musica2`Midi`Midi,
  Musica2`Midi`Track
  ];

EndPackage[]
