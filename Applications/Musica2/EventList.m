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
  2004-08-10  bch :  changed MidiToEventList to call MidiSetState
  2004-08-06  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`EventList`",
  {
    "Musica2`Midi`",
    "Musica`",
    "Musica`EventLists`MIDI`IO`",
    "Musica`EventLists`MIDI`Messages`",
    "Musica`EventLists`MIDI`Transformations`"
    }
  ]

Unprotect[
  EventListToMidi,
  MidiToEventList
  ];

EventListToMidi::usage = "EventListToMidi[e]"
MidiToEventList::usage = "MidiToEventList[m]"

Begin["`Private`"]

EventListToMidi[e_] :=
  Module[{x = ConvertEventList[e, EventList[MIDI, Tick, {}, Null]]},
    Midi[{MidiFileFormat -> (MIDIFileFormat /. x[[0, 4]]), MidiTPQ -> (TPQ /. x[[0, 4]]), MidiShape -> MidiFile, MidiTimeUnit -> MidiTick, MidiTiming -> MidiAbsolute},
      ReplacePart[{#[[1]],
        If[MatchQ[#[[2]], _NoteOn], {MidiNoteOn, {#[[2, 3]]-1, #[[2, 1]], #[[2, 2]]}},
          If[MatchQ[#[[2]], _NoteOff], {MidiNoteOff, {#[[2, 3]]-1, #[[2, 1]], #[[2, 2]]}},
            If[MatchQ[#[[2]], _Sysex], {#[[2, 1, 1]], Drop[#[[2, 1]], 1]},
              If[MatchQ[#[[2]], _MetaEvent], {{MidiMeta, #[[2, 1]]}, #[[2, 2]]},
                Null (* this is just a starting point since we will miss all channel-events but notes *)
                ]
              ]
            ]
          ]
        }& /@ #, List, {0}]& /@ ReplacePart[x, List, {0}]
      ]
    ]

MidiToEventList[m_] :=
  Module[{},
    EventList[MIDI, Tick, {},Evaluate[{MIDIFileFormat -> (MidiFileFormat /. m[[1]] /. Options[Midi]), TPQ -> (MidiTPQ /. m[[1]] /. Options[Midi])}]] @@ (
      (Track @@ (Event[#[[1]],
        If[#[[2, 1]] === MidiNoteOn, NoteOn[#[[2, 2, 2]], #[[2, 2, 3]], #[[2, 2, 1]]+1],
          If[#[[2, 1]] === MidiNoteOff,NoteOff[#[[2, 2, 2]], #[[2, 2, 3]], #[[2, 2, 1]]+1],
            If[#[[2, 1]] === MidiSysX0, Sysex[Prepend[#[[2, 2]],MidiSysX0]],
              If[#[[2, 1]] === MidiSysX7, Sysex[Prepend[#[[2, 2]],MidiSysX7]],
                If[ListQ[#[[2]]] && #[[2, 1, 1]] === MidiMeta, MetaEvent[#[[2, 1, 2]], #[[2, 2]]],
                  Null (* this is just a starting point since we will miss all channel-events but notes *)
                  ]
                ]
              ]
            ]
          ]
        ] & /@ #)) & /@
        MidiSetState[m, {MidiShape->MidiFile, MidiTiming->MidiAbsolute, MidiTimeUnit->MidiTick}][[2]]
      )
    ]

End[]

Protect[
  EventListToMidi,
  MidiToEventList
  ];

EndPackage[]
