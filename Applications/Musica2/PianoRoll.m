(* :Title: PianoRoll *)

(* :Summary: Functions for PianoRoll *)

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

(* :Context: Musica2`PianoRoll` *)

(* :History:
  2005-01-15  bch :  added TestSuite
  2005-01-07  bch :  added conversions from Note and its Collections
                     converted TimeUnit in Midi to Tick
  2004-11-29  bch :  bugfix
  2004-10-26  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`PianoRoll`",
  {
    "Musica2`Common`",
    "Musica2`Midi`",
    "Musica2`Note`",
    "Musica2`ObjectType`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  PianoRoll,
  MinTime,
  MaxTime,
  MinNote,
  MaxNote
  ];

PianoRoll::usage = "todo"
MinTime::usage = "todo"
MaxTime::usage = "todo"
MinNote::usage = "todo"
MaxNote::usage = "todo"

Begin["`Private`"]

Options[PianoRoll] = {MinTime -> 0, MaxTime -> Infinity, MinNote -> Automatic, MaxNote -> Automatic}

PianoRoll[x_Note,         opts___] := PianoRoll[Midi[x,opts],opts]
PianoRoll[x_Melody,       opts___] := PianoRoll[Midi[x,opts],opts]
PianoRoll[x_Chord,        opts___] := PianoRoll[Midi[x,opts],opts]
PianoRoll[x_Counterpoint, opts___] := PianoRoll[Midi[x,opts],opts]
PianoRoll[x_Progression,  opts___] := PianoRoll[Midi[x,opts],opts]

PianoRoll[mx_Midi, opts___] :=
  Module[{m = Midi[mx, TimeUnit->Tick],duration},
    duration = TotalDuration[m];
    Module[{
        n = Note[m],
        p,
        curtpq = Max[1, TPQ[m]],
        mintime = MinTime /. {opts} /. Options[PianoRoll],
        maxtime = MaxTime /. {opts} /. Options[PianoRoll],
        minnote = MinNote /. {opts} /. Options[PianoRoll],
        maxnote = MaxNote /. {opts} /. Options[PianoRoll],
        x1 = 3, x2 = 35, y = 15
        },

      p = Select[PitchCode /@ Flatten[n],DataPlainValueQ];
      If[minnote === Automatic, minnote = Min[p]-4];
      If[maxnote === Automatic, maxnote = Max[p]+4];

      mintime = Max[0, mintime];
      maxtime = Min[duration, maxtime];
      minnote = Max[0, minnote];
      maxnote = Min[127, maxnote];

      mintime = If[duration <= mintime, 0, mintime];
      maxtime = If[maxtime <= mintime, duration, maxtime];
      minnote = If[127 < minnote, 0, minnote];
      maxnote = If[maxnote < minnote, 127, maxnote];

      Show[
        Graphics[
          {(* a list of areas for drawing *)
            Rectangle[{0, 0}, {x1, y},
              Graphics[
                {(* a list of functioncalls returning drawing primitives *)
                  PianoRollPiano[minnote, maxnote]
                  },
                PlotRange -> {{0, 100}, {minnote, maxnote}},
                AspectRatio -> y/x1]],
            Rectangle[{x1, 0}, {x2, y},
              Graphics[
                {(* a list of functioncalls returning drawing primitives *)
                  PianoRollTonics[m, mintime, maxtime, minnote, maxnote],
                  PianoRollNoteLines[m, mintime, maxtime, minnote, maxnote],
                  PianoRollMeasures[m, mintime, maxtime, minnote, maxnote,curtpq],
                  PianoRollNotes[n, mintime, maxtime, minnote, maxnote]
                  },
                PlotRange -> {{mintime, maxtime}, {minnote, maxnote}},
                AspectRatio -> y/(x2 - x1)]]
            }
          ],
        AspectRatio -> y/x2]
      ]
    ]

(* The PianoRoll* functions return the drawing primitives. *)

PianoRollNotes[n:{{___Note}...}, mintime_, maxtime_, minnote_, maxnote_] :=
  Module[
    {d, t, p, v},
    Select[
      Flatten[
        (
          d = Data /@ #;
          p = #[[2, 1]] & /@ d;
          v = #[[2, 2]] & /@ d;
          t = #[[1]] & /@ d;
          t = Partition[DeltasToValues[t], 2, 1];
          MapThread[
            If[!DataPlainValueQ[#3] || #2<minnote || maxnote<#2 || #1[[2]]<mintime || maxtime<#1[[1]],
              DataNoValue,
              Rectangle[
                {#1[[1]], #2},
                {#1[[2]], #2 + 1}
                ]
              ]&,
            {t, p, v}
            ]
          ) & /@ n
        ],
      DataPlainValueQ
      ]
    ]

PianoRollMeasures[m_Midi, mintime_, maxtime_, minnote_, maxnote_, curtpq_] :=
  Module[{
      t,r
      },
    t = {#[[1]], #[[2, 2, 1]] 4 curtpq/2^#[[2, 2, 2]], 4 curtpq/2^#[[2, 2, 2]] }& /@
        Select[Data /@ Event[m[[1]]], MatchQ[#, {_, {EventTypeTimeSignature, {_, _, _, _}}}] &];
    t = MapThread[{##} &, {Partition[Append[#[[1]] & /@ t, \[Infinity]], 2, 1], Drop[#, 1] & /@ t}];
    If[t=={},t={{{0,Infinity},{4curtpq,curtpq}}}];
    r = Flatten[
      Table[
        If[Mod[i, #[[2, 1]]] == 0,
          Line[{{i, minnote}, {i, maxnote + 1}}],(* v - measure lines *)
          {GrayLevel[0.7], Line[{{i, minnote}, {i, maxnote + 1}}]}(* v - beat lines *)
          ],
        {i, Max[mintime, #[[1, 1]]], Min[maxtime, #[[1, 2]]], #[[2, 2]]}
        ]& /@ t,
      1
      ];
    r
    ]

PianoRollTonics[m_Midi, mintime_, maxtime_, minnote_, maxnote_] :=
  Module[{
      t,r
      },
    t = {#[[1]], If[128 <= #[[2, 2, 1]], #[[2, 2, 1]] - 256, #[[2, 2, 1]]], #[[2, 2, 2]] }& /@
        Select[Data /@ Event[m[[1]]], MatchQ[#, {_, {EventTypeKeySignature, {__}}}] &];
    t = MapThread[{##} &, {Partition[Append[#[[1]] & /@ t, \[Infinity]], 2, 1], Drop[#, 1] & /@ t}];
    t = {#[[1]], Mod[#[[2, 1]]*7 - #[[2, 2]]*3, 12]} & /@ t;
    If[t=={},t={{{0,Infinity},0}}];
    r = {
      RGBColor[1, .6, .6],
      Flatten[
        Table[
          Line[{{Max[mintime, #[[1, 1]]], i + 0.5}, {Min[maxtime, #[[1, 2]] - 1], i + 0.5}}],(* h - tonic lines *)
          {i, Ceiling[(minnote - #[[2]])/12]*12 + #[[2]], maxnote, 12}
          ]& /@ t,
        1
        ]
      };
    r
    ]

PianoRollNoteLines[m_Midi, mintime_, maxtime_, minnote_, maxnote_] :=
  Module[{
      },
    {(* h - note lines *)
      RGBColor[.9, .9, 1],
      Table[Line[{{mintime, i}, {maxtime - 1, i}}], {i, minnote, maxnote}]
      }
    ]

PianoRollPiano[minnote_, maxnote_] :=
  Module[{
      h0 = 0, h1 = 66, h2 = 100,
      middlec = 60
      },
    {
      (* black keys *)
      Map[
        Rectangle[{h0, #}, {h1, # + 1}] &,
        Cases[
          Flatten[Map[(# + {1, 3, 6, 8, 10})&,Table[i, {i, 0, maxnote, 12}]]],
          p_ /; (minnote <= p <= maxnote) -> p
          ]
        ],
      (* h - line after black keys *)
      Map[
        Line[{{h1, # + 0.5}, {h2, # + 0.5}}] &,
        Cases[
          Flatten[Map[(# + {1, 3, 6, 8, 10}) &,Table[i, {i, 0, maxnote, 12}]]],
          p_ /; (minnote <= p < maxnote) -> p
          ]
        ],
      (* h - line between white keys *)
      Map[
        Line[{{h0, #}, {h2, #}}] &,
        Cases[
          Flatten[Map[(# + {0, 5}) &, Table[i, {i, 0, maxnote, 12}]]],
          p_ /; (minnote <= p <= maxnote) -> p
          ]
        ],
      {
        (* the rectangle on middle - c *)
        RGBColor[1, 0, 0],
        Rectangle[{h1 + (h2 - h1)1/4, middlec + .2}, {h1 + (h2 - h1) 3/4,middlec + .8}]
        },
      (* the boundrys *)

      Line[{{h2 - 1, minnote}, {h2 - 1, maxnote + 1}}]
      }
    ]

PianoRoll /: TestSuite[PianoRoll] := {}
    
End[]

Protect[
  PianoRoll,
  MinTime,
  MaxTime,
  MinNote,
  MaxNote
  ];

EndPackage[]
