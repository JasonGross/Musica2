(* :Title: ValueObject *)

(* :Summary: Functions for ValueObject *)

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

(* :Context: Musica2`ValueObject` *)

(* :History:
  2005-02-13  bch :  reorganized code in file, hopefully in an uniform manner
  2005-02-12  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`ValueObject`",
  {
    "Musica2`Common`",
    "Musica2`Midi`",
    "Musica2`Note`",
    "Musica2`ObjectType`",
    "Musica2`Sound`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert,
  Par,
  Seq
  ];

Unprotect[
  ValueObject
  ];

CreateElement[DurationObject, {
  Object_,
  DurationFunction_,
  TrackFunction_,
  SnippetFunction_
  }, {DataNoValue, 0&, Track[{DataNoValue,0}]&, Snippet[{DataNoValue,0}]&},
"todo."
];

CreateContainer[DurationObjectTrack, DurationObject,
"todo."
];
     
CreateElement[DurationValue, {
  Duration_,
  Value_
  }, {1, Null},
"todo"
];

CreateContainer[DurationValueTrack, DurationValue,
"todo"
]

CreateElement[EventMessage, {
  EventType_,
  EventData_
  }, {EventTypeEOT, {}}, 
"todo"
];

CreateElement[TimeObject, {
  Object_DurationObject,
  Time_
  }, {DurationObject[], 0},
"todo."
];

CreateContainer[TimeObjectTrack, TimeObject,
"todo."
];

CreateElement[TypeValue, {
  ValueType_,
  Value_DurationValueTrack
  }, {0, DurationValueTrack[{}]},
"todo"
];

ValueObject::usage = "todo"

Begin["`Private`"]

(* DurationObject ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationObject modifications and interceptions *******************************************)

DataDeep[DurationObject] = Function[x,
  ReplacePart[Data[x],DataDeep[Object[x]],Pos[DurationObject,Object]]
  ]

(* DurationObject constructors **************************************************************)

DurationObject[{DataNoValue,d_}] := DurationObject[{d,#&,Track[{DataNoValue,#}]&,Snippet[{DataNoValue,#}]&}]

DurationObject[{x_DurationObjectTrack}] := DurationObject[{x,TotalDuration,Track,Snippet}]
DurationObject[x_TimeObjectTrack] := DurationObject[{x,TotalDuration,Track,Snippet}]
DurationObject[x_TypeValue] := DurationObject[{x,TotalDuration,Track,Snippet[{DataNoValue,TotalDuration[#]}]&}]

DurationObject[x_Chord] := DurationObject[{x,TotalDuration,Track,Snippet}]
DurationObject[x_Counterpoint] := DurationObject[{x,TotalDuration,Track,Snippet}]
DurationObject[x_Event] := DurationObject[{x,EventTime&,Track,Snippet}]
DurationObject[x_EventMessage] := DurationObject[{x,Duration,Track,Snippet}]
DurationObject[x_Melody] := DurationObject[{x,TotalDuration,Track,Snippet}]
DurationObject[x_Note] := DurationObject[{x,Duration,Track,Snippet}]
DurationObject[x_Progression] := DurationObject[{x,TotalDuration,Track,Snippet}]
DurationObject[x_Snippet] := DurationObject[{x,Duration,Track[{DataNoValue,Duration[#]}]&,#&}]
DurationObject[x_Track] := DurationObject[{x,TotalDuration,#&,Snippet}]

(* DurationObject reverse constructors ******************************************************)

DurationObject /: Track[x_DurationObject] := TrackFunction[x][Object[x]]
DurationObject /: Snippet[x_DurationObject] := SnippetFunction[x][Object[x]]

(* DurationObject common functions **********************************************************)

DurationObject /: Duration[x_DurationObject] := DurationFunction[x][Object[x]]

(* DurationObject unique functions **********************************************************)

(* DurationObject tests *********************************************************************)

DurationObject /: TestSuite[DurationObject] = Join[TestSuite[DurationObject],{
  }];

(* DurationObject --------------------------------------------------------------------------*)

(* DurationObjectTrack +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationObjectTrack modifications and interceptions **************************************)

(* DurationObjectTrack constructors *********************************************************)

DurationObjectTrack[x_TimeObjectTrack]:=
  Module[{to=TimeObject[Sort[x,Time]],r={}},
    Scan[
      Function[toi,
        r=#[[2]]&/@Sort[{TotalDuration[#],#}&/@r];
        If[r==={}||Time[toi]<TotalDuration[r[[1]]],
          r=Prepend[r,DurationObjectTrack[{}]]
          ];
        If[TotalDuration[r[[1]]]<Time[toi],
          r[[1]]=AppendTo[r[[1]],DurationObject[{DataNoValue,Time[toi]-TotalDuration[r[[1]]]}]];
          ];
        r[[1]]=AppendTo[r[[1]],Object[toi]];
        ],
      to
      ];
    r
    ]

DurationObjectTrack[x_Melody] := DurationObjectTrack[DurationObject/@Note[x]]
DurationObjectTrack[x_Progression] := DurationObjectTrack[DurationObject/@Chord[x]]

(* DurationObjectTrack reverse constructors *************************************************)

DurationObjectTrack /: Track[x_DurationObjectTrack] := Seq[Track/@DurationObject[x]]
DurationObjectTrack /: Snippet[x_DurationObjectTrack] := Seq[Snippet/@Select[DurationObject[x],0<Duration[#]&]]

(* DurationObjectTrack common functions *****************************************************)

DurationObjectTrack /: Duration[x_DurationObjectTrack] := Duration/@DurationObject[x]
DurationObjectTrack /: TotalDuration[x_DurationObjectTrack] := Total[Duration[x]]

(* DurationObjectTrack unique functions *****************************************************)

(* DurationObjectTrack tests ****************************************************************)

DurationObjectTrack /: TestSuite[DurationObjectTrack] = Join[TestSuite[DurationObjectTrack],{
  }];

(* DurationObjectTrack ---------------------------------------------------------------------*)

(* DurationValue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationValue modifications and interceptions ********************************************)

(* DurationValue constructors ***************************************************************)

DurationValue[x_Chord] := DurationValue[{TotalDuration[x],Data[x]},Sequence@@RemOpts[Opts[x],Duration]]
DurationValue[x_Note]  := ReplacePart[x,DurationValue,0]

(* DurationValue reverse constructors *******************************************************)

DurationValue /: Chord[x_DurationValue] := Chord[Value[x],Sequence@@AddOpts[Opts[x],Duration\[Rule]Duration[x]]]
DurationValue /: Note[ x_DurationValue] := ReplacePart[x,Note,0]

(* DurationValue common functions ***********************************************************)

Par[x : {__DurationValue}]  := DurationValue[{Duration[x[[1]]], Value /@ x}] /; (Length[Union[Duration /@ x]] === 1)
Par[x : {__DurationValue}]  := Par[DurationValueTrack /@ x]                  /; (Length[Union[Duration /@ x]] =!= 1)

Seq[x:{__DurationValue}]    := DurationValueTrack[x]

DurationValue /: TotalDuration[x_DurationValue] := Duration[x]

DurationValue /: UnPar[x_DurationValue] := DurationValue[{Duration[x], #}]& /@ Value[x]

(* DurationValue unique functions ***********************************************************)

(* DurationValue tests **********************************************************************)

DurationValue /: TestSuite[DurationValue] = Join[TestSuite[DurationValue],{
  }];

(* DurationValue ---------------------------------------------------------------------------*)

(* DurationValueTrack ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationValueTrack modifications and interceptions ***************************************)

Tidy[DurationValueTrack] = Function[x,
  Module[{r = x, i},
    (* a leading tie is regarded as no - value *)
    If[0 < Length[r] && DataTieQ[r[[1, Value]]],
      r = ReplacePart[r, DataNoValue[DataUnTie[r[[1, Value]]]], 1, Value];
      ];
    For[i = 1, i < Length[r], i++,
      (* merge repetitions and ties *)
      If[r[[i, Value]] === r[[i + 1, Value]] ||
          DataTieQ[r[[i + 1, Value]]],
        r = ReplacePart[r, r[[i, Duration]] + r[[i + 1, Duration]], i, Duration];
        r = Delete[r, i + 1];
        i--;
        Continue[]
        ];
      (* delete zero - durations *)
      If[r[[i, Duration]] === 0,
        r = Delete[r, i];
        i--;
        Continue[]
        ];
      ];
    r
    ]
  ];

(* DurationValueTrack constructors **********************************************************)

DurationValueTrack[x_Counterpoint] := DurationValueTrack[Par[DurationValueTrack /@ Melody[x]], Sequence @@ Opts[x]]
DurationValueTrack[x_Melody]       := ReplacePart[x,DurationValueTrack,0]
DurationValueTrack[x_Progression]  := DurationValueTrack[Seq[DurationValue /@ Chord[x]], Sequence @@ Opts[x]]

(* DurationValueTrack reverse constructors **************************************************)

DurationValueTrack /: Counterpoint[x_DurationValueTrack] := Counterpoint[Melody /@ UnPar[x], Sequence @@ Opts[x]]
DurationValueTrack /: Melody[      x_DurationValueTrack] := Melody[Note /@ x, Sequence @@ Opts[x]]
DurationValueTrack /: Progression[ x_DurationValueTrack] := Progression[Chord /@ x, Sequence @@ Opts[x]]

(* DurationValueTrack common functions ******************************************************)

Convert[Time, Time, x_DurationValueTrack]:=
  Module[{d = Duration[x]},
    MakeNestedIfs[
      Transpose[{d,Partition[DeltasToValues[d],2,1]}],
      {-\[Infinity], 0},
      {Total[d],\[Infinity]}
      ]
    ]

Convert[Time, Value, x_DurationValueTrack] :=
  MakeNestedIfs[
    Data /@ x,
    DataNoValue[x[[1,Value]]]
    ]

Par[x:{__DurationValueTrack}]:= DurationValueTrack[DurationValue /@ ParOfSeqToSeqOfPar[Data[#]& /@ x]]

Seq[x:{__DurationValueTrack}]:= DurationValueTrack[Flatten[Data[#]& /@ x,1]]

DurationValueTrack /: TotalDuration[x_DurationValueTrack] := Total[Duration[x]]

DurationValueTrack /: UnPar[x_DurationValueTrack] := DurationValueTrack[Transpose[{Duration[x],#}]]& /@ Transpose[Value[x]]

DurationValueTrack /: UnSeq[x_DurationValueTrack] := DurationValue[x]

(* DurationValueTrack unique functions ******************************************************)

(* DurationValueTrack tests *****************************************************************)

DurationValueTrack /: TestSuite[DurationValueTrack] = Join[TestSuite[DurationValueTrack],{
  }];

(* DurationValueTrack ----------------------------------------------------------------------*)

(* EventMessage ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* EventMessage modifications and interceptions *********************************************)

(* EventMessage constructors ****************************************************************)

EventMessage[DataNoValue] := EventMessage[]
EventMessage[x_Event] := EventMessage[{EventType[x],EventData[x]}]

(* EventMessage reverse constructors ********************************************************)

EventMessage /: Event[x_EventMessage] := Event[{0,Data[x]}]
EventMessage /: Track[x_EventMessage] := Track[Event[x]]
EventMessage /: Snippet[x_EventMessage] := Snippet[DurationObject[{DataNoValue,0}]]

(* EventMessage common functions ************************************************************)

EventMessage /: Duration[x_EventMessage] := 0

(* EventMessage unique functions ************************************************************)

(* EventMessage tests ***********************************************************************)

EventMessage /: TestSuite[EventMessage] = Join[TestSuite[EventMessage],{
  }];

(* EventMessage ----------------------------------------------------------------------------*)

(* TimeObject ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TimeObject modifications and interceptions ***********************************************)

DataDeep[TimeObject] = Function[x,
  ReplacePart[Data[x],DataDeep[Object[x]],Pos[TimeObject,Object]]
  ]

(* TimeObject constructors ******************************************************************)

TimeObject[x_Event] := TimeObject[{DurationObject[EventMessage[x]],EventTime[x]}]
TimeObject[{x_EventMessage,t_}] := TimeObject[{DurationObject[x],t}]

(* TimeObject reverse constructors **********************************************************)

TimeObject /: Track[x_TimeObject] := Map[#+Time[x]&,Track[Object[x]],EventTime]
TimeObject /: Snippet[x_TimeObject] := Seq[Snippet/@{DurationObject[{DataNoValue,Time[x]}],Object[x]}]
  
(* TimeObject common functions **************************************************************)

TimeObject /: Duration[x_TimeObject] := Duration[Object[x]]

(* TimeObject unique functions **************************************************************)

(* TimeObject tests *************************************************************************)

TimeObject /: TestSuite[TimeObject] = Join[TestSuite[TimeObject],{
  }];

(* TimeObject ------------------------------------------------------------------------------*)

(* TimeObjectTrack +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TimeObjectTrack modifications and interceptions ******************************************)

(* TimeObjectTrack constructors *************************************************************)

TimeObjectTrack[x_DurationObjectTrack] :=
  TimeObjectTrack[
    Transpose[{DurationObject[x],Drop[DeltasToValues[Duration[x]],-1]}]
    ]

TimeObjectTrack[x_Track] := TimeObjectTrack[TimeObject/@Event[x]]

(* TimeObjectTrack reverse constructors *****************************************************)

TimeObjectTrack /: Track[x_TimeObjectTrack] := Mix[Track/@TimeObject[x]]
TimeObjectTrack /: Snippet[x_TimeObjectTrack] := Mix[Snippet/@DurationObjectTrack[x]]

(* TimeObjectTrack common functions *********************************************************)

TimeObjectTrack /: Duration[x_TimeObjectTrack] := Duration[Object[#]]&/@TimeObject[x]
TimeObjectTrack /: TotalDuration[x_TimeObjectTrack] := Max[(Time[#]+Duration[Object[#]])&/@TimeObject[x]]

(* TimeObjectTrack unique functions *********************************************************)

(* TimeObjectTrack tests ********************************************************************)

TimeObjectTrack /: TestSuite[TimeObjectTrack] = Join[TestSuite[TimeObjectTrack],{
  }];

(* TimeObjectTrack -------------------------------------------------------------------------*)

(* TypeValue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TypeValue modifications and interceptions ************************************************)

DataDeep[TypeValue] = Function[x,
  ReplacePart[Data[x],DataDeep[Value[x]],Pos[TypeValue,Value]]
  ]

(* TypeValue constructors *******************************************************************)

TypeValue[x_Counterpoint] := TypeValue[{{1, MidiChannel /. Opts[x] /. {MidiChannel -> 0}}, DurationValueTrack[x]}, Sequence @@ Opts[x]]
TypeValue[x_Melody]       := TypeValue[{{1, MidiChannel /. Opts[x] /. {MidiChannel -> 0}}, DurationValueTrack[x]}, Sequence @@ Opts[x]]
TypeValue[x_Progression]  := TypeValue[{{1, MidiChannel /. Opts[x] /. {MidiChannel -> 0}}, DurationValueTrack[x]}, Sequence @@ Opts[x]]

TypeValue[x_Track, t_] := TypeValue /@ Melody[x] /; (t === EventTypeNoteOn)

TypeValue[x_Track, t_] :=
  Module[{y = Tidy[x], z, d, v},
    z = Data /@ Event[Select[y, MatchQ[Data[#][[2]], t] &]];
    If[z === {} || z[[1, 1]] =!= 0,
      z = Prepend[z,
        {0, {t[[1]], If[z =!= {}, DataNoValue[z[[1, 2, 2]]], DataNoValue]}}
        ]
      ];
    {d, v} = Transpose[z];
    d = ValuesToDeltas[Append[d, y[[-1, EventTime]]]];
    v = #[[2]] & /@ v;
    TypeValue[{t[[1]], DurationValueTrack[Transpose[{d, v}]]}]
    ] /; (t =!= EventTypeNoteOn)

TypeValue[x_Track] :=
  Module[{t = Union[EventType /@ Event[x]]},
    TypeValue[x, {#,_}] & /@ t
    ]

(* TypeValue reverse constructors ***********************************************************)

TypeValue /: Track[x_TypeValue?(MatchQ[Data[#], {{EventTypeNoteOn, _}, _}] &)] :=
  Module[
    {
      t = ValueType[x],
      d = Partition[DeltasToValues[Duration[Value[x]]], 2, 1],
      v = Value[Value[x]],
      dv
      },
    dv = Transpose[{d, v}];
    dv = Select[dv, DataPlainValueQ[#[[2]]] &];
    dv = Flatten[
      {
        {#[[1, 1]], {{EventTypeNoteOn, t[[2]]}, #[[2]]}},
        {#[[1, 2]], {{EventTypeNoteOff, t[[2]]}, #[[2]]}}
        } & /@ dv,
      1];
    dv = Append[dv, {d[[-1, 2]], {EventTypeEOT, {}}}];
    Track[Event /@ dv]
    ]

TypeValue /: Track[x_TypeValue] :=
  Module[
    {
      t = ValueType[x],
      d = Partition[DeltasToValues[Duration[Value[x]]], 2, 1],
      v = Value[Value[x]],
      dv
      },
    dv = Transpose[{d, v}];
    dv = Select[dv, DataPlainValueQ[#[[2]]] &];
    dv = {#[[1, 1]], {t, #[[2]]}} & /@ dv;
    dv = Append[dv, {d[[-1, 2]], {EventTypeEOT, {}}}];
    Track[Event /@ dv]
    ]

TypeValue /: Counterpoint[x_TypeValue] := Counterpoint[Melody[#,MidiChannel->ValueType[x][[2]]]& /@ UnPar[x], Sequence @@ Opts[x]]
TypeValue /: Melody[      x_TypeValue] := Melody[Note /@ x, Sequence @@ AddOpts[Opts[x],MidiChannel->ValueType[x][[2]]]]
TypeValue /: Progression[ x_TypeValue] := Progression[Chord /@ x, Sequence @@ Opts[x]]

(* TypeValue common functions ***************************************************************)

Par[x:{__TypeValue}]:= TypeValue[{ValueType /@ x,Par[Value /@ x]}]

Tidy[TypeValue] = Function[x,TypeValue[{ValueType[x], Tidy[Value[x]]}, Sequence @@ Opts[x]]];

TypeValue /: TotalDuration[x_TypeValue] := TotalDuration[Value[x]]

TypeValue /: UnPar[x_TypeValue] := TypeValue /@ Transpose[{ValueType[x],UnPar[Value[x]]}]

(* TypeValue unique functions ***************************************************************)

(* TypeValue tests **************************************************************************)

TypeValue /: TestSuite[TypeValue] = Join[TestSuite[TypeValue],{
  }];

(* TypeValue -------------------------------------------------------------------------------*)

(* Not yet "released"

Track[x : {__TypeValue}] := Tidy[Mix[Track /@ x]]
Midi[x : {{__TypeValue}..}, opts___?OptionQ] := Midi[Track /@ x, opts]
TypeValue[x_Midi, t_] := TypeValue /@ Melody[x] /; (t === EventTypeNoteOn)
TypeValue[x_Midi] := TypeValue /@ Track[x]

*)

End[]

Protect[
  ValueObject
  ];

Protect[
  Convert,
  Par,
  Seq
  ];

EndPackage[]
