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
  2005-02-16  bch :  initiated usage of Usage ;-)
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
    "Musica2`Usage`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert,
  Par,
  Seq
  ];

Unprotect[
  ValueObject,
  DurationObjectTrack
  ];

CreateElement[Musica2,EventMessage, {
  EventType_,
  EventData_
  }, {EventTypeEOT, {}}, 
"todo"
];

CreateElement[Musica2,TimedObject, {
  Object_,
  Time_
  }, {0, 0},
"todo."
];

CreateContainer[Musica2,TimedObjectTrack, TimedObject,
"todo."
];

CreateElement[Musica2,DurationValue, {
  Duration_,
  Value_
  }, {1, Null},
"todo"
];

CreateContainer[Musica2,DurationValueTrack, DurationValue,
"todo"
]

CreateElement[Musica2,TypeValue, {
  ValueType_,
  Value_DurationValueTrack
  }, {0, DurationValueTrack[{}]},
"todo"
];

DurationObjectTrack::usage = "todo"
ValueObject::usage = "todo"

Begin["`Private`"]

(* DurationObjectTrack +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationObjectTrack modifications and interceptions **************************************)

DurationObjectTrack /: Data[x_DurationObjectTrack] := ReplacePart[x,List,0]
DurationObjectTrack /: DataDeep[x_DurationObjectTrack] := DataDeep /@ Data[x]

Format[x_DurationObjectTrack] := "\[SkeletonIndicator]"<>SymbolName[DurationObjectTrack]<>"\[SkeletonIndicator]";

(* DurationObjectTrack constructors *********************************************************)

Usage[Musica2,DurationObjectTrack,{_List},_DurationObjectTrack,"todo"]
DurationObjectTrack[x_List] := DurationObjectTrack @@ x

Usage[Musica2,DurationObjectTrack,{_Melody},_DurationObjectTrack,"todo"]
DurationObjectTrack[x_Melody] := DurationObjectTrack[If[DataNoValueQ[#],Duration[#],#]&/@Note[x]]

Usage[Musica2,DurationObjectTrack,{_Progression},_DurationObjectTrack,"todo"]
DurationObjectTrack[x_Progression] := DurationObjectTrack[Chord[x]]

Usage[Musica2,DurationObjectTrack,{_TimedObjectTrack},{___DurationObjectTrack},"todo"]
DurationObjectTrack[x_TimedObjectTrack]:=
  Module[{to=TimedObject[Sort[x,Time]],r={}},
    Scan[
      Function[toi,
        r=#[[2]]&/@Sort[{TotalDuration[#],#}&/@r];
        If[r==={}||Time[toi]<TotalDuration[r[[1]]],
          r=Prepend[r,DurationObjectTrack[]]
          ];
        If[TotalDuration[r[[1]]]<Time[toi],
          r[[1]]=AppendTo[r[[1]],Time[toi]-TotalDuration[r[[1]]]];
          ];
        r[[1]]=AppendTo[r[[1]],Object[toi]];
        ],
      to
      ];
    r
    ]

(* DurationObjectTrack reverse constructors *************************************************)

Usage[Musica2,Snippet,{_DurationObjectTrack},_Snippet,"todo"]
DurationObjectTrack /: Snippet[x_DurationObjectTrack] := Seq[Mix[Snippet[#]]& /@ Select[Data[x], 0 < TotalDuration[#] &]]

Usage[Musica2,Track,{_DurationObjectTrack},_Track,"todo"]
DurationObjectTrack /: Track[x_DurationObjectTrack] := Seq[Mix[Track[#]]& /@ Data[x]]

(* DurationObjectTrack common functions *****************************************************)

Usage[Musica2,Duration,{_DurationObjectTrack},_List,"todo"]
DurationObjectTrack /: Duration[x_DurationObjectTrack] := TotalDuration /@ Data[x]

Usage[Musica2,TotalDuration,{_DurationObjectTrack},_,"todo"]
DurationObjectTrack /: TotalDuration[x_DurationObjectTrack] := Total[Duration[x]]

(* DurationObjectTrack unique functions *****************************************************)

(* DurationObjectTrack tests ****************************************************************)

DurationObjectTrack /: TestSuite[DurationObjectTrack] = {
  };

(* DurationObjectTrack ---------------------------------------------------------------------*)

(* EventMessage ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* EventMessage modifications and interceptions *********************************************)

(* EventMessage constructors ****************************************************************)

Usage[Musica2,EventMessage,{DataNoValue},_EventMessage,"todo"]
EventMessage[DataNoValue] := EventMessage[]

Usage[Musica2,EventMessage,{_Event},_EventMessage,"todo"]
EventMessage[x_Event] := EventMessage[{EventType[x],EventData[x]}]

(* EventMessage reverse constructors ********************************************************)

Usage[Musica2,Event,{_EventMessage},_Event,"todo"]
EventMessage /: Event[x_EventMessage] := Event[{0,Data[x]}]

Usage[Musica2,Track,{_EventMessage},_Track,"todo"]
EventMessage /: Track[x_EventMessage] := Track[Event[x]]

Usage[Musica2,Snippet,{_EventMessage},_Snippet,"todo"]
EventMessage /: Snippet[x_EventMessage] := Snippet[0]

(* EventMessage common functions ************************************************************)

Usage[Musica2,Duration,{_EventMessage},0,"todo"]
EventMessage /: Duration[x_EventMessage] := 0

Usage[Musica2,TotalDuration,{_EventMessage},0,"todo"]
EventMessage /: TotalDuration[x_EventMessage] := 0

(* EventMessage unique functions ************************************************************)

(* EventMessage tests ***********************************************************************)

EventMessage /: TestSuite[EventMessage] = Join[TestSuite[EventMessage],{
  }];

(* EventMessage ----------------------------------------------------------------------------*)

(* TimedObject +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TimedObject modifications and interceptions **********************************************)

DataDeep[TimedObject] = Function[x,
  ReplacePart[Data[x],DataDeep[Object[x]],Pos[TimedObject,Object]]
  ]

(* TimedObject constructors *****************************************************************)

Usage[Musica2,TimedObject,{_Event},_TimedObject,"todo"]
TimedObject[x_Event] := TimedObject[{EventMessage[x],EventTime[x]}]

(* TimedObject reverse constructors *********************************************************)

Usage[Musica2,Track,{_TimedObject},_Track,"todo"]
TimedObject /: Track[x_TimedObject] := Map[#+Time[x]&,Mix[Track[Object[x]]],EventTime]

Usage[Musica2,Snippet,{_TimedObject},_Snippet,"todo"]
TimedObject /: Snippet[x_TimedObject] := Seq[Mix[Snippet[#]]& /@ {Time[x],Object[x]}]
  
(* TimedObject common functions *************************************************************)

Usage[Musica2,Duration,{_TimedObject},_,"todo"]
TimedObject /: Duration[x_TimedObject] := TotalDuration[Object[x]]

Usage[Musica2,TotalDuration,{_TimedObject},_,"todo"]
TimedObject /: TotalDuration[x_TimedObject] := Duration[x]

(* TimedObject unique functions *************************************************************)

(* TimedObject tests ************************************************************************)

TimedObject /: TestSuite[TimedObject] = Join[TestSuite[TimedObject],{
  }];

(* TimedObject -----------------------------------------------------------------------------*)

(* TimedObjectTrack ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TimedObjectTrack modifications and interceptions *****************************************)

(* TimedObjectTrack constructors ************************************************************)

Usage[Musica2,TimedObjectTrack,{_DurationObjectTrack},_TimedObjectTrack,"todo"]
TimedObjectTrack[x_DurationObjectTrack] :=
  TimedObjectTrack[
    Transpose[{Data[x],Drop[DeltasToValues[Duration[x]],-1]}]
    ]

Usage[Musica2,TimedObjectTrack,{_Track},_TimedObjectTrack,"todo"]
TimedObjectTrack[x_Track] := TimedObjectTrack[TimedObject/@Event[x]]

(* TimedObjectTrack reverse constructors ****************************************************)

Usage[Musica2,Track,{_TimedObjectTrack},_Track,"todo"]
TimedObjectTrack /: Track[x_TimedObjectTrack] := Mix[Track /@ TimedObject[x]]

Usage[Musica2,Snippet,{_TimedObjectTrack},_Snippet,"todo"]
TimedObjectTrack /: Snippet[x_TimedObjectTrack] := Mix[Snippet /@ DurationObjectTrack[x]]

(* TimedObjectTrack common functions ********************************************************)

Usage[Musica2,Duration,{_TimedObjectTrack},_List,"todo"]
TimedObjectTrack /: Duration[x_TimedObjectTrack] := TotalDuration[Object[#]]&/@TimedObject[x]

Usage[Musica2,TotalDuration,{_TimedObjectTrack},_,"todo"]
TimedObjectTrack /: TotalDuration[x_TimedObjectTrack] := Max[(Time[#]+TotalDuration[Object[#]])&/@TimedObject[x]]

Usage[Musica2,Tidy,{TimedObjectTrack},_,"todo"]
Tidy[TimedObjectTrack] = Function[x,
  Sort[Select[x,!NumberQ[Object[#]]&],Time]
  ]

(* TimedObjectTrack unique functions ********************************************************)

(* TimedObjectTrack tests *******************************************************************)

TimedObjectTrack /: TestSuite[TimedObjectTrack] = Join[TestSuite[TimedObjectTrack],{
  }];

(* TimedObjectTrack ------------------------------------------------------------------------*)

(* DurationValue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationValue modifications and interceptions ********************************************)

(* DurationValue constructors ***************************************************************)

Usage[Musica2,DurationValue,{_Chord},_DurationValue,"todo"]
DurationValue[x_Chord] := DurationValue[{TotalDuration[x],Data[x]},Sequence@@RemOpts[Opts[x],Duration]]

Usage[Musica2,DurationValue,{_Note},_DurationValue,"todo"]
DurationValue[x_Note]  := ReplacePart[x,DurationValue,0]

(* DurationValue reverse constructors *******************************************************)

Usage[Musica2,Chord,{_DurationValue},_Chord,"todo"]
DurationValue /: Chord[x_DurationValue] := Chord[Value[x],Sequence@@AddOpts[Opts[x],Duration->Duration[x]]]

Usage[Musica2,Note,{_DurationValue},_Note,"todo"]
DurationValue /: Note[ x_DurationValue] := ReplacePart[x,Note,0]

(* DurationValue common functions ***********************************************************)

Usage[Musica2,Par,{{__DurationValue}},(_DurationValue|_DurationValueTrack),"todo"]
Par[x : {__DurationValue}]  := DurationValue[{Duration[x[[1]]], Value /@ x}] /; (Length[Union[Duration /@ x]] === 1)

Par[x : {__DurationValue}]  := Par[DurationValueTrack /@ x]                  /; (Length[Union[Duration /@ x]] =!= 1)

Usage[Musica2,Seq,{{__DurationValue}},_DurationValueTrack,"todo"]
Seq[x:{__DurationValue}]    := DurationValueTrack[x]

Usage[Musica2,TotalDuration,{_DurationValue},_,"todo"]
DurationValue /: TotalDuration[x_DurationValue] := Duration[x]

Usage[Musica2,UnPar,{_DurationValue},{___DurationValue},"todo"]
DurationValue /: UnPar[x_DurationValue] := DurationValue[{Duration[x], #}]& /@ Value[x]

(* DurationValue unique functions ***********************************************************)

(* DurationValue tests **********************************************************************)

DurationValue /: TestSuite[DurationValue] = Join[TestSuite[DurationValue],{
  }];

(* DurationValue ---------------------------------------------------------------------------*)

(* DurationValueTrack ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* DurationValueTrack modifications and interceptions ***************************************)

(* DurationValueTrack constructors **********************************************************)

Usage[Musica2,DurationValueTrack,{_Counterpoint},_DurationValueTrack,"todo"]
DurationValueTrack[x_Counterpoint] := DurationValueTrack[Par[DurationValueTrack /@ Melody[x]], Sequence @@ Opts[x]]

Usage[Musica2,DurationValueTrack,{_Melody},_DurationValueTrack,"todo"]
DurationValueTrack[x_Melody] := ReplacePart[x,DurationValueTrack,0]

Usage[Musica2,DurationValueTrack,{_Progression},_DurationValueTrack,"todo"]
DurationValueTrack[x_Progression] := DurationValueTrack[Seq[DurationValue /@ Chord[x]], Sequence @@ Opts[x]]

(* DurationValueTrack reverse constructors **************************************************)

Usage[Musica2,Counterpoint,{_DurationValueTrack},_Counterpoint,"todo"]
DurationValueTrack /: Counterpoint[x_DurationValueTrack] := Counterpoint[Melody /@ UnPar[x], Sequence @@ Opts[x]]

Usage[Musica2,Melody,{_DurationValueTrack},_Melody,"todo"]
DurationValueTrack /: Melody[x_DurationValueTrack] := Melody[Note /@ x, Sequence @@ Opts[x]]

Usage[Musica2,Progression,{_DurationValueTrack},_Progression,"todo"]
DurationValueTrack /: Progression[ x_DurationValueTrack] := Progression[Chord /@ x, Sequence @@ Opts[x]]

(* DurationValueTrack common functions ******************************************************)

Usage[Musica2,Convert,{Time, Time, _DurationValueTrack},_,"todo"]
Convert[Time, Time, x_DurationValueTrack]:=
  Module[{d = Duration[x]},
    MakeNestedIfs[
      Transpose[{d,Partition[DeltasToValues[d],2,1]}],
      {-\[Infinity], 0},
      {Total[d],\[Infinity]}
      ]
    ]

Usage[Musica2,Convert,{Time, Value, _DurationValueTrack},_,"todo"]
Convert[Time, Value, x_DurationValueTrack] :=
  MakeNestedIfs[
    Data /@ x,
    DataNoValue[x[[1,Value]]]
    ]

Usage[Musica2,Par,{{__DurationValueTrack}},_DurationValueTrack,"todo"]
Par[x:{__DurationValueTrack}]:= DurationValueTrack[DurationValue /@ ParOfSeqToSeqOfPar[Data[#]& /@ x]]

Usage[Musica2,Seq,{{__DurationValueTrack}},_DurationValueTrack,"todo"]
Seq[x:{__DurationValueTrack}]:= DurationValueTrack[Flatten[Data[#]& /@ x,1]]

Usage[Musica2,Tidy,{DurationValueTrack},_,"todo"]
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

Usage[Musica2,TotalDuration,{_DurationValueTrack},_,"todo"]
DurationValueTrack /: TotalDuration[x_DurationValueTrack] := Total[Duration[x]]

Usage[Musica2,UnPar,{_DurationValueTrack},{___DurationValueTrack},"todo"]
DurationValueTrack /: UnPar[x_DurationValueTrack] := DurationValueTrack[Transpose[{Duration[x],#}]]& /@ Transpose[Value[x]]

Usage[Musica2,UnSeq,{_DurationValueTrack},{___DurationValue},"todo"]
DurationValueTrack /: UnSeq[x_DurationValueTrack] := DurationValue[x]

(* DurationValueTrack unique functions ******************************************************)

(* DurationValueTrack tests *****************************************************************)

DurationValueTrack /: TestSuite[DurationValueTrack] = Join[TestSuite[DurationValueTrack],{
  }];

(* DurationValueTrack ----------------------------------------------------------------------*)

(* TypeValue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TypeValue modifications and interceptions ************************************************)

DataDeep[TypeValue] = Function[x,
  ReplacePart[Data[x],DataDeep[Value[x]],Pos[TypeValue,Value]]
  ]

(* TypeValue constructors *******************************************************************)

Usage[Musica2,TypeValue,{_Counterpoint},_TypeValue,"todo"]
TypeValue[x_Counterpoint] := TypeValue[{{1, MidiChannel /. Opts[x] /. {MidiChannel -> 0}}, DurationValueTrack[x]}, Sequence @@ Opts[x]]

Usage[Musica2,TypeValue,{_Melody},_TypeValue,"todo"]
TypeValue[x_Melody] := TypeValue[{{1, MidiChannel /. Opts[x] /. {MidiChannel -> 0}}, DurationValueTrack[x]}, Sequence @@ Opts[x]]

Usage[Musica2,TypeValue,{_Progression},_TypeValue,"todo"]
TypeValue[x_Progression] := TypeValue[{{1, MidiChannel /. Opts[x] /. {MidiChannel -> 0}}, DurationValueTrack[x]}, Sequence @@ Opts[x]]

Usage[Musica2,TypeValue,{_Track,_},{___TypeValue},"todo"]
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

Usage[Musica2,TypeValue,{_Track},{{___TypeValue}...},"todo"]
TypeValue[x_Track] :=
  Module[{t = Union[EventType /@ Event[x]]},
    TypeValue[x, {#,_}] & /@ t
    ]

(* TypeValue reverse constructors ***********************************************************)

Usage[Musica2,Track,{_TypeValue},_Track,"todo"]
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

Usage[Musica2,Counterpoint,{_TypeValue},_Counterpoint,"todo"]
TypeValue /: Counterpoint[x_TypeValue] := Counterpoint[Melody[#,MidiChannel->ValueType[x][[2]]]& /@ UnPar[x], Sequence @@ Opts[x]]

Usage[Musica2,Melody,{_TypeValue},_Melody,"todo"]
TypeValue /: Melody[x_TypeValue] := Melody[Note /@ x, Sequence @@ AddOpts[Opts[x],MidiChannel->ValueType[x][[2]]]]

Usage[Musica2,Progression,{_TypeValue},_Progression,"todo"]
TypeValue /: Progression[x_TypeValue] := Progression[Chord /@ x, Sequence @@ Opts[x]]

(* TypeValue common functions ***************************************************************)

Usage[Musica2,Par,{{__TypeValue}},_TypeValue,"todo"]
Par[x:{__TypeValue}]:= TypeValue[{ValueType /@ x,Par[Value /@ x]}]

Usage[Musica2,Tidy,{TypeValue},_,"todo"]
Tidy[TypeValue] = Function[x,TypeValue[{ValueType[x], Tidy[Value[x]]}, Sequence @@ Opts[x]]];

Usage[Musica2,TotalDuration,{TypeValue},_,"todo"]
TypeValue /: TotalDuration[x_TypeValue] := TotalDuration[Value[x]]

Usage[Musica2,UnPar,{TypeValue},{___TypeValue},"todo"]
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
  ValueObject,
  DurationObjectTrack
  ];

Protect[
  Convert,
  Par,
  Seq
  ];

EndPackage[]
