(* :Title: DurVal *)

(* :Summary: Functions for DurVal *)

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

(* :Context: Musica2`DurVal` *)

(* :History:
  2005-01-24  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`DurVal`",
  {
    "Musica2`Common`",
    "Musica2`ObjectType`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert,
  Par,
  Seq
  ];

CreateElement[DurVal, {Duration_, Value_}, {1, Null}, "todo"]
CreateContainer[DurValList, DurVal,
"DurVal is experimental and might change A LOT or even be REMOVED.\[NewLine]"
]
CreateElement[DurValType, {ValueType_, ValueList_DurValList}, {0, DurValList[{}]},
"DurValType is experimental and might change A LOT or even be REMOVED.\[NewLine]"
]

Begin["`Private`"]

Convert[Time, Time, x_DurValList]:=
  Module[{d = Duration[x]},
    MakeNestedIfs[
      Transpose[{d,Partition[DeltasToValues[d],2,1]}],
      {-\[Infinity], 0},
      {Total[d],\[Infinity]}
      ]
    ]
Convert[Time, Value, x_DurValList] := MakeNestedIfs[Data /@ x,DataNoValue[x[[1,Value]]]]

Par[x : {__DurVal}]  := DurVal[{Duration[x[[1]]], Value /@ x}] /; (Length[Union[Duration /@ x]] === 1)
Par[x : {__DurVal}]  := Par[DurValList /@ x]                   /; (Length[Union[Duration /@ x]] =!= 1)
Par[x:{__DurValList}]:=DurValList[DurVal /@ ParOfSeqToSeqOfPar[Data[#]& /@ x]]
Par[x:{__DurValType}]:=DurValType[{ValueType /@ x,Par[ValueList /@ x]}]

Seq[x:{__DurVal}]    := DurValList[x]
Seq[x:{__DurValList}]:=DurValList[Flatten[Data[#]& /@ x,1]]

Tidy[DurValList] = Function[x,
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

Tidy[DurValType] = Function[x,DurValType[{ValueType[x], Tidy[ValueList[x]]}, Sequence @@ Opts[x]]];

DurVal     /: TotalDuration[x_DurVal]     := Duration[x]
DurValList /: TotalDuration[x_DurValList] := Total[Duration[x]]
DurValType /: TotalDuration[x_DurValType] := TotalDuration[ValueList[x]]

DurVal     /: UnPar[x_DurVal]     := DurVal[{Duration[x], #}]& /@ Value[x]
DurValList /: UnPar[x_DurValList] := DurValList[Transpose[{Duration[x],#}]]& /@ Transpose[Value[x]]
DurValType /: UnPar[x_DurValType] := DurValType /@ Transpose[{ValueType[x],UnPar[ValueList[x]]}]

DurValList /: UnSeq[x_DurValList] := DurVal[x]

End[]

Protect[
  Convert,
  Par,
  Seq
  ];

EndPackage[]
