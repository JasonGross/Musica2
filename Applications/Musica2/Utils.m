(* :Title: Utils *)

(* :Summary: Common utility functions not belonging to a perticular package *)

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

(* :Context: Musica2`Utils` *)

(* :History:
  2004-08-27  bch :  added ToDoString
  2004-08-26  bch :  added some help/usage-text
  2004-08-11  bch :  moved FuncToList & ListToFunc from Utils.m to Sound.m
  2004-08-10  bch :  used the ListInterpolation function inside ListToFunc
                     changed FunctionQ and UnCompile to also handle InterpolatingFunction's
  2004-08-08  bch :  just found out about Composition which has now replaced ReArg1
                     also removed Func1Normalize, only used by ReArg1 and quite a silly function actually
                     changed FunctionQ and UnCompile to also handle Composition's
  2004-08-07  bch :  moved MakeInitDotEm[] to Setup.m
  2004-08-06  bch :  added EventList to the list of pkg's
  2004-08-04  bch :  first release
  2004-07-28  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Utils`",
  {
    "DiscreteMath`Tree`"
    }
  ]

Unprotect[
  DataAnyValue,
  DataAnyValueQ,
  DataNoValue,
  DataNoValueQ,
  DataTie,
  DataTieQ,
  DataUnTie,
  DeltasToValues,
  FunctionQ,
  MakeNestedIfs,
  NormalizeList,
  ParOfSeqToSeqOfPar,
  SeqOfParToParOfSeq,
  ToDoString,
  UnCompile,
  ValuesToDeltas
  ];

ToDoString::usage = "ToDoString is the string that separates the actual usage-text from the appended todo-text."
Begin["`Private`"]
ToDoString = "\[NewLine]\[NewLine]ToDo: "
End[]

DataAnyValue::usage = "The symbol indicating non-empty data. Will be handy..."
DataAnyValueQ::usage = "DataAnyValueQ[expr_] tests if expr is/contains the symbol DataAnyValue, tied or not."
DataNoValue::usage = "The symbol indicating no-data, like a rest."
DataNoValueQ::usage = "DataNoValueQ[expr_] tests if expr is/contains the symbol DataNoValue, tied or not."
DataTie::usage = "A symbol that indicates a tie when in shape MidiChord. Well, its actually also a function, the opposite to calling DataUnTie."
DataTieQ::usage = "DataTieQ[expr_] tests if expr is/contains a tie or not."
DataUnTie::usage = "DataUnTie[d_], the opposite to calling DataTie."
DeltasToValues::usage = "DeltasToValues[d_List, c_Integer:0] is the opposite to ValuesToDeltas";
FunctionQ::usage = "FunctionQ[expr_] tests if expr is a function."
MakeNestedIfs::usage = "MakeNestedIfs[de$:{{_,_}...}, default$_] and MakeNestedIfs[de$:{{_,_}...}, defaultLo$_, defaultHi$_] creates a function containing nested if's. The de-argument is a list of {delta,expr} which describes the function to return. The default-argument is the expr the function returned will return when called whith a parameter outside the normal range and defaults to 0 (zero)."
NormalizeList::usage = "NormalizeList[d_,opts___] takes a list of numbers and normalize them to range from -1 to 1. opts can take PlayRange->{lo,hi} as an argument which otherwise will be calculated."
ParOfSeqToSeqOfPar::usage = "todo"
SeqOfParToParOfSeq::usage = "todo"
UnCompile::usage = "UnCompile[f_] returns f in an uncompiled version."
ValuesToDeltas::usage = "ValuesToDeltas[v_List] is the opposite to DeltasToValues and calculates the deltas between the values in the list.";

Begin["`Private`"]

DataAnyValueQ[expr_] := If[AtomQ[expr],expr===DataAnyValue||expr===DataTie[DataAnyValue],Or@@(DataAnyValueQ/@expr)]

DataNoValueQ[expr_] := If[AtomQ[expr],expr===DataNoValue||expr===DataTie[DataNoValue],Or@@(DataNoValueQ/@expr)]

DataTie[d_DataTie] := d

DataTie[d_?NumberQ] := If[DataTieQ[d],d,-d-1]

DataTie[d_List] := DataTie /@ d

DataTieQ[d_] := MatchQ[d,_DataTie] || (NumberQ[d]&&d<0) || (!AtomQ[d]&&Or@@(DataTieQ/@d))

DataUnTie[d_DataTie] := d[[1]]

DataUnTie[d_?NumberQ] := If[DataTieQ[d],-d-1,d]

DataUnTie[d_List] := DataUnTie /@ d

DataUnTie[d_] := d

DeltasToValues[d_List, c_Integer:0] :=
  Module[{k = 0},
    Prepend[Table[k += d[[i]], {i, Length[d]}], 0] + c
    ]

FunctionQ[expr_] := MatchQ[expr, _Function | _CompiledFunction | _Composition | _InterpolatingFunction]

MNI[x$_, c$_, e$_, p$_, d$_] :=
  Module[{t$ = x$, r$},
    If[Length[t$] == 0,
      If[p$ == 0, d$, e$[[p$]]],
      t$[[0]] = If;
      r$ = t$[[1, 2]];
      t$[[1, 2]] = t$[[1, 1]];
      t$[[1, 1]] = c$;
      t$[[2]] = MNI[t$[[2]], c$, e$, p$, d$];
      t$[[3]] = MNI[t$[[3]], c$, e$, r$, d$];
      t$[[1, 0]] = Less;
      t$
      ]
    ]

MakeNestedIfs[de$:{{_,_}...}, default$_:0] := MakeNestedIfs[de$, default$, default$]
MakeNestedIfs[de$:{{_,_}...}, defaultLo$_, defaultHi$_] :=
  Module[{det$,deltas$={},expr$={},t$, i$, c$},
    If[0<Length[de$],
      det$=Transpose[de$];
      deltas$ = det$[[1]];
      expr$ = det$[[2]]
      ];
    t$ = MakeTree[DeltasToValues[deltas$]];
    i$ = MNI[t$, c$, Append[expr$, defaultHi$], 0, defaultLo$];
    Function[Evaluate[c$], Evaluate[i$]]
    ]

NormalizeList[d_,opts___] :=
  Module[
    {
      pr = PlayRange /. {opts},
      md,sp
      },
    If[pr === PlayRange, pr = {Min[d],Max[d]}];
    md = (pr[[1]]+pr[[2]])/2;
    sp = (pr[[2]]-pr[[1]])/2;
    N[(d-md)/sp]
    ]

ParOfSeqToSeqOfPar[pos:{{{_,_},{_,_}...},{{_,_},{_,_}...}...}] := (* melodies to chords *)
  Module[{at,ut,st,al,ad,sd},
    (* get all timing *)
    at = (#[[1]]& /@ #)& /@ pos;
    (* get the union of all timing, which will be the resulting timing *)
    ut = ValuesToDeltas[Sort[Union[Flatten[DeltasToValues /@ at]],OrderedQ[{N[#1], N[#2]}]&]];
    (* get all-timingt sliced *)
    st = (Module[{i=1,s=ut[[1]]},
      Split[ut,
        Function[{a,b},
          If[Length[#]<i || s+b<=#[[i]],
            s+=b; True,
            s=b; i++; False
            ]
          ]
        ]
      ]& /@ at);
    (* get the lengths *)
    al = (Length /@ #)& /@ st;
    (* get all data *)
    ad = (#[[2]]& /@ #)& /@ pos;
    sd = Transpose[
      MapThread[
        Function[{l,d},
          Flatten[
            MapThread[
              Function[{li,di},
                Prepend[Table[DataTie[di],{li-1}],di]
                ],
              {l,If[Length[d]<Length[l],Append[d,DataNoValue],d]}
              ],
              1
            ]
          ],
        {al,ad}
        ]
      ];
    Transpose[{ut,sd}]
    ]

SeqOfParToParOfSeq[sop:{{_,{__}},{_,{__}}...}] := (* chords to melodies *)
  Module[{v},
    Reap[
      Do[
        p = {0,DataNoValue};
        Scan[(
          v = If[i<=Length[#[[2]]],#[[2,i]],DataNoValue];
          If[DataTieQ[v],
            p[[1]] += #[[1]],
            If[0<p[[1]],Sow[p,i]];
            p={#[[1]],v}
            ]
          )&,
          sop
          ];
        If[0<p[[1]],Sow[p,i]],
        {i,Max[Length[#[[2]]]& /@ sop]}
        ]
      ][[2]]
    ]

UnCompile[f_CompiledFunction] := f[[5]]
UnCompile[f_Function] := f
UnCompile[f_Composition] := Function[x,Evaluate[f[x]]]
UnCompile[f_InterpolatingFunction] := f

ValuesToDeltas[v_List] := Drop[v, 1] - Drop[v, -1]

End[]

Protect[
  DataAnyValue,
  DataAnyValueQ,
  DataNoValue,
  DataNoValueQ,
  DataTie,
  DataTieQ,
  DataUnTie,
  DeltasToValues,
  FunctionQ,
  MakeNestedIfs,
  NormalizeList,
  ParOfSeqToSeqOfPar,
  SeqOfParToParOfSeq,
  ToDoString,
  UnCompile,
  ValuesToDeltas
  ];

EndPackage[]

