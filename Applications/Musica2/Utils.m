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
  DeltasToValues,
  Func1Q,
  Func1ListQ,
  Func1ToList,
  FunctionQ,
  ListToFunc1,
  MakeNestedIfs,
  UnCompile,
  ValuesToDeltas
  ];

DeltasToValues::usage = "DeltasToValues[d_, c_:0]";
Func1Q::usage = "Func1Q[f_?FunctionQ]"
Func1ListQ::usage = ""
Func1ToList::usage = "Func1ToList[f_, sr_, duration_]"
FunctionQ::usage = ""
ListToFunc1::usage = "ListToFunc1[d_, sr_, smooth_:False]"
MakeNestedIfs::usage = "MakeNestedIfs[deltas$_, expr$_, default$_]"
UnCompile::usage = "UnCompile[f_?FunctionQ]"
ValuesToDeltas::usage = "ValuesToDeltas[t_]";

Begin["`Private`"]

DeltasToValues[d_List, c_Integer:0] :=
  Module[{k = 0},
    Prepend[Table[k += d[[i]], {i, Length[d]}], 0] + c
    ]

SlotQ[expr_] := !AtomQ[expr] && Length[expr]==1 && expr[[0]]===Slot && IntegerQ[expr[[1]]] && 0<expr[[1]]
GetSlotNrs[f_]:=If[FunctionQ[f]||AtomQ[f],{},If[SlotQ[f],f[[1]],GetSlotNrs/@ReplacePart[f,List,{0}]]]

Func1Q[f_?FunctionQ]:= (* is this really working? *)
  Module[{g=UnCompile[f]},
    g[[0]]=List;
    If[Length[g]==2,
      AtomQ[g[[1]]] || Length[g[[1]]]==1,
      Max[Union[Flatten[GetSlotNrs[g]]]]<=1
      ]
    ]

Func1ListQ[expr_] := ListQ[expr] && And @@ Func1Q /@ expr

Func1ToList[f_, sr_, sd_] :=
  Module[{sc},
    sc = sr*sd;
    Table[f[i/sr], {i, 0, sc - 1}]
    ]

FunctionQ[expr_] := MatchQ[expr, _Function | _CompiledFunction | _Composition]

ListToFunc1[d_, sr_, sm_:False] :=
  Module[{sc = Length[d], sf = (#1 + (#2 - #1)#3) &},
    If[sm,
      Compile[{{t, _Real}},
        sf[
          d[[1 + Floor[Mod[t*sr, sc]]]],
          d[[1 + Floor[Mod[1 + t*sr, sc]]]],
          FractionalPart[t*sr]
          ]
        ],
      Compile[{{t, _Real}},
        d[[1 + Floor[Mod[t*sr, sc]]]]
        ]
      ]
    ]

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

MakeNestedIfs[de$:{{_,_}...}, default$_] := MakeNestedIfs[de$, default$, default$]
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

UnCompile[f_CompiledFunction] := f[[5]]
UnCompile[f_Function] := f
UnCompile[f_Composition] := Function[x,Evaluate[f[x]]]

ValuesToDeltas[v_List] := Drop[v, 1] - Drop[v, -1]

End[]

Protect[
  DeltasToValues,
  Func1Q,
  Func1ListQ,
  Func1ToList,
  FunctionQ,
  ListToFunc1,
  MakeNestedIfs,
  UnCompile,
  ValuesToDeltas
  ];

EndPackage[]

