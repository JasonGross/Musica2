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
  Func1Normalize,
  Func1ToList,
  FunctionQ,
  ListToFunc1,
  MakeInitDotEm,
  MakeNestedIfs,
  ReArg1,
  UnCompile,
  ValuesToDeltas
  ];

DeltasToValues::usage = "DeltasToValues[d_, c_:0]";
Func1Q::usage = "Func1Q[f_?FunctionQ]"
Func1ListQ::usage = ""
Func1Normalize::usage = "Func1Normalize[f_?Func1Q]"
Func1ToList::usage = "Func1ToList[f_, sr_, duration_]"
FunctionQ::usage = ""
ListToFunc1::usage = "ListToFunc1[d_, sr_, smooth_:False]"
MakeInitDotEm::usage = "MakeInitDotEm[pkg_, pkgs_, fn_]";
MakeNestedIfs::usage = "MakeNestedIfs[deltas$_, expr$_, default$_]"
ReArg1::usage = "ReArg1[f_?Func1Q, p_, a_]"
UnCompile::usage = "UnCompile[f_?FunctionQ]"
ValuesToDeltas::usage = "ValuesToDeltas[t_]";

Begin["`Private`"]

DeltasToValues[d_List, c_Integer:0] :=
  Module[{k = 0},
    Prepend[Table[k += d[[i]], {i, Length[d]}], 0] + c
    ]

SlotQ[expr_] := !AtomQ[expr] && Length[expr]==1 && expr[[0]]===Slot && IntegerQ[expr[[1]]] && 0<expr[[1]]
GetSlotNrs[f_]:=If[FunctionQ[f]||AtomQ[f],{},If[SlotQ[f],f[[1]],GetSlotNrs/@ReplacePart[f,List,{0}]]]

Func1Q[f_?FunctionQ]:=
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

FunctionQ[expr_] := MatchQ[expr, _Function | _CompiledFunction]

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

MakeInitDotEm[pkg_:"Musica2", pkgs_:{"Midi","Sound","Utils"}, fn_:"Musica2/Applications/Musica2/Kernel/init.m"] :=
  Module[{fout = OpenWrite[fn],d=ToString/@Date[]},
    WriteString[fout,"(* :Title: Master Declarations File for " <> pkg <> " *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.\nWhen loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout,"(* :Author: This file was created by the function Musica2`Utils`MakeInitDotEm[], written by Bo C. Herlin *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "(* :History: File created "<>d[[1]]<>"-"<>d[[2]]<>"-"<>d[[3]]<>" at "<>d[[4]]<>":"<>d[[5]]<>" *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "If[!MemberQ[$Packages,\"" <> pkg <> "`\"],\n  System`Private`p = Unprotect[$Packages];\n  PrependTo[$Packages,\"" <> pkg <> "`\"];\n  Protect @@ System`Private`p  \n];\n"];
    WriteString[fout, "\n"];
    (
      Print[#];
      Get[pkg <> "`" <> # <> "`"];
      n = Names[pkg <> "`" <> # <> "`*"];
      WriteString[fout,"DeclarePackage[\"" <> pkg <> "`" <> # <> "`\",\n"];
      Write[fout, n];
      WriteString[fout, "];\n"];
      WriteString[fout, "\n"];
      ) & /@ pkgs;
    WriteString[fout, "Null\n"];
    Close[fout];
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

Func1Normalize[f_?Func1Q] :=
  Module[{g = UnCompile[f]},
    If[Length[g] == 1, Function[x, Evaluate[g[x]]],
      If[! AtomQ[g[[1]]] && Length[g[[1]]] == 1,
        ReplacePart[g, g[[1, 1]], 1],
        g
        ]
      ]
    ]

ReArg1[f_?Func1Q, p_, a_] :=
  Module[{g = Func1Normalize[f]},
    g = ReplacePart[g, g[[2]] /. {g[[1]] -> (a)}, 2];
    ReplacePart[g, p, 1]
    ]

UnCompile[f_CompiledFunction] := f[[5]]
UnCompile[f_Function] := f

ValuesToDeltas[v_List] := Drop[v, 1] - Drop[v, -1]

End[]

Protect[
  DeltasToValues,
  Func1Q,
  Func1ListQ,
  Func1Normalize,
  Func1ToList,
  FunctionQ,
  ListToFunc1,
  MakeInitDotEm,
  MakeNestedIfs,
  ReArg1,
  UnCompile,
  ValuesToDeltas
  ];

EndPackage[]

