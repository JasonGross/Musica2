(* :Title: Setup *)

(* :Summary: Functions for Setup, You should not need them really *)

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

(* :Context: Musica2`Setup` *)

(* :History:
  2004-09-03  bch :  added Note and Common to the list of pkg's
  2004-08-27  bch :  added ToDo function
  2004-08-26  bch :  added some help/usage-text
  2004-08-19  bch :  added MidiPlay to the list of pkg's
  2004-08-08  bch :  added CalcMidiStateRoutes
  2004-08-07  bch :  created
                     added Setup to the list of pkg's
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Setup`",
  {
    "DiscreteMath`Combinatorica`",
    "Musica2`Midi`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  CalcMidiStateRoutes,
  ClearInitDotEm,
  MakeInitDotEm,
  ToDo
  ];

CalcMidiStateRoutes::usage = "CalcMidiStateRoutes[m_Midi, opts___] takes a Midi-object as an argument and recalculates MidiStateRoutes. The option Verbose->True plots a graph along with some timings. The result returned by CalcMidiStateRoutes is the average time it takes for a change of state for the Midi-object."
ClearInitDotEm::usage = "MakeInitDotEm[pkg_, pkgs_, fn_] rewrites the file init.m."
MakeInitDotEm::usage = "MakeInitDotEm[pkg_, pkgs_, fn_] rewrites the file init.m."
ToDo::usage = "ToDo[] prints what todo ;-)"

Begin["`Private`"]

SW[e_, m$_,pr_] :=
  Module[{r = e, p, q, w, t},
    p = Position[MidiStatesExpanded, MidiGetState[m$]][[1, 1]];
    w = Position[e, #][[1, 1]] & /@ Cases[e, {{p, _}, \[Infinity]}];
    Scan[If[r[[#, 2]] == \[Infinity],
      q = r[[#, 1, 2]];
      t = Timing[MidiSetStateLow[m$, MidiStatesExpanded[[q]]]];
      r[[#, 2]] = t[[1, 1]];
      If[pr,Print[r[[#]]]];
      r = SW[r, t[[2]],pr];
      ] &, w];
    r
    ]

COP[e_, p_] :=
  Module[{pe = Transpose[{Drop[p, -1], Drop[p, 1]}]},
    Total[Cases[e, {s$_, c$_} /; MemberQ[pe, s$] -> c$]]
    ]

CalcMidiStateRoutes[m_Midi, opts___] :=
  Module[{e, g,c,p,sc,pr=Verbose/.{opts}/.{Verbose->False}},
    Unprotect[MidiStateRoutes];
    e = {Position[MidiStatesExpanded, #][[1, 1]] & /@ #, \[Infinity]} & /@ MidiStatePathsExpanded;
    e = SW[e, m, pr];
    If[MemberQ[e, {{_, _}, \[Infinity]}],
      Print["not connected?"];
      Print[ColumnForm[Cases[e, {{_, _}, \[Infinity]}]]];
      ];
    g = AddEdges[EmptyGraph[Length[MidiStatesExpanded], Type -> Directed], ReplacePart[#, EdgeWeight -> #[[2]], {2}] & /@ e];
    If[pr,ShowGraph[SetVertexLabels[g,StringJoin[(StringTake[ToString[#[[2]]], {5}]) & /@ #] & /@ MidiStatesExpanded], VertexNumber -> True]];
    sc=Length[MidiStatesExpanded];
    c=0;
    MidiStateRoutes = Table[
      p=ShortestPath[g, f, t];
      If[1<Length[p],c+=COP[e,p]];
      p,
      {f, sc},{t, sc}
      ];
    Protect[MidiStateRoutes];
    c/(#^2-#)&[sc]
    ]

fn="Musica2/Applications/Musica2/Kernel/init.m";
pkg="Musica2";
pkgs={"Common","EventList","Midi","MidiPlay","Note","Setup","Sound","Utils"};

ClearInitDotEm[] := ClearInitDotEm[pkg, fn]
ClearInitDotEm[pkg_, fn_] := MakeInitDotEm[pkg,{},fn]

MakeInitDotEm[] := MakeInitDotEm[pkg, pkgs, fn]
MakeInitDotEm[pkg_, pkgs_, fn_] :=
  Module[{fout = OpenWrite[fn],d=ToString/@Date[],pr=True},
    WriteString[fout,"(* :Title: Master Declarations File for " <> pkg <> " *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.\nWhen loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout,"(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "(* :History: File created "<>d[[1]]<>"-"<>d[[2]]<>"-"<>d[[3]]<>" at "<>d[[4]]<>":"<>d[[5]]<>" *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "If[!MemberQ[$Packages,\"" <> pkg <> "`\"],\n  System`Private`p = Unprotect[$Packages];\n  PrependTo[$Packages,\"" <> pkg <> "`\"];\n  Protect @@ System`Private`p  \n];\n"];
    WriteString[fout, "\n"];
    (
      If[pr,Print[#]];
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

ToDo[] := ToDo[pkg, pkgs]
ToDo[pkg_, pkgs_] :=
  Drop[
    Union[
      Flatten[
        Module[{e = ToExpression[#], s},
          If[Head[e] === Symbol,
            s = MessageName[Evaluate[e], "todo"];
            If[StringQ[s], {#, s},Null],
            Null
            ]
          ]& /@ Names[pkg <> "`" <> # <> "`*"] & /@ pkgs,
        1
        ]
      ],
    1
    ]

End[]

Protect[
  CalcMidiStateRoutes,
  ClearInitDotEm,
  MakeInitDotEm,
  ToDo
  ];

EndPackage[]
