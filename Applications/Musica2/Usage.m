(* :Title: Usage *)

(* :Summary: Functions for Usage *)

(* :Author: Bo C. Herlin *)

(* :Licence: GPL

Copyright (C) 2005  Bo C. Herlin

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

(* :Context: Musica2`Usage` *)

(* :History:
  2005-02-17  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Usage`"]

Unprotect[
  ];

Usage::usage = "Evaluate Usage[Usage,Usage] for more info on usage of Usage."

Begin["`Private`"]

Usage[List] = {};

Usage[Append,app_Symbol, n_Symbol, p_, r_, u_String] :=
  Module[{dup},
    (* initialize *)
    If[!MatchQ[Usage[List,app],_List],
      Usage[List,app] = {};
      Usage[List] = Append[Usage[List],app];
      ];
    (* delete duplicates *)
    dup = Position[Usage[List,app],{n,Verbatim[p],_,_}];
    If[dup =!= {},
      Usage[List,app] = Delete[Usage[List,app],dup[[1,1]]];
      ];
    (* append the usage-text *)
    Usage[List,app] = Append[Usage[List,app],{n, p, r, u}];
    ]

Usage[Append,Usage,Usage,{List},{___Symbol},"The list of Usage-text-domains."]
Usage[Append,Usage,Usage,{Append,_Symbol, _Symbol, _, _, _String},Null,"Append a new function-description to the Usage-text (and delete any old)."]

Usage[Append,Usage,Usage,{Default,_Symbol,_String},Null,"Get the default ::usage-text."]
Usage[Default,app_Symbol,s_String] := "Evaluate Usage["<>ToString[app]<>","<>s<>"] for more info on usage of "<>s<>"."

ts[u_] :=
  (ToString[u[[1]]] <>
    If[!ListQ[u[[2]]],"",(*will this ever happen?*)
      "[" <>
      StringTake[ToString[u[[2]]], {2, -2}] <>
      "]"
      ] <>
    " -> " <>
    ToString[u[[3]]] <>
    " , " <>
    u[[4]]
    )

Usage[Append,Usage,Usage,{_Symbol,__},_String,"Search a Usage-text-domain."]
Usage[app_Symbol,s__] :=
  Module[{t},
    t=Select[
      Usage[List,app],
      (
        t=#;
        And @@ (MemberQ[t, #, Infinity]& /@ {s})
        )&
      ];
    t=ReplacePart[t,List,0];
    If[t=!={},
      t = Sort[t];
      t = ts /@ t;
      StringJoin[(# <> "\n") & /@ Drop[t, -1]] <> t[[-1]],
      "Sorry, no Usage-text found for "<>ToString[{s}]<>"."
      ]
    ]/;(
      app =!= Default &&
      app =!= Append &&
      app =!= List &&
      app =!= All
      )

Usage[Append,Usage,Usage,{{__Symbol},__},_String,"Search a list of Usage-text-domain's."]
Usage[app:{__Symbol},s__] := StringDrop[StringJoin[(ToString[#] <> ":\n\n" <> Usage[#,s] <> "\n\n")& /@ app],-2]

Usage[Append,Usage,Usage,{All,__},_String,"Search all Usage-text-domain's."]
Usage[All,s__] := Usage[Usage[List],s]

End[]

Protect[
  ];

EndPackage[]
