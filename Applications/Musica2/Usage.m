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

Usage::usage = "Evaluate Usage[App_Symbol,s__Symbol] for more info on usage of s."

Begin["`Private`"]

Usage[app_Symbol,s_String] := "Evaluate Usage["<>SymbolName[app]<>","<>s<>"] for more info on usage of "<>s<>"."

Usage /: ToString[Usage,u_] :=
  (ToString[u[[1]]] <>
    "[" <>
    StringTake[ToString[u[[2]]], {2, -2}] <>
    "] -> " <>
    ToString[u[[3]]] <>
    " , " <>
    u[[4]]
    )

Usage[app_Symbol,s__Symbol] :=
  Module[{p = ((Symbol[#] -> List)& /@ Join[Names["Blank*"], Names["Pattern*"]]), t},
    t=Select[
      Usage[app],
      (
        t = #;
        t = Drop[t,-1] /. p;
        t = Flatten[t];
        And @@ (MemberQ[t, #]& /@ {s})
        )&
      ];
    If[t=!={},
      t = ToString[Usage,#]& /@ t;
      StringJoin[(# <> "\n") & /@ Drop[t, -1]] <> t[[-1]],
      "Sorry, no Usage-text found for symbols "<>ToString[{s}]<>"."
      ]
    ]

Usage[app_Symbol, n_, p_, r_, u_String] :=
  (
    If[!MemberQ[Usage[app],{n,Verbatim[p],_,_}],
      Usage[app] = Sort[Append[Usage[app],{n, p, r, u}]]
      ];
    )

End[]

Protect[
  ];

EndPackage[]
