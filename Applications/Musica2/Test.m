(* :Title: Test *)

(* :Summary: Functions for Test *)

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

(* :Context: Musica2`Test` *)

(* :History:
  2005-01-15  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Test`"]

Unprotect[
  TestCase,
  TestCreate,
  TestSuite,
  TestRun
  ];

TestCase::usage = "todo"
TestCreate::usage = "todo"
TestSuite::usage = "todo"
TestRun::usage = "todo"

Begin["`Private`"]

SetAttributes[TestCase,HoldAll]
SetAttributes[TestCreate,HoldAll]

TestCreate[t_List] := (TestCreate @@ #)& /@ t
TestCreate[e_] := TestCase[e,Evaluate[e]]

TestRun[t_List,opts___?OptionQ] := And @@ (TestRun[#,opts]& /@ t)
TestRun[TestCase[t_List],opts___?OptionQ] := TestRun[t,opts]
TestRun[TestCase[e_,r_],opts___?OptionQ] :=
  If[e === r,
    If[Verbose /. {opts} /. {Verbose -> False},
      Print["OK: ",Unevaluated[e]," === ",r];
      ];
    True,
    Print["ERROR: ",Unevaluated[e]," === ",e," =!= ",r];
    False
    ]
TestRun[s_Symbol,opts___?OptionQ] := TestRun[TestSuite[s],opts]

End[]

Protect[
  TestCase,
  TestCreate,
  TestSuite,
  TestRun
  ];

EndPackage[]
