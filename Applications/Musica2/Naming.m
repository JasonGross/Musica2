(* :Title: Naming *)

(* :Summary: Functions for Naming *)

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

(* :Context: Musica2`Naming` *)

(* :History:
  2004-11-29  bch :  added use of Convert for getting ConversionFunctions
  2004-10-26  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Naming`",
  {
    "Musica2`Common`",
    "Musica2`Note`",
    "Musica2`ObjectType`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert
  ];

Unprotect[
  ChordNaming,
  ChordNamingFunction, (* to be removed *)
  FlatSymbols,
  Naming,
  NoteNaming,
  NoteNamingFunction, (* to be removed *)
  PrepareString,
  SharpSymbols
  ];

CreateElement[HelixNoteNaming,{
Octave_Integer,
NoteNames:{__String},
Helix:{PitchCodeStart_Integer,PitchCodeInterval_Integer,NameCodeStart_Integer},
FlatsCount_Integer
},Null,
"todo.\[NewLine]"
]

CreateElement[HelixChordNaming,{
RootNaming_,
ChordNames:{
  {{__String},{__Integer}}..
  }
},Null,
"todo.\[NewLine]"
]

ChordNames::usage = "ChordNames is a member of HelixChordNaming."
ChordNaming::usage = "todo"
FlatSymbols::usage = "todo"
Naming::usage = "todo"
NoteNaming::usage = "todo"
PrepareString::usage = "todo"
SharpSymbols::usage = "todo"

Begin["`Private`"]

FlatSymbols={{"\[Flat]","b"}};

Convert[{PitchCode},String] := Convert[{PitchCode},String,ChordNaming]
Convert[String,{PitchCode}] := Convert[String,{PitchCode},ChordNaming]
Convert[PitchCode,String] := Convert[PitchCode,String,NoteNaming]
Convert[String,PitchCode] := Convert[String,PitchCode,NoteNaming]

Convert[{PitchCode},String,x_HelixChordNaming] := ChordNamingFunction[x, False]
Convert[String,{PitchCode},x_HelixChordNaming] := ChordNamingFunction[x, True]
Convert[PitchCode,String,x_HelixNoteNaming] := NoteNamingFunction[x, False]
Convert[String,PitchCode,x_HelixNoteNaming] := NoteNamingFunction[x, True]

HelixChordNaming[] := HelixChordNaming[{NoteNaming, {
  {{"m-5"},      {0, 3, 6}},
  {{"sus2"},     {0, 2, 7}},
  {{"m", "min"}, {0, 3, 7}},
  {{"", "maj"},  {0, 4, 7}},
  {{"sus4"},     {0, 5, 7}},
  {{"+5"},       {0, 4, 8}},

  {{"dim"},  {0, 3, 6,  9}},
  {{"m7-5"}, {0, 3, 6, 10}},
  {{"m7"},   {0, 3, 7, 10}},
  {{"7"},    {0, 4, 7, 10}},
  {{"mMaj7"},{0, 3, 7, 11}},
  {{"Maj7"}, {0, 4, 7, 11}}
  }}]
HelixChordNaming[o_?OptionQ, d_?(DataQ[HelixChordNaming])][p:{__Integer}] := ChordNamingFunction[HelixChordNaming[o,d]][p,0]
HelixChordNaming[o_?OptionQ, d_?(DataQ[HelixChordNaming])][p:{__Integer},ks_Integer] := ChordNamingFunction[HelixChordNaming[o,d]][p,ks]
HelixChordNaming[o_?OptionQ, d_?(DataQ[HelixChordNaming])][s_String] := ChordNamingFunction[HelixChordNaming[o,d],True][s]

ChordNamingFunction[x_HelixChordNaming, False] :=
  Module[{
      pcc = Octave[RootNaming[x]],
      cn = ChordNames[x],
      rnf = NoteNamingFunction[RootNaming[x], False, False],
      cnf
      },
    cn = {#[[1, 1]], Data[FigBass[#[[2]], Octave -> pcc]][[2]]} & /@ cn;
    cnf = Function[{pcs, ks, ab},
      Module[{d, r, b, n, bn},
        d = Data[FigBass[pcs, Octave -> pcc]][[2]];
        d[[1]] = d[[1,1]];
        (*Print["d: ",d];*)
        b = Mod[Sort[pcs][[1]], pcc];
        (*Print["b: ",b];*)
        bn = Drop[rnf[b, ks], -1];
        bn = ToUpperCase[bn[[1]]] <> bn[[2]];
        (*Print["bn: ",bn];*)
        r = Select[cn, MatchQ[#, {_, {_, d[[2]]}}] &];
        (*Print["r: ",r];*)
        r = Function[ri,
          (*Print["ri: ",ri];*)
          Module[{t=(d[[1]] - #)& /@ ri[[2, 1]]},
            {(ToUpperCase[#[[1]]]<>#[[2]])& /@ (Take[rnf[#,ks],2]& /@ t), ri[[1]], t}
            ]
          ] /@ r;
        (*Print["r: ",r];*)
        r = Function[ri,
          MapThread[
            (#1<>ri[[2]]<>If[ab && (Mod[#2,pcc] != b),"/"<>bn,""])&,
            {ri[[1]],ri[[3]]}
            ]
          ] /@ r;
        Flatten[r]
        ]
      ];
    Function[{pcs, ks},
      Module[{pcsm = Union[Mod[pcs, pcc]], r = cnf[pcs, ks, True]},
        If[r != {},
          r,
          r = {cnf[Delete[pcsm, #], ks, False], pcsm[[#]]}& /@ Range[Length[pcsm]];
          r = Select[r, (#[[1]] != {}) &];
          r = Module[
            {b = (ToUpperCase[#[[1]]] <> #[[2]]) &[rnf[#[[2]], ks]]},
            (# <> "/" <> b) & /@ #[[1]]
            ]& /@ r;
          Flatten[r]
          ]
        ]
      ]
    ]

ChordNamingFunction[x_HelixChordNaming, True] :=
  Module[{
      pcc = Octave[RootNaming[x]],
      cn = ChordNames[x],
      rnf = NoteNamingFunction[RootNaming[x], True]
      },
    Function[s,
      Module[{q = StringPosition[s, "/"], cs, bs, cp, bp, r, b, mp, m},
        (* do we have a bass != root? *)
        If[Length[q] == 1,
          cs = StringTake[s, q[[1, 1]] - 1];
          bs = StringDrop[s, q[[1, 2]]],
          cs = s;
          bs = s
          ];
        cp = PrepareString[cs];
        bp = PrepareString[bs];
        r = Mod[rnf[cp[[1]]] + cp[[2]], pcc];
        b = Mod[rnf[bp[[1]]] + bp[[2]], pcc];
        mp = Position[cn, cp[[3]]][[1, 1]];
        m = Union[cn[[mp, 2]] + r, {b}];
        Sort[Mod[m, pcc, b]]
        ]
      ]
    ]

ChordNaming := HelixChordNaming[]
ChordNamingFunction[x_] := ChordNamingFunction[x, False]

HelixNoteNaming[] := HelixNoteNaming[{12,{"c","d","e","f","g","a","b"},{5,5,3},2}]
HelixNoteNaming[o_?OptionQ, d_?(DataQ[HelixNoteNaming])][p_Integer] := NoteNamingFunction[HelixNoteNaming[o,d]][p,0]
HelixNoteNaming[o_?OptionQ, d_?(DataQ[HelixNoteNaming])][p_Integer,ks_Integer] := NoteNamingFunction[HelixNoteNaming[o,d]][p,ks]
HelixNoteNaming[o_?OptionQ, d_?(DataQ[HelixNoteNaming])][s_String] := NoteNamingFunction[HelixNoteNaming[o,d],True][s]

NoteNamingFunction[x_HelixNoteNaming,False,False] :=
  Module[{i,pcc=Octave[x],pn2s=NoteNames[x],h2pc,pc2h,pnc,fc=FlatsCount[x],pn2pc,h2pn},
    h2pc=Mod[Table[PitchCodeStart[x]-i PitchCodeInterval[x],{i,0,pcc-1}],pcc];
    pc2h=Table[Position[h2pc,i][[1,1]]-1,{i,0,pcc-1}];
    pnc=Length[pn2s];
    pn2pc=Sort[Take[h2pc,pnc]];
    h2pn=Table[Position[pn2pc,h2pc[[i]]][[1,1]]-1,{i,pnc}];
    Function[{pc,ks},
      Module[{o,h,pn,a},
        If[pcc==pnc,
          pn = Mod[pc,pcc];
          a = 0;
          o = Floor[pc/pcc],
          h = Mod[pc2h[[Mod[pc,pcc]+1]],pcc,ks-fc];
          pn = h2pn[[Mod[h,pnc]+1]];
          a = Floor[h/pnc];
          o = (pc-h2pc[[Mod[h,pnc]+1]]-a)/pcc;
          ];
        {pn2s[[pn+1]] , If[a==0,"",Table[If[a<0,FlatSymbols[[1,1]],SharpSymbols[[1,1]]],{Abs[a]}]] , ToString[o]}
        ]
      ]
    ]
NoteNamingFunction[x_HelixNoteNaming,False] :=
  Module[{nnf=NoteNamingFunction[x,False,False]},
    Function[{pc,ks},
      StringJoin[nnf[pc,ks]]
      ]
    ]
NoteNamingFunction[x_HelixNoteNaming,True] :=
  Module[{i,pcc=Octave[x],pn2s=NoteNames[x],h2pc,pn2pc},
    h2pc=Mod[Table[PitchCodeStart[x]-i PitchCodeInterval[x],{i,0,pcc-1}],pcc];
    pnc=Length[pn2s];
    pn2pc=Sort[Take[h2pc,pnc]];
    Function[str,
      Module[{s,a,r,o,pn},
        {s,a,r} = PrepareString[str,FlatSymbols,SharpSymbols];
        pn = Position[pn2s,ToLowerCase[s]][[1,1]]-1;
        o = ToExpression[r];
        pn2pc[[pn+1]] + a + pcc If[o===Null,0,o]
        ]
      ]
    ]

NoteNaming := HelixNoteNaming[]
NoteNamingFunction[x_] := NoteNamingFunction[x, False]

Naming[x_Chord] := Naming[x,0]
Naming[x_Chord,ks_] := ChordNamingFunction[ChordNaming, False][PitchCode[x],ks]
Naming[x_String,Chord] := Chord[ChordNamingFunction[ChordNaming, True][x]]

Naming[x_Note] := Naming[x,0]
Naming[x_Note,ks_] := NoteNamingFunction[NoteNaming, False][PitchCode[x],ks]
Naming[x_String,Note] := Note[NoteNamingFunction[NoteNaming, True][x]]

PrepareString[str_String,flats:{{__String}...},sharps:{{__String}...}]:=
  Module[{r=str,n,a=0,s={},f={}},
    n=StringTake[r,1];
    r=StringDrop[r,1];
    While[
      0<StringLength[r] &&
        (Length[f=Position[flats,StringTake[r,1]]]!=0||
         Length[s=Position[sharps,StringTake[r,1]]]!=0),
      (a-=#[[1]])& /@ f;
      (a+=#[[1]])& /@ s;
      r=StringDrop[r,1];
      s=f={};
      ];
    {n,a,r}(*{string,integer,string}*)
    ]
PrepareString[str_String] := PrepareString[str,FlatSymbols,SharpSymbols]

SharpSymbols={{"\[Sharp]","#"},{"x"}};

End[]

Protect[
  Convert
  ];

Protect[
  ChordNaming,
  ChordNamingFunction,
  FlatSymbols,
  Naming,
  NoteNaming,
  NoteNamingFunction,
  PrepareString,
  SharpSymbols
  ];

EndPackage[]
