(* :Title: Sound *)

(* :Summary: Functions for Sound *)

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

(* :Context: Musica2`Sound` *)

(* :History:
  2004-08-11  bch :  added SoundSetDuration and some SoundMake*[*]
                     moved FuncToList & ListToFunc from Utils.m to Sound.m
                     added RDC's
  2004-08-10  bch :  added SoundPitchShift
                     added opts to many make-functions
                     removed SoundSmooth in favor of InterpolationOrder
                     removed all Func1 things
  2004-08-08  bch :  just found out about Composition, which has now replaced ReArg1
  2004-08-04  bch :  first release
  2004-08-02  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Sound`",
  {
    "Musica2`Utils`"
    }
  ]

Unprotect[
  FuncToList,
  ListToFunc,
  SoundChannelCount,
  SoundDuration,
  SoundFuncQ,
  SoundGetChannelCount,
  SoundGetDuration,
  SoundGetFunc,
  SoundGetInfo,
  SoundGetList,
  SoundGetSampleCount,
  SoundGetSampleRate,
  SoundImportWav,
  SoundListQ,
  SoundLoop,
  SoundMakeFunc,
  SoundMakeList,
  SoundMix,
  SoundOfSilence,
  SoundPar,
  SoundPitchShift,
  SoundSampleCount,
  SoundSetDuration,
  SoundSeq,
  SoundType,
  SoundUnPar,
  SoundUnSeq,
  Zound
  ];

FuncToList::usage = "FuncToList[f_, opts___]"
ListToFunc::usage = "ListToFunc[d_, opts___]"
SoundChannelCount::usage = ""
SoundDuration::usage = ""
SoundFuncQ::usage = "SoundFuncQ[expr_]"
SoundGetChannelCount::usage = "SoundGetChannelCount[s_Sound]"
SoundGetDuration::usage = "SoundGetDuration[s_Sound]"
SoundGetFunc::usage = "SoundGetFunc[s_?SoundFuncQ, opts___]"
SoundGetInfo::usage = "SoundGetInfo[s_Sound]"
SoundGetList::usage = "SoundGetList[s_?SoundListQ]"
SoundGetSampleCount::usage = "SoundGetSampleCount[s_Sound]"
SoundGetSampleRate::usage = "SoundGetSampleRate[s_Sound]"
SoundImportWav::usage = "SoundImportWav[fn_String]"
SoundListQ::usage = "SoundListQ[expr_]"
SoundLoop::usage = ""
SoundMakeFunc::usage = "SoundMakeFunc[fl_, opts___]"
SoundMakeList::usage = "SoundMakeList[ll_, opts___]"
SoundMix::usage = "SoundMix[s_Sound, mix : {{_?FunctionQ, _?FunctionQ ...}, {_?FunctionQ, _?FunctionQ ...} ...}, opts___]"
SoundOfSilence::usage = "SoundOfSilence[opts___]"
SoundPar::usage = "SoundPar[sl:SFLP, opts___]"
SoundPitchShift::usage = ""
SoundSetDuration::usage = "SoundSetDuration[s_Sound,sd_,opts___]"
SoundSampleCount::usage = ""
SoundSeq::usage = "SoundSeq[sl:SFLP, opts___]"
SoundType::usage = ""
SoundUnPar::usage = "SoundUnPar[s_?SoundFuncQ]"
SoundUnSeq::usage = "SoundUnSeq[s_?SoundFuncQ, c_List]"
Zound::usage = ""

Begin["`Private`"]

RDCX[rr_,dd_,cc_,opts___] :=
  Module[
    {
      r = SampleRate /. {opts},
      d = SoundDuration /. {opts},
      c = SoundSampleCount /. {opts}
      },
    If[MatchQ[{r, d, c}, {SampleRate, SoundDuration, SoundSampleCount}],
      r = rr;
      d = dd;
      c = cc;
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {_, SoundDuration, SoundSampleCount}],
      d = dd;
      c = IntegerPart[r*d];
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {SampleRate, _, SoundSampleCount}],
      r = rr;
      c = IntegerPart[r*d];
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {SampleRate, SoundDuration, _}],
      r = rr;
      d = N[c/r];
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {SampleRate, _, _}],
      r = IntegerPart[c/d];
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {_, SoundDuration, _}],
      d = N[c/r];
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {_, _, SoundSampleCount}],
      c = IntegerPart[r*d];
      Return[{r,d,c}];
      ];
    If[MatchQ[{r, d, c}, {_, _, _}],
      d = N[c/r];
      Return[{r,d,c}];
      ];
    {r,d,c}
    ]

RDCZ[opts___] := RDCX[
  SampleRate /. Options[Zound],
  SoundDuration /. Options[Zound],
  IntegerPart[(SampleRate /. Options[Zound]) * (SoundDuration /. Options[Zound])],
  opts
  ]

RDCS[s_Sound, opts___] := RDCX[
  SoundGetSampleRate[s],
  SoundGetDuration[s],
  SoundGetSampleCount[s],
  opts
  ]

RDC2O[rdc_] := {SampleRate->#1, SoundDuration->#2, SoundSampleCount->#3}& @@ rdc

FuncToList[f_, opts___] :=
  Module[
    {sr,sd,sc},
    {sr,sd,sc}=RDCZ[opts];
    Table[N[f[i/sr]], {i, 0, sc - 1}]
    ]

ListToFunc[d_, opts___] :=
  Module[
    {
      sr,sd,sc,
      f,
      io = InterpolationOrder /. {opts} /. {InterpolationOrder->0}, (* is this wise? *)
      p = NormalizeList[d,opts]
      },
    {sr,sd,sc}=RDCZ[SoundSampleCount->Length[d],opts];
    f = ListInterpolation[Append[p,p[[-1]]],InterpolationOrder->io];
    Function[t,Evaluate[f[1 + Mod[t*sr, sc]]]]
    ]

FP = _?FunctionQ
FLP = {FP,FP...}

SFP = _?SoundFuncQ
SFLP = {SFP,SFP...}

LP = {_Integer | _Real, (_Integer | _Real) ...}
LLP = {LP,LP...}

SLP = _?SoundListQ
SLLP = {SLP,SLP...}

SoundFuncQ[expr_] := MatchQ[expr, Sound[SampledSoundFunction[_, _, _]]]

SoundGetChannelCount[s_?SoundFuncQ] := If[ListQ[s[[1, 1]]],Length[s[[1, 1]]],1]

SoundGetChannelCount[s_?SoundListQ] := If[Depth[s[[1, 1]]] == 3, Length[s[[1, 1]]], 1]

SoundGetDuration[s_Sound] := N[SoundGetSampleCount[s]/SoundGetSampleRate[s]]

SoundGetFunc[s_?SoundFuncQ, opts___] :=
  (Module[
    {
      sr,sd,sc,
      cfl = If[ListQ[s[[1, 1]]], s[[1, 1]], {s[[1, 1]]}]
      },
      {sr,sd,sc}=RDCZ[SampleRate->SoundGetSampleRate[s],SoundSampleCount->SoundGetSampleCount[s],opts];
      If[SoundLoop /. {opts} /. Options[Zound],
        Composition[UnCompile[#],(1 + Mod[#*sr, sc])&]& /@ cfl,
        Composition[UnCompile[#],(1 + #*sr)&]& /@ cfl
        ]
    ])

SoundGetFunc[s_?SoundListQ, opts___] :=
  (Module[
    {
      sr,sd,sc,
      fl = SoundGetList[s]
      },
    {sr,sd,sc}=RDCZ[SampleRate->SoundGetSampleRate[s],SoundSampleCount->SoundGetSampleCount[s],opts];
    ListToFunc[#, SampleRate->sr, opts]& /@ fl
    ])

SoundGetInfo[s_Sound] :=
  Module[
    {
      sr,sd,sc,
      scc = SoundGetChannelCount[s],
      st = If[SoundFuncQ[s],SampledSoundFunction,SampledSoundList]
      },
    {sr,sd,sc}=RDCS[s];
    {SoundSampleCount->sc, SampleRate->sr, SoundDuration->sd, SoundChannelCount->scc, SoundType->st}
    ]

SoundGetList[s_?SoundFuncQ, opts___] :=
  Module[{
      sr,sd,sc,
      fl = SoundGetFunc[s]
      },
    {sr,sd,sc}=RDCZ[SampleRate->SoundGetSampleRate[s],SoundSampleCount->SoundGetSampleCount[s],opts];
    FuncToList[#, SampleRate->sr, SoundDuration->sd, opts]& /@ fl
    ]

SoundGetList[s_?SoundListQ, opts___] := If[Depth[s[[1, 1]]] == 3, s[[1, 1]], {s[[1, 1]]}]

SoundGetSampleCount[s_?SoundFuncQ] := s[[1, 2]]

SoundGetSampleCount[s_?SoundListQ] := If[Depth[s[[1, 1]]] == 3, Max[Length /@ s[[1, 1]]], Length[s[[1, 1]]]]

SoundGetSampleRate[s_?SoundFuncQ] := s[[1, 3]]

SoundGetSampleRate[s_?SoundListQ] := s[[1, 2]]

SoundImportWav[fn_String] := Import[fn,"WAV"]

SoundListQ[expr_] := MatchQ[expr, Sound[SampledSoundList[_, _]]]

SoundMakeFunc[p:FP, opts___] := SoundMakeFunc[{p}, opts]

SoundMakeFunc[p:FLP, opts___] :=
  Module[
    {
      sr,sd,sc
      },
    {sr,sd,sc}=RDCZ[opts];
    Sound[
      SampledSoundFunction[
        Evaluate[Compile[{{n,_Integer}},Evaluate[UnCompile[#][(n - 1.0)/sr]]]& /@ p],
        Evaluate[IntegerPart[sc]],
        Evaluate[IntegerPart[sr]]
        ]
      ]
    ]

SoundMakeFunc[s:SFP, opts___] :=
  Module[
    {},
    SoundMakeFunc[SoundGetFunc[s, opts], Sequence @@ RDC2O[RDCS[s,opts]], opts]
    ]

SoundMakeFunc[p:LP, opts___] := SoundMakeFunc[{p}, opts]

SoundMakeFunc[p:LLP, opts___] :=
  SoundMakeFunc[SoundMakeList[p,opts],opts]

SoundMakeFunc[s:SLP, opts___] :=
  Module[
    {},
    SoundMakeFunc[SoundGetFunc[s, opts], Sequence @@ RDC2O[RDCS[s,opts]], opts]
    ]

SoundMakeList[p:FP, opts___] := SoundMakeList[{p}, opts]

SoundMakeList[p:FLP, opts___] :=
  SoundMakeList[SoundMakeFunc[p,opts],opts]

SoundMakeList[s:SFP, opts___] :=
  Module[
    {},
    SoundMakeList[SoundGetList[s,opts], Sequence @@ RDC2O[RDCS[s,opts]], opts]
    ]

SoundMakeList[p:LP, opts___] := SoundMakeList[{p}, opts]

SoundMakeList[p:LLP, opts___] :=
  Module[
    {
      sr,sd,sc
      },
    {sr,sd,sc}=RDCZ[opts,SoundSampleCount->Max[Length/@p]];
    Sound[SampledSoundList[NormalizeList[p,opts],sr]]
    ]

SoundMakeList[s:SLP, opts___] :=
  Module[
    {},
    SoundMakeList[SoundGetList[s, opts], Sequence @@ RDC2O[RDCS[s,opts]], opts]
    ]

SoundMix[s_Sound, mix : {{_?FunctionQ, _?FunctionQ ...}, {_?FunctionQ, _?FunctionQ ...} ...}, opts___] :=
  Module[
    {
      fl = Table[
        Transpose[{Transpose[mix][[c]], SoundGetFunc[s, opts]}],
        {c, Length[mix[[1]]]}
        ]
      },
    fl = Function[t,
      Evaluate[
        Total[
          Function[w,
            Evaluate[#[[1]][w]#[[2]][w]]
            ][t] & /@ #
          ]
        ]
      ]& /@ fl;
    SoundMakeFunc[fl, Sequence @@ SoundGetInfo[s], opts]
    ] /; SoundGetChannelCount[s] == Length[mix] && Module[{x = Union[Length /@ mix]}, Length[x] == 1 && 1 <= x[[1]]]

SoundOfSilence[opts___] :=
  Module[
    {
      ch = SoundChannelCount /. {opts} /. Options[Zound]
      },
    SoundMakeFunc[Table[0 &, {ch}], Sequence @@ RDC2O[RDCZ[opts]]]
    ]

SoundPar[sl:SFLP, opts___] :=
  Module[{
      sd = Max[SoundGetDuration /@ sl],
      r = sl
      },
    (* create a list of sound objects whith the same duration *)
    r = Module[{csd = SoundGetDuration[#]},
      If[sd == csd, SoundMakeFunc[#, opts],
        SoundSeq[
          {
            SoundMakeFunc[#, opts],
            SoundOfSilence[SoundChannelCount->SoundGetChannelCount[#], SoundDuration-> sd-csd, opts]
            },
          opts
          ]
        ]
      ] & /@ r;
    r = Flatten[SoundGetFunc[#,opts]& /@ r];
    SoundMakeFunc[r, SoundDuration->sd, opts]
    ]

SoundPitchShift[s_Sound,r_,opts___] :=
  Module[
    {
      sr = SoundGetSampleRate[s],
      sd,
      sc = SoundGetSampleCount[s],
      f = Function[t,Evaluate[#[t*r]]]& /@ SoundGetFunc[s,opts]
      },
    sd = N[sc/(r*sr)];
    SoundMakeFunc[f, SoundDuration->sd, SampleRate->sr, opts]
    ]

SoundSetDuration[s_Sound,sd_,opts___] :=
  Module[
    {
      f = SoundGetFunc[s,SoundLoop->True],
      sr = SoundGetSampleRate[s]
      },
    SoundMakeFunc[f, SoundDuration->sd, SampleRate->sr, opts]
    ]

SSF[fl_, dl_] :=
  Module[{c,it,sl=Drop[DeltasToValues[dl],-1],bl,ni},
    bl = MapThread[(#1[c-#2])&, {fl, sl}];
    ni = MakeNestedIfs[Transpose[{dl, bl}], 0];
    it = ni[c];
    Function[Evaluate[c], Evaluate[it]]
    ]

SoundSeq[sl:SFLP, opts___] :=
  Module[
    {
      sd = Total[SoundGetDuration /@ sl],
      scc = Max[SoundGetChannelCount /@ sl],
      r = sl,
      dv, fv
      },
    (* create a list of sound objects whith the same number of channels *)
    r = Module[{cscc = SoundGetChannelCount[#]},
      If[scc == cscc, SoundMakeFunc[#, opts],
        SoundPar[
          {
            SoundMakeFunc[#, opts],
            SoundOfSilence[SoundChannelCount-> scc-cscc, SoundDuration->SoundGetDuration[#], opts]
            },
          opts
          ]
        ]
      ] & /@ r;
    (*the list of deltas*)
    dv = SoundGetDuration /@ r;
    (*the list of lists of functions, one per channel*)
    fv = Transpose[SoundGetFunc[#,opts]& /@ r];
    SoundMakeFunc[SSF[UnCompile /@ #, dv] & /@ fv, SoundDuration->sd, opts]
    ]

SoundUnPar[s_?SoundFuncQ,opts___] :=
  Module[
    {
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s]
      },
    SoundMakeFunc[#, SoundDuration->sd, SampleRate -> sr,opts] & /@ SoundGetFunc[s,opts]
    ]

SoundUnSeq[s_?SoundFuncQ, c_List,opts___] :=
  Module[
    {
      cc,
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s],
      fl = SoundGetFunc[s,opts]
      },
    cc = Cases[c, q$_ /; 0 < q$ < sd -> q$];
    If[Length[cc] == 0, s,
      cc = MapThread[{#1, #2 - #1} &, {Prepend[cc, 0], Append[cc, sd]}];
      Function[ci,
          SoundMakeFunc[
            Function[fi,
                Composition[fi,(# + ci[[1]])&]
                ] /@ fl,
            SoundDuration->ci[[2]],
            SampleRate -> sr,
            opts
            ]
          ] /@ cc
      ]
    ]

Options[Zound] =
  {
    SampleRate -> 2^13,
    SoundChannelCount -> 1,
    SoundDuration -> 1,
    SoundLoop -> False,
    SoundType -> SampledSoundFunction
    }

End[]

Protect[
  FuncToList,
  ListToFunc,
  SoundChannelCount,
  SoundDuration,
  SoundFuncQ,
  SoundGetDuration,
  SoundGetFunc,
  SoundGetInfo,
  SoundGetList,
  SoundGetChannelCount,
  SoundGetSampleCount,
  SoundGetSampleRate,
  SoundImportWav,
  SoundListQ,
  SoundLoop,
  SoundMakeFunc,
  SoundMakeList,
  SoundMix,
  SoundOfSilence,
  SoundPar,
  SoundPitchShift,
  SoundSampleCount,
  SoundSetDuration,
  SoundSeq,
  SoundType,
  SoundUnPar,
  SoundUnSeq,
  Zound
  ];

EndPackage[]

