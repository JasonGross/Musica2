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
  SoundSampleCount,
  SoundSeq,
  SoundSmooth,
  SoundType,
  SoundUnPar,
  SoundUnSeq,
  Zound
  ];

SoundChannelCount::usage = ""
SoundDuration::usage = ""
SoundFuncQ::usage = "SoundFuncQ[expr_]"
SoundGetChannelCount::usage = "SoundGetChannelCount[s_Sound]"
SoundGetDuration::usage = "SoundGetDuration[s_Sound]"
SoundGetFunc::usage = "SoundGetFunc[s_?SoundFuncQ]"
SoundGetInfo::usage = "SoundGetInfo[s_Sound]"
SoundGetList::usage = "SoundGetList[s_?SoundListQ]"
SoundGetSampleCount::usage = "SoundGetSampleCount[s_Sound]"
SoundGetSampleRate::usage = "SoundGetSampleRate[s_Sound]"
SoundImportWav::usage = "SoundImportWav[fn_String]"
SoundListQ::usage = "SoundListQ[expr_]"
SoundLoop::usage = ""
SoundMakeFunc::usage = "SoundMakeFunc[fl_?Func1ListQ, opts___]"
SoundMakeList::usage = "SoundMakeList[ll_, opts___]"
SoundMix::usage = "SoundMix[s_Sound, mix : {{_?Func1Q, _?Func1Q ...}, {_?Func1Q, _?Func1Q ...} ...}, opts___]"
SoundOfSilence::usage = "SoundOfSilence[opts___]"
SoundPar::usage = "SoundPar[sl:SFLP, opts___]"
SoundSampleCount::usage = ""
SoundSeq::usage = "SoundSeq[sl:SFLP, opts___]"
SoundSmooth::usage = ""
SoundType::usage = ""
SoundUnPar::usage = "SoundUnPar[s_?SoundFuncQ]"
SoundUnSeq::usage = "SoundUnSeq[s_?SoundFuncQ, c_List]"
Zound::usage = ""

Begin["`Private`"]

SFP = _?SoundFuncQ
SFLP = {SFP,SFP...}

LP = {_Integer | _Real, (_Integer | _Real) ...}
LLP = {LP,LP...}

SoundFuncQ[expr_] := MatchQ[expr, Sound[SampledSoundFunction[_, _, _]]]

SoundGetChannelCount[s_?SoundFuncQ] := If[ListQ[s[[1, 1]]],Length[s[[1, 1]]],1]

SoundGetChannelCount[s_?SoundListQ] := If[Depth[s[[1, 1]]] == 3, Length[s[[1, 1]]], 1]

SoundGetDuration[s_Sound] := N[SoundGetSampleCount[s]/SoundGetSampleRate[s]]

SoundGetFunc[s_?SoundFuncQ, opts___] :=
  Module[
    {
      sc = SoundGetSampleCount[s],
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s],
      cfl = If[ListQ[s[[1, 1]]], s[[1, 1]], {s[[1, 1]]}]
      },
      If[SoundLoop /. {opts} /. Options[Zound],
        ReArg1[#,t,1 + Mod[t*sr, sc]]& /@ cfl,
        ReArg1[#,t,1 + t*sr]& /@ cfl
        ]
    ]

SoundGetFunc[s_?SoundListQ, opts___] :=
  Module[
    {
      sr = SoundGetSampleRate[s],
      fl = SoundGetList[s],
      sm = SoundSmooth /. {opts} /. Options[Zound]
      },
    ListToFunc1[#, sr, sm]& /@ fl
    ]

SoundGetInfo[s_Sound] :=
  Module[
    {
      sc = SoundGetSampleCount[s],
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s],
      scc = SoundGetChannelCount[s],
      st = If[SoundFuncQ[s],SampledSoundFunction,SampledSoundList]
      },
    {SoundSampleCount->sc, SampleRate->sr, SoundDuration->sd, SoundChannelCount->scc, SoundType->st}
    ]

SoundGetList[s_?SoundFuncQ] :=
  Module[{
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s],
      fl = SoundGetFunc[s]
      },
    Func1ToList[#, sr, sd]& /@ fl
    ]

SoundGetList[s_?SoundListQ] := If[Depth[s[[1, 1]]] == 3, s[[1, 1]], {s[[1, 1]]}]

SoundGetSampleCount[s_?SoundFuncQ] := s[[1, 2]]

SoundGetSampleCount[s_?SoundListQ] := If[Depth[s[[1, 1]]] == 3, Max[Length /@ s[[1, 1]]], Length[s[[1, 1]]]]

SoundGetSampleRate[s_?SoundFuncQ] := s[[1, 3]]

SoundGetSampleRate[s_?SoundListQ] := s[[1, 2]]

SoundImportWav[fn_String] := Import[fn,"WAV"]

SoundListQ[expr_] := MatchQ[expr, Sound[SampledSoundList[_, _]]]

SoundMakeFunc[f_?Func1Q, opts___] := SoundMakeFunc[{f}, opts]

SoundMakeFunc[fl_?Func1ListQ, opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. Options[Zound],
      sd = SoundDuration /. {opts} /. Options[Zound]
      },
    Sound[
      SampledSoundFunction[
        Evaluate[Compile[{{n,_Integer}},Evaluate[ReArg1[#,n,(n - 1.0)/sr][n]]]& /@ fl],
        Evaluate[IntegerPart[sr*sd]],
        Evaluate[IntegerPart[sr]]
        ]
      ]
    ]

SoundMakeFunc[s_?SoundFuncQ, opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. {SampleRate->SoundGetSampleRate[s]}
      },
    If[sr == SoundGetSampleRate[s],
      s,
      SoundMakeFunc[SoundGetFunc[s], SoundDuration->SoundGetDuration[s], SampleRate -> sr]
      ]
    ]

SoundMakeFunc[s_?SoundListQ, opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. {SampleRate->SoundGetSampleRate[s]},
      sd = SoundGetDuration[s],
      sm = SoundSmooth /. {opts} /. Options[Zound]
      },
    SoundMakeFunc[SoundGetFunc[s, SoundSmooth->sm], SoundDuration->sd, SampleRate->sr]
  ]

SoundMakeList[f:LP, opts___] := SoundMakeList[{f}, opts]

SoundMakeList[fl:{LP,LP...}, opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. Options[Zound],
      pr = PlayRange /. {opts},
      md,sp
      },
    If[pr === PlayRange, pr = {Min[fl],Max[fl]}];
    md = (pr[[1]]+pr[[2]])/2;
    sp = (pr[[2]]-pr[[1]])/2;
    Sound[SampledSoundList[N[(fl-md)/sp],sr]]
    ]

SoundMakeList[s_?SoundFuncQ, opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. {SampleRate->SoundGetSampleRate[s]}
      },
    SoundMakeList[SoundGetList[s],SampleRate->sr]
    ]

SoundMakeList[s_?SoundListQ, opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. {SampleRate->SoundGetSampleRate[s]},
      sm = SoundSmooth /. {opts} /. Options[Zound]
      },
    If[sr == SoundGetSampleRate[s],
      s,
      SoundMakeList[SoundMakeFunc[s, SoundSmooth->sm,SampleRate->sr],SampleRate->sr]
      ]
    ]

SoundMix[s_Sound, mix : {{_?Func1Q, _?Func1Q ...}, {_?Func1Q, _?Func1Q ...} ...}, opts___] :=
  Module[
    {
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s],
      fl = Table[
        Transpose[{Transpose[mix][[c]], SoundGetFunc[s]}],
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
    SoundMakeFunc[fl, SampleRate -> sr, SoundDuration -> sd]
    ] /; SoundGetChannelCount[s] == Length[mix] && Module[{x = Union[Length /@ mix]}, Length[x] == 1 && 1 <= x[[1]]]

SoundOfSilence[opts___] :=
  Module[
    {
      sr = SampleRate /. {opts} /. Options[Zound],
      sd = SoundDurtaion /. {opts} /. Options[Zound],
      ch = SoundChannelCount /. {opts} /. Options[Zound]
      },
    SoundMakeFunc[Table[0 &, {ch}], SoundDurtaion->sd, SampleRate -> sr]
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
    r = Flatten[SoundGetFunc /@ r];
    SoundMakeFunc[r, SoundDuration->sd, opts]
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
    fv = Transpose[SoundGetFunc /@ r];
    SoundMakeFunc[SSF[UnCompile /@ #, dv] & /@ fv, SoundDuration->sd, opts]
    ]

SoundUnPar[s_?SoundFuncQ] :=
  Module[
    {
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s]
      },
    SoundMakeFunc[#, SoundDuration->sd, SampleRate -> sr] & /@ SoundGetFunc[s]
    ]

SoundUnSeq[s_?SoundFuncQ, c_List] :=
  Module[
    {
      cc,
      sr = SoundGetSampleRate[s],
      sd = SoundGetDuration[s],
      fl = SoundGetFunc[s]
      },
    cc = Cases[c, q$_ /; 0 < q$ < sd -> q$];
    If[Length[cc] == 0, s,
      cc = MapThread[{#1, #2 - #1} &, {Prepend[cc, 0], Append[cc, sd]}];
      Function[ci,
          SoundMakeFunc[
            Function[fi,
                ReArg1[fi, t, t + ci[[1]]]
                ] /@ fl,
            SoundDuration->ci[[2]],
            SampleRate -> sr
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
    SoundSmooth -> False,
    SoundType -> SampledSoundFunction
    }

End[]

Protect[
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
  SoundSampleCount,
  SoundSeq,
  SoundSmooth,
  SoundType,
  SoundUnPar,
  SoundUnSeq,
  Zound
  ];

EndPackage[]

