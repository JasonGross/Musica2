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
  2004-08-28  bch :  added SoundMixStereo
  2004-08-27  bch :  removed " from some help/usage-text
                     added message todo
  2004-08-26  bch :  added some help/usage-text
  2004-08-23  bch :  added SoundExportWav
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
  SoundExportWav,
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
  SoundMixStereo,
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

SoundMakeFunc::todo = "Make it possible to give not just pure functions as function-argument."
SoundMakeList::todo = "Make it possible to give not just pure functions as function-argument."
SoundPar::todo = "Handle the list-only-case without making func's."
SoundPitchShift::todo = "Handle the list-only-case without making func's."
SoundSetDuration::todo = "Handle the list-only-case without making func's."
SoundSeq::todo = "Handle the list-only-case without making func's."
SoundUnSeq::todo = "Use this one with care as it probably WILL CHANGE."

FuncToList::usage = "FuncToList[f_, opts___] takes a function and samples it. The function is supposed to be a function of time (sec). The opts-argument takes SampleRate, SoundDuration and/or SoundSampleCount and defaults to Options[Zound] if required."
ListToFunc::usage = "ListToFunc[d_, opts___] takes a list and makes a function out of it. The function returned is a function of time (sec). The opts-argument takes SampleRate, SoundDuration and/or SoundSampleCount and defaults to Options[Zound] if required. The opts-argument can also be PlayRange and InterpolationOrder. InterpolationOrder defaults to 0 (zero) and PlayRange defaults to whatever NormalizeList does."
SoundChannelCount::usage = "SoundChannelCount is returned by the SoundGetInfo function and is also an opt to various functions."
SoundDuration::usage = "SoundDuration is returned by the SoundGetInfo function and is also an opt to various functions."
SoundExportWav::usage = "SoundExportWav[fn_String, s_Sound] simply calls Export[fn,s,WAV]."
SoundFuncQ::usage = "SoundFuncQ[expr_] tests if expr is a Sound-object based on a list of function."
SoundGetChannelCount::usage = "SoundGetChannelCount[s_Sound] returns the number of channels in the Sound-object."
SoundGetDuration::usage = "SoundGetDuration[s_Sound] returns the duration in sec in the Sound-object."
SoundGetFunc::usage = "SoundGetFunc[s_Sound, opts___] returns a list of functions. If the Sound-object contains lists of sample-lists ListToFunc is called and opts can be InterpolationOrder. If opts is SoundLoop the returned functions will call Mod on its argument."
SoundGetInfo::usage = "SoundGetInfo[s_Sound] returns various info about the Sound-object."
SoundGetList::usage = "SoundGetList[s_Sound] returns a list of sample-lists. If the Sound-object contains a list of functions FuncToList is called."
SoundGetSampleCount::usage = "SoundGetSampleCount[s_Sound] returns the number of samples per channel in the Sound-object."
SoundGetSampleRate::usage = "SoundGetSampleRate[s_Sound] returns the sample-rate in the Sound-object."
SoundImportWav::usage = "SoundImportWav[fn_String] simply calls Import[fn,WAV]."
SoundListQ::usage = "SoundListQ[expr_] tests if expr is a Sound-object based on a list of sample-lists."
SoundLoop::usage = "Currently an option to SoundGetFunc and SoundSetDuration."
SoundMakeFunc::usage = "SoundMakeFunc[p_, opts___] creates a Sound-object based on a list of functions. The p-argument can be a function, a list of functions, a sample-list, a list of sample-lists or a Sound-object. The opts-argument can be SampleRate, SoundDuration, SoundSampleCount and InterpolationOrder depending on the type of the p-argumnet and defaults to Options[Zound]."<>ToDoString<>SoundMakeFunc::todo
SoundMakeList::usage = "SoundMakeList[p_, opts___] creates a Sound-object based on a list of sample-lists. The p-argument can be a function, a list of functions, a sample-list, a list of sample-lists or a Sound-object. The opts-argument can be SampleRate, SoundDuration, SoundSampleCount and InterpolationOrder depending on the type of the p-argumnet and defaults to Options[Zound]."<>ToDoString<>SoundMakeList::todo
SoundMix::usage = "SoundMix[s_Sound, mix : {{_?FunctionQ, _?FunctionQ ...}, {_?FunctionQ, _?FunctionQ ...} ...}, opts___] creates a new Sound-object by using a mix of the s-arguments channels. The mix-argument is a list of lists containing functions of time that determin the amplitude. The mix-argument must have the same length as the s-argument has channels. Each list in the mix-argument-list mus be of the same length and determines the number ocf channels in the resulting Sound-object."
SoundMixStereo::usage = "SoundMixStereo[s_Sound] calls SoundMix to make a simple stereo-mix."
SoundOfSilence::usage = "SoundOfSilence[opts___] returns an sound-less Sound-object. The opts-argument may be SoundChannelCount and all the usual opts and defaults to Options[Zound]."
SoundPar::usage = "SoundPar[sl:SFLP, opts___] takes a list of sound-function-objects and creates a new one with the supplied Sound-objects in parallel."<>ToDoString<>SoundPar::todo
SoundPitchShift::usage = "SoundPitchShift[s_Sound,r_,opts___] changes the pitch of the s-argument by changing the duration and keeping the sample-rate."<>ToDoString<>SoundPitchShift::todo
SoundSetDuration::usage = "SoundSetDuration[s_Sound,sd_,opts___] sets the duration by either chopping it or looping it."<>ToDoString<>SoundSetDuration::todo
SoundSampleCount::usage = "SoundSampleCount is returned by the SoundGetInfo function and is also an opt to various functions."
SoundSeq::usage = "SoundSeq[sl:SFLP, opts___] takes a list of sound-function-objects and creates a new one with the supplied Sound-objects in sequence."<>ToDoString<>SoundSeq::todo
SoundType::usage = "SoundSampleCount is returned by the SoundGetInfo function and WILL BE an opt to various functions."
SoundUnPar::usage = "SoundUnPar[s_?SoundFuncQ] splits the Sound-object to a list of Sound-objects, one for each channel. The opposite to SoundPar."
SoundUnSeq::usage = "SoundUnSeq[s_?SoundFuncQ, c_List] splits the Sound-object to a list of Sound-objects. The c-parameter is a list of points in time where to cut. The opposite to SoundSeq."<>ToDoString<>SoundUnSeq::todo
Zound::usage = "The holder of default values/options."

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

SoundExportWav[fn_String, s_Sound] := Export[fn,s,"WAV"]

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

SoundMixStereo[s_Sound] :=
  Module[{c = SoundGetChannelCount[s],mix},
    If[c == 2, s,
      mix = If[c == 1, {{1 &, 1 &}},Table[N[{Evaluate[2(c - i)/(c^2 - c)] &, Evaluate[2(i - 1)/(c^2 - c)] &}], {i, c}]];
      SoundMix[s, mix]
      ]
    ]

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
  SoundExportWav,
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
  SoundMixStereo,
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

