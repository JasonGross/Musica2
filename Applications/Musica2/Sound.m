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
  2004-10-04  bch :  not much, lost track... sorry
  2004-09-24  bch :  changed SnippetData to Content
  2004-09-22  bch :  changed Show to Play2
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
  2004-09-12  bch :  using Common.m, Sound(G|S)etDuration is now (G|S)etDuration, SoundGetInfo is now GetInfo
  2004-08-28  bch :  added SoundMixStereo
  2004-08-27  bch :  removed " from some help/usage-text
                     added message ToDo
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
    "Musica2`Common`",
    "Musica2`Test`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Content,
  Convert,
  Mix,
  Par,
  Seq,
  Sound
  ];

Unprotect[
  SampleCount,
  Snippet,
  SnippetQ,
  SoundQ,
  SoundType
  ];

CreateElement[Snippet,
  {SoundType:(SampledSoundFunction|SampledSoundList), Content_, SampleRate_Integer, SampleCount_Integer},
  {SampledSoundFunction,Sin[2 Pi 440 #]&,2^13,2^13},
  "todo"
  ];
CreateContainer[Sound,Snippet,"todo"];

SampleCount::usage = "todo"
SoundType::usage = "todo"

Begin["`Private`"]

(*****************)

Options[Sound] = {
  SoundType->SampledSoundFunction,
  SampleRate->2^13,
  SampleCount->2^13
  }
TypeQ[Sound] = MatchQ[#, Sound[SampledSoundFunction[_,_Integer,_Integer]|SampledSoundList[_,_Integer]]]&;
Sound[d_?(DataQ[Sound]),opts___?OptionQ] :=
  Module[{st = SoundType /. {opts}, sr = SampleRate /. {opts}, sc = SampleCount /. {opts}},
  If[st === SampledSoundFunction,
    Sound[
      SampledSoundFunction[
        Evaluate[If[MatchQ[#,_CompiledFunction],#,Compile[{n},Evaluate[#[n]]]]& /@ d],
        Evaluate[IntegerPart[sc]],
        Evaluate[IntegerPart[sr]]
        ]
      ],
    Sound[
      SampledSoundList[
        d,
        IntegerPart[sr]
        ]
      ]
    ]
  ]
Sound /: Opts[x_Sound] := ({
  SoundType->#[[1,0]],
  SampleRate->If[MatchQ[#[[1]],_SampledSoundFunction],#[[1,3]],#[[1,2]]],
  SampleCount->If[MatchQ[#[[1]],_SampledSoundFunction],#[[1,2]],Length[#[[1,1,1]]]]
  }&[ReplacePart[Tidy[Sound][x],List,{0}]]) /; TypeQ[Sound][x];
Sound /: Data[x_Sound] := (ReplacePart[Tidy[Sound][x],List,{0}][[1,1]]) /; TypeQ[Sound][x];
Tidy[Sound] = Function[s,
  Module[{q=s},
    q[[0]]=List;
    If[MatchQ[q[[1]],_SampledSoundFunction],
      If[!ListQ[q[[1, 1]]],     q[[1, 1]] = {q[[1, 1]]}],
      If[Depth[q[[1, 1]]] != 3, q[[1, 1]] = {q[[1, 1]]}]
      ];
    q[[0]]=Sound;
    q
    ]
  ]
DataQ[Sound] = MatchQ[#, {__}]&;
Pack[Sound] = Function[{sup,sub},
  If[ListQ[sub],
    Snippet[{SampledSoundList,     sub, SampleRate/.Opts[sup], Length[sub]}],
    Snippet[{SampledSoundFunction, sub, SampleRate/.Opts[sup], SampleCount/.Opts[sup]}]
    ]
  ];
UnPack[Sound] = Function[{sub,opts},
  Content[Snippet[sub,Sequence @@ opts]]
  ];
UnPackOpts[Sound] = Function[{subs,opts},
  {
    SoundType->(SoundType/.opts/.{SoundType->SoundType[subs[[1]]]}),
    SampleRate->(SampleRate/.opts/.{SampleRate->SampleRate[subs[[1]]]}),
    SampleCount->(SampleCount/.opts/.{SampleCount->Max[SampleCount/@subs]})
    }
  ];

(*****************)

Snippet /: Data[x_Snippet, y_, SoundType, pos_] :=
  Module[{s = x,f,d,i},
    If[SoundType[x] === SampledSoundFunction && y === SampledSoundList,
      f = Content[s];
      d = Table[f[i],{i,SampleCount[s]}];
      s = ReplacePart[s,d,Content];
      s = Data[s,SampledSoundList,pos];
      ];
    If[SoundType[x] === SampledSoundList && y === SampledSoundFunction,
      d = Content[s];
      f = ListInterpolation[d,InterpolationOrder->1];
      s = ReplacePart[s,f,Content];
      s = Data[s,SampledSoundFunction,pos];
      ];
    s
    ]

Snippet /: Data[x_Snippet, y_, SampleCount, pos_] :=
  Module[{s = x,d},
    If[y =!= SampleCount[x] && IntegerQ[y] && 0 < y,
      If[SoundType[x] === SampledSoundList,
        d = Content[s];
        d = If[y < SampleCount[s],
          Take[d,y],
          PadRight[d,y]
          ];
        s = ReplacePart[s,d,Content];
        ];
      s = Data[s,y,pos];
      ];
    s
    ]
    
Snippet /: Duration[x_Snippet] := SampleCount[x]/SampleRate[x]
Sound   /: Duration[x_Sound] := SampleCount[x]/SampleRate[x]

Mix[x:{__Snippet}] := "todo" (* todo: write the code! *)
Mix[x_Sound, mix : {{__}...}] :=
  Module[
    {
      fl = Table[
        Transpose[{Transpose[mix][[c]], UnCompile /@ Data[x]}],
        {c, Length[mix[[1]]]}
        ]
      },
    fl = Function[t,
      Evaluate[
        Total[
          Function[w,
            Evaluate[(#[[1]][w])*(#[[2]][w])]
            ][t] & /@ #
          ]
        ]
      ]& /@ fl;
    Sound[fl,Sequence @@ Opts[x]]
    ] /; (SoundType[x]===SampledSoundFunction) && (Length[x] == Length[mix]) && Module[{o = Union[Length /@ mix]}, (Length[o] == 1) && (1 <= o[[1]])]

Mix[s_Sound,1] :=
  Module[{c = Length[s],mix},
    If[c == 1, s,
      mix = Table[{Evaluate[1/c]&},{c}];
      Mix[s, mix]
      ]
    ]

Mix[s_Sound,2] :=
  Module[{c = Length[s],mix},
    If[c == 2, s,
      mix = If[c == 1, {{1 &, 1 &}},Table[N[{Evaluate[2(c - i)/(c^2 - c)] &, Evaluate[2(i - 1)/(c^2 - c)] &}], {i, c}]];
      Mix[s, mix]
      ]
    ]

Par[x:{__Snippet},opts___?OptionQ] := Sound[x,opts]
Par[x:{__Sound},  opts___?OptionQ] := Sound[Flatten[Snippet /@ x],opts] (* todo: handle opts *)

Snippet /: Play2[x_Snippet] := Play2[Sound[x]]
Sound   /: Play2[x_Sound]   := Show[Mix[x,2]]

Sound /: SampleCount[x_Sound] := SampleCount /. Opts[x]

Sound /: SampleRate[x_Sound] := SampleRate /. Opts[x]

Seq[x:{__Snippet},opts___?OptionQ] := "todo" (* todo: write the code! *)
Seq[x:{__Sound},  opts___?OptionQ] := "todo" (* todo: write the code! *)

Sound /: SoundType[x_Sound] := SoundType /. Opts[x]

End[]

Protect[
  Content,
  Convert,
  Mix,
  Par,
  Seq,
  Sound
  ];

Protect[
  SampleCount,
  Snippet,
  SnippetQ,
  SoundQ,
  SoundType
  ];

EndPackage[]

