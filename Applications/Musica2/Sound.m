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
  2005-02-16  bch :  initiated usage of Usage ;-)
  2005-02-13  bch :  reorganized code in file, hopefully in an uniform manner
  2005-01-21  bch :  converting from func to list now does //N as well
  2005-01-20  bch :  quite a lot actually, better handling of opts and conversions a,d seq and par and...
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
    "Musica2`ObjectType`",
    "Musica2`Test`",
    "Musica2`Usage`",
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

CreateElement[Musica2,Snippet,
{SoundType:(SampledSoundFunction|SampledSoundList), Content_, SampleRate_Integer, SampleCount_Integer},
{SampledSoundFunction,Sin[2 Pi 440 #/2^13]&,2^13,2^13},
"todo.\[NewLine]"
];
CreateContainer[Musica2,Sound,Snippet,
"todo.\[NewLine]"
];

SampleCount::usage = "todo"
SoundType::usage = "todo"

TestOne::usage=""
TestTwo::usage=""

Begin["`Private`"]

(* Snippet ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Snippet modifications and interceptions *)

Snippet /: Data[x_Snippet, y_, SoundType, pos_] :=
  Module[{s = x,f,d,i},
    If[SoundType[x] === SampledSoundFunction && y === SampledSoundList,
      f = Content[s];
      d = Table[f[i],{i,SampleCount[s]}] // N;
      s = ReplacePart[s,d,Content];
      s = Data[s,SampledSoundList,pos];
      ];
    If[SoundType[x] === SampledSoundList && y === SampledSoundFunction,
      d = Content[s];
      f = Interpolation[d,InterpolationOrder->1];
      s = ReplacePart[s,f,Content];
      s = Data[s,SampledSoundFunction,pos];
      ];
    s
    ]

Snippet /: Data[x_Snippet, y_, SampleCount, pos_] :=
  Module[{s = x,d,sc=SampleCount[x]},
    If[y =!= SampleCount[x] && IntegerQ[y] && 0 < y,
      d = Content[s];
      d = If[SoundType[x] === SampledSoundList,
        If[sc < y,
          PadRight[d,y],
          Take[d,y]
          ],
        If[sc < y,
          Evaluate[If[sc < #,0,Evaluate[d[#]]]]&,
          d
          ]
        ];
      s = ReplacePart[s,d,Content];
      s = Data[s,y,pos];
      ];
    s
    ]
    
(* Snippet constructors *)

Usage[Append,Musica2,Snippet,{{DataNoValue, _},___?OptionQ},_Snippet,"todo"]
Snippet[{DataNoValue, d_},opts___?OptionQ] := Snippet[{
  SampledSoundFunction,
  0&,
  SampleRate /. Options[Snippet],
  d * (SampleRate /. Options[Snippet])
  },opts]

Usage[Append,Musica2,Snippet,{_?NumberQ,___?OptionQ},_Snippet,"todo"]
Snippet[x_?NumberQ,opts___?OptionQ] := Snippet[{DataNoValue, x},opts]

Snippet[SampledSoundFunction] := Snippet[{SampledSoundFunction,0#,2,2}]
Snippet[SampledSoundList] := Snippet[{SampledSoundList,{0,0},2,2}]

(* Snippet reverse constructors *)

(* Snippet common functions *)

Usage[Append,Musica2,Duration,{_Snippet},_,"todo"]
Snippet /: Duration[x_Snippet] := SampleCount[x]/SampleRate[x]

Usage[Append,Musica2,Mix,{_Snippet},_Snippet,"todo"]
Mix[x_Snippet] := x

Usage[Append,Musica2,Mix,{{__Snippet}},_Snippet,"todo"]
Mix[x:{__Snippet}] := Snippet[Mix[Par[x],1]][[1]]

Usage[Append,Musica2,Par,{{__Snippet}},_Sound,"todo"]
Par[x:{__Snippet},opts___?OptionQ] := Sound[x,opts]

Usage[Append,Musica2,Play2,{_Snippet},_Sound,"todo"]
Snippet /: Play2[x_Snippet] := Play2[Sound[x]]

SSF[fl_, vl_,dl_] :=
  Module[{c,it,bl,ni},
    bl = MapThread[(#1[c-#2])&, {fl, vl}];
    ni = MakeNestedIfs[Transpose[{dl, bl}], 0&];
    it = ni[c-1];
    Function[Evaluate[c], Evaluate[it]]
    ]

Usage[Append,Musica2,Seq,{{__Snippet}},_Snippet,"todo"]
Seq[x:{__Snippet},opts___?OptionQ] :=
  Module[
    {
      s,c,d,v,
      st = SoundType /. {opts} /. {SoundType->SoundType[x[[1]]]},
      sr = SampleRate /. {opts} /. {SampleRate->SampleRate[x[[1]]]},
      sc
      },
    s = Snippet[x,SoundType->st,SampleRate->sr];
    s = Tidy /@ s;
    s = If[st === SampledSoundList,

      c = Flatten[Content /@ s];
      sc = Length[c];
      Snippet[{SampledSoundList, c, sr, sc}],
      
      d = SampleCount /@ s;
      v = Drop[DeltasToValues[d],-1];
      c = SSF[Content/@s,v,d];
      sc = Total[SampleCount /@ s];
      Snippet[{SampledSoundFunction, c, sr, sc}]
      ];
    s
    ]
    
Usage[Append,Musica2,Tidy,{Snippet},_Function,"todo"]
Tidy[Snippet] = Function[s,
  Module[{r = s},
    If[SoundType[r] === SampledSoundFunction,
      r = ReplacePart[r,UnCompile[Content[r]],Content],
      r = ReplacePart[r,Length[Content[r]],SampleCount]
      ];
    r
    ]
  ]
  
Usage[Append,Musica2,TotalDuration,{_Snippet},_,"todo"]
Snippet /: TotalDuration[x_Snippet] := SampleCount[x]/SampleRate[x]

(* Snippet unique functions *)

(* Snippet tests *)

TestOne[at_,bt_,r_,t_] :=
  Module[{a, b, s},
    Export["tmp.wav", "almost empty", "Text"];
    a = Snippet[{SampledSoundFunction, Sin[#1]& , 2, 7}];
    b = Snippet[{SampledSoundFunction, Sin[#1]& , 3, 5}];
    a = Snippet[a, SoundType -> at];
    b = Snippet[b, SoundType -> bt];
    s = If[t,
      If[r,
        Par[{Seq[{b, a}], Seq[{a, b}]}],
        Par[{Seq[{a, b}], Seq[{b, a}]}]
        ],
      If[r,
        Seq[{Par[{b, a}], Par[{a, b}]}],
        Seq[{Par[{a, b}], Par[{b, a}]}]
        ]
      ];
    Export["tmp.wav", s];
    Total[Flatten[Content[Import["tmp.wav"]]]]
    ]

Snippet /: TestSuite[Snippet] = Join[TestSuite[Snippet],{
  TestCase[TestOne[SampledSoundFunction,SampledSoundFunction,False,False],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundFunction,True,False],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundList,False,False],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundList,True,False],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundFunction,False,False],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundFunction,True,False],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundList,False,False],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundList,True,False],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundFunction,False,True],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundFunction,True,True],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundList,False,True],1.40625],
  TestCase[TestOne[SampledSoundFunction,SampledSoundList,True,True],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundFunction,False,True],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundFunction,True,True],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundList,False,True],1.40625],
  TestCase[TestOne[SampledSoundList,SampledSoundList,True,True],1.40625]
  }]

(* Snippet --------------------------------------------------------------------------*)

(* Sound ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Sound modifications and interceptions *)

Options[Sound] = {
  SoundType->SampledSoundFunction,
  SampleRate->2^13,
  SampleCount->2^13
  }

Sound /: Data[x_Sound] := (ReplacePart[Tidy[Sound][x],List,{0}][[1,1]]) /; ObjectTypeQ[Sound][x];

DataQ[Sound] = MatchQ[#, {__}]&;

ObjectTypeQ[Sound] = MatchQ[#, Sound[SampledSoundFunction[_,_Integer,_Integer]|SampledSoundList[_,_Integer]]]&;

Sound /: Opts[x_Sound] := ({
  SoundType->#[[1,0]],
  SampleRate->If[MatchQ[#[[1]],_SampledSoundFunction],#[[1,3]],#[[1,2]]],
  SampleCount->If[MatchQ[#[[1]],_SampledSoundFunction],#[[1,2]],Length[#[[1,1,1]]]]
  }&[ReplacePart[Tidy[Sound][x],List,{0}]]) /; ObjectTypeQ[Sound][x];

Pack[Sound] = Function[{sup,sub},
  If[ListQ[sub],
    Snippet[{SampledSoundList,     sub, SampleRate/.Opts[sup], Length[sub]}],
    Snippet[{SampledSoundFunction, sub, SampleRate/.Opts[sup], SampleCount/.Opts[sup]}]
    ]
  ];
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

UnPack[Sound] = Function[{sub,opts},
  Content[Snippet[sub,Sequence @@ opts]]
  ];

UnPackOpts[Sound] = Function[{subs,opts},{
  SoundType->(SoundType/.opts/.{SoundType->SoundType[subs[[1]]]}),
  SampleRate->(SampleRate/.opts/.{SampleRate->SampleRate[subs[[1]]]}),
  SampleCount->(SampleCount/.opts/.{SampleCount->Max[SampleCount /@ subs]})
  }];

(* Sound constructors *)

Usage[Append,Musica2,Sound,{_?(DataQ[Sound]),___?OptionQ},_Sound,"todo"]
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

(* Sound reverse constructors *)

(* Sound common functions *)

Usage[Append,Musica2,Mix,{_Sound,  {{__}...}},_Sound,"todo"]
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

Mix[x_Sound, mix : {{__}...}] :=
  Module[
    {
      sl = Table[
        Transpose[{Transpose[mix][[c]], Data[x]}],
        {c, Length[mix[[1]]]}
        ]
      },
    sl = Function[t,Total[Function[fd,MapIndexed[(fd[[1]][#2[[1]]]#1)&,fd[[2]]]]/@t]] /@ sl;
    Sound[sl,Sequence @@ Opts[x]]
    ] /; (SoundType[x]===SampledSoundList) && (Length[x] == Length[mix]) && Module[{o = Union[Length /@ mix]}, (Length[o] == 1) && (1 <= o[[1]])]

Usage[Append,Musica2,Mix,{_Sound,1},_Sound,"todo"]
Mix[s_Sound,1] :=
  Module[{c = Length[s],mix},
    If[c == 1, s,
      mix = Table[{Evaluate[1/c]&},{c}];
      Mix[s, mix]
      ]
    ]

Usage[Append,Musica2,Mix,{_Sound,2},_Sound,"todo"]
Mix[s_Sound,2] :=
  Module[{c = Length[s],mix},
    If[c == 2, s,
      mix = If[c == 1, {{1 &, 1 &}},Table[N[{Evaluate[2(c - i)/(c^2 - c)] &, Evaluate[2(i - 1)/(c^2 - c)] &}], {i, c}]];
      Mix[s, mix]
      ]
    ]

Usage[Append,Musica2,Par,{{__Sound},___?OptionQ},_Sound,"todo"]
Par[x:{__Sound},  opts___?OptionQ] := Sound[Flatten[Snippet /@ x],opts]

Usage[Append,Musica2,Play2,{_Sound},_Sound,"todo"]
Sound   /: Play2[x_Sound]   := Show[Mix[x,2]]

Usage[Append,Musica2,SampleCount,{_Sound},_Integer,"todo"]
Sound /: SampleCount[x_Sound] := SampleCount /. Opts[x]

Usage[Append,Musica2,SampleRate,{_Sound},_Integer,"todo"]
Sound /: SampleRate[x_Sound] := SampleRate /. Opts[x]

Usage[Append,Musica2,Seq,{{__Sound},___?OptionQ},_Sound,"todo"]
Seq[x:{__Sound},opts___?OptionQ] :=
  Module[
    {
      s=x,
      st = SoundType /. {opts} /. {SoundType->SoundType[x[[1]]]},
      sr = SampleRate /. {opts} /. {SampleRate->SampleRate[x[[1]]]},
      sc,
      ch = Max[Length /@ x]
      },
    s = (
      s=#;
      While[Length[s]<ch,
        s=Append[s,Snippet[SampledSoundList]]
        ];
      s
      )& /@ s;
    s = Snippet /@ s;
    s = Transpose[s];
    s = Seq /@ s;
    Sound[s]
    ]

Usage[Append,Musica2,SoundType,{_Sound},(SampledSoundFunction|SampledSoundList),"todo"]
Sound /: SoundType[x_Sound] := SoundType /. Opts[x]

Usage[Append,Musica2,TotalDuration,{_Sound},_Integer,"todo"]
Sound /: TotalDuration[x_Sound] := SampleCount[x]/SampleRate[x]

(* Sound unique functions *)

(* Sound tests *)

Sound /: TestSuite[Sound] = Join[TestSuite[Sound],{
  }];

(* Sound --------------------------------------------------------------------------*)

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

