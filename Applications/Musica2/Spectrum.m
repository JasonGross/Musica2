(* :Title: Spectrum *)

(* :Summary: Functions for Spectrum *)

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

(* :Context: Musica2`Spectrum` *)

(* :History:
  2005-01-27  bch :  renamed Spectrum to ToneSpectrum to avoid collision with Combinatorica
  2004-10-07  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Spectrum`",
  {
    "Musica2`Common`",
    "Musica2`ObjectType`",
    "Musica2`Sound`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Frequency,
  Mix
  ];

Unprotect[
  ToneSpectrum,
  ToneSpectrumQ,
  Tone,
  ToneQ
  ];

CreateElement[Tone, {Frequency_, {Amplitude_, Phase_}},{440,{1,0}},
"todo.\[NewLine]"
];
CreateContainer[ToneSpectrum,Tone,
"todo.\[NewLine]"
];

Amplitude::usage = "Amplitude is a member of Tone."

Begin["`Private`"]

Mix[x:{__ToneSpectrum}, opts___?OptionQ] := ToneSpectrum[Flatten[Tone /@ x]]

ToneSpectrum /: Snippet[x_ToneSpectrum, opts___?OptionQ] := (* todo: an option to use IFFT *)
  Module[
    {
      sr = Round[SampleRate /. {opts} /. Opts[x] /. Options[Sound]],
      sc = Round[SampleCount /. {opts} /. Opts[x] /. Options[Sound]],
      f,t
      },
      f = Total[(Amplitude[#] Sin[Phase[#] + 2Pi Frequency[#]t/sr])& /@ x];
      f = Function[s,Evaluate[f /. {t->s}]];
      Snippet[{SampledSoundFunction,f,sr,sc}]
    ]

ToneSpectrum[x_Snippet, opts___?OptionQ] :=
  Module[
    {
      s = Snippet[x,SoundType -> SampledSoundList],
      sr = SampleRate[x],
      sc = SampleCount[x],
      d,r
      },
    d = Fourier[Content[s],FourierParameters -> {-1, 1}];
    (* skip zero-freq *)
    d = Rest[d];
    (* nyquist theorem *)
    d = Take[d,Ceiling[Length[d]/2]];

    d = {2Abs[#],Arg[#]}& /@ d;
    r = ToneSpectrum[MapIndexed[Tone[{sr #2[[1]]/sc, #1}]&, d], opts, SampleRate->sr, SampleCount->sc];
    r
    ]

ToneSpectrum[x:{__?AtomQ}, opts___?OptionQ] := ToneSpectrum[Tone /@ x,opts]

Tidy[ToneSpectrum] = Module[{r = #,i,c},
  r = Tidy /@ r;
  r = Select[r, 0 =!= Amplitude[#]&];
  r = Sort[r, Frequency];
  r = Tone[r];
  For[i = 1, i < Length[r], i++,
    If[Frequency[r[[i]]] === Frequency[r[[i+1]]],
      c = Complex[r[[i]]] + Complex[r[[i+1]]];
      r = Delete[r,i+1];
      If[c!=0,
        r[[i]] = Tidy[Tone[{Frequency[r[[i]]],c},Sequence @@ Opts[r[[i]]]]],
        r = Delete[r,i];
        ];
      i--;
      ];
    ];
  r = ToneSpectrum[r, Sequence @@ Opts[#]];
  r
  ]&

Tidy[Tone] = Module[{r = #},
  If[Frequency[r] < 0,
    r = ReplacePart[r,-Frequency[r],Frequency];
    r = ReplacePart[r,Phase[r]+Pi,Phase]
    ];
  If[Amplitude[r] < 0,
    r = ReplacePart[r,-Amplitude[r],Amplitude];
    r = ReplacePart[r,Phase[r]+Pi,Phase]
    ];
  If[Phase[r] < 0 || 2Pi <= Phase[r],
    r = ReplacePart[r,Mod[Phase[r],2Pi],Phase]
    ];
  r
  ]&

ToneSpectrum /: Plus[a_ToneSpectrum,b_ToneSpectrum] := ToneSpectrum[Join[Tone[a],Tone[b]]]
ToneSpectrum /: Plus[a_ToneSpectrum,b_Tone] := ToneSpectrum[Append[Tone[a],b]]
ToneSpectrum /: Plus[b_Tone,a_ToneSpectrum] := ToneSpectrum[Append[Tone[a],b]]
Tone /: Plus[a_Tone,b_Tone] := ToneSpectrum[{a,b}]

ToneSpectrum /: Power[a_ToneSpectrum,2] := a*a
Tone /: Power[a_Tone,2] := a*a

ToneSpectrum /: Times[a_ToneSpectrum,b_ToneSpectrum] := ToneSpectrum[Flatten[Tone /@ (a*Tone[b])]]
ToneSpectrum /: Times[a_ToneSpectrum,b_Tone] := ToneSpectrum[Flatten[Tone /@ (Tone[a] * b)]]
ToneSpectrum /: Times[b_Tone,a_ToneSpectrum] := ToneSpectrum[Flatten[Tone /@ (Tone[a] * b)]]
Tone /: Times[a_Tone,b_Tone] :=
  Module[{af,aa,ap,bf,ba,bp},
    {af,{aa,ap}}=Data[Tidy[a]];
    {bf,{ba,bp}}=Data[Tidy[b]];
    ToneSpectrum[{
      Tone[{Abs[af-bf],{ aa*ba/2,ap-bp+Pi/2}}],
      Tone[{Abs[af+bf],{-aa*ba/2,ap+bp+Pi/2}}]
      }]
    ]

complex[a_,p_] := a(Cos[p] + I Sin[p])
Tone /: Complex[x_Tone] := complex[Amplitude[x],Phase[x]]
Tone[{f_,c_?NumberQ}, opts___?OptionQ] := Tone[{f,{Abs[c],Arg[c]}},opts]
Tone[f_, opts___?OptionQ] := Tone[{f,1},opts]

End[]

Protect[
  Frequency,
  Mix
  ];

Protect[
  ToneSpectrum,
  ToneSpectrumQ,
  Tone,
  ToneQ
  ];

EndPackage[]
