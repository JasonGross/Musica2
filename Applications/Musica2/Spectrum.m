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
  2004-10-07  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Spectrum`",
  {
    "Musica2`Common`",
    "Musica2`Sound`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Frequency,
  Mix
  ];

Unprotect[
  Spectrum,
  SpectrumQ,
  Tone,
  ToneQ
  ];

CreateElement[Tone, {Frequency_, {Amplitude_, Phase_}},"todo\[NewLine]"];
CreateContainer[Spectrum,Tone,"todo\[NewLine]"];

Begin["`Private`"]

Mix[x:{__Spectrum}, opts___?OptionQ] := Spectrum[Flatten[Tone /@ x]]

Spectrum /: Snippet[x_Spectrum, opts___?OptionQ] := (* todo: an option to use IFFT *)
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

Spectrum[x_Snippet, opts___?OptionQ] :=
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
    r = Spectrum[MapIndexed[Tone[{sr #2[[1]]/sc, #1}]&, d], opts, SampleRate->sr, SampleCount->sc];
    r
    ]

Spectrum[x:{__?AtomQ}, opts___?OptionQ] := Spectrum[Tone /@ x,opts]

Tidy[Spectrum] = Module[{r = #,i,c},
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
  r = Spectrum[r, Sequence @@ Opts[#]];
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

Spectrum /: Plus[a_Spectrum,b_Spectrum] := Spectrum[Join[Tone[a],Tone[b]]]
Spectrum /: Plus[a_Spectrum,b_Tone] := Spectrum[Append[Tone[a],b]]
Spectrum /: Plus[b_Tone,a_Spectrum] := Spectrum[Append[Tone[a],b]]
Tone /: Plus[a_Tone,b_Tone] := Spectrum[{a,b}]

Spectrum /: Power[a_Spectrum,2] := a*a
Tone /: Power[a_Tone,2] := a*a

Spectrum /: Times[a_Spectrum,b_Spectrum] := Spectrum[Flatten[Tone /@ (a*Tone[b])]]
Spectrum /: Times[a_Spectrum,b_Tone] := Spectrum[Flatten[Tone /@ (Tone[a] * b)]]
Spectrum /: Times[b_Tone,a_Spectrum] := Spectrum[Flatten[Tone /@ (Tone[a] * b)]]
Tone /: Times[a_Tone,b_Tone] :=
  Module[{af,aa,ap,bf,ba,bp},
    {af,{aa,ap}}=Data[Tidy[a]];
    {bf,{ba,bp}}=Data[Tidy[b]];
    Spectrum[{
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
  Spectrum,
  SpectrumQ,
  Tone,
  ToneQ
  ];

EndPackage[]
