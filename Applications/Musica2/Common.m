(* :Title: Common *)

(* :Summary: Functions for Common *)

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

(* :Context: Musica2`Common` *)

(* :History:
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
  2004-09-03  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Common`",
  {
    "Musica2`Utils`"
    }
  ]

Unprotect[
  ];

Convert::usage = ""
Data::usage = ""
DataQ::usage = ""
DefineSub::usage = ""
DefineSup::usage = ""
Duration::usage = ""
Info::usage = ""
Mix::usage = ""
Pack::usage = ""
Par::usage = ""
Seq::usage = ""
SubType::usage = ""
TypeQ::usage = ""
Tidy::usage = ""
UnPack::usage = ""
UnPackOpts::usage = ""

Begin["`Private`"]

DefineSub[T_Symbol] := (
  (* no options *)
  Options[T] = {};

  (* test-function *)
  TypeQ[T] = MatchQ[#, T[_?OptionQ, _?(DataQ[T])]]&;
  Symbol[SymbolName[T] <> "Q"][expr_] := TypeQ[T][expr];

  (* default constructor *)
  T[d_?(DataQ[T]),opts___?OptionQ] := T[{opts},d];

  (* basic format *)
  Format[x_T] := "\[SkeletonIndicator]"<>SymbolName[T]<>"\[SkeletonIndicator]" /; TypeQ[T][x];

  (* the "backdoor" to the info and data *)
  T /: Info[x_T] := ReplacePart[Tidy[T][x],List,{0}][[1]] /; TypeQ[T][x];
  T /: Data[x_T] := ReplacePart[Tidy[T][x],List,{0}][[2]] /; TypeQ[T][x];

  (* tidy it up (this might go away...) *)
  Tidy[T] = #&;
);

DefineSup[T_Symbol, ST_Symbol] := (
  SubType[T] = ST;
  T /: SubType[x_T] := ST;
  DataQ[T] = MatchQ[#, {(_?(DataQ[ST])|{_?OptionQ,_?(DataQ[ST])})...}]&;

  (* outgoing and incoming sub's *)
  Pack[T] = Function[{sup,sub},If[DataQ[ST][sub],ST[sub],ST[sub[[2]], Sequence @@ sub[[1]]]]];
  UnPack[T] = Function[{sub,opts},If[Info[sub]=={},Data[sub],{Info[sub],Data[sub]}]];
  UnPackOpts[T] = Function[{subs,opts},opts];

  (* constructors, Sup from Sub *)
  T[{y__?(TypeQ[ST])},opts___?OptionQ] := Function[o,T[(UnPack[T][#,o])& /@ {y}, Sequence @@ o]][UnPackOpts[T][{y},{opts}]];
  T[y_?(TypeQ[ST]),opts___?OptionQ] := T[{y}, opts];

  (* constructor, Sub from Sup *)
  T /: ST[x_T] := (Pack[T][Tidy[T][x],#]& /@ Data[x]) /; TypeQ[T][x];

  (* common list-manipulation-functions *)
  T /: Append[x_T, y_?(TypeQ[ST])] := T[Append[Data[x],UnPack[T][y,Info[x]]], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Delete[x_T, n__] := T[Delete[Data[x],n], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Drop[x_T, n_] := T[Drop[Data[x],n], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Extract[x_T, n_Integer] := Part[Tidy[T][x],n] /; TypeQ[T][x];
  T /: First[x_T] := Pack[T][Tidy[T][x],#]&[First[Data[x]]] /; TypeQ[T][x];
  T /: Insert[x_T, y_?(TypeQ[ST]), n_Integer] := T[Insert[Data[x],UnPack[T][y,Info[x]],n], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Last[x_T] := Pack[T][Tidy[T][x],#]&[Last[Data[x]]] /; TypeQ[T][x];
  T /: Length[x_T] := Length[Data[x]] /; TypeQ[T][x];
  T /: Map[f_, x_T] := Module[{r=Map[f, ST[Tidy[T][x]]]},If[MatchQ[r,{y__?(TypeQ[ST])}],T[r,Sequence@@Info[x]],r]] /; TypeQ[T][x];
  T /: MapIndexed[f_, x_T] := Module[{r=MapIndexed[f, ST[Tidy[T][x]]]},If[MatchQ[r,{y__?(TypeQ[ST])}],T[r,Sequence@@Info[x]],r]] /; TypeQ[T][x];
  T /: Most[x_T] := T[Most[Data[x]], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Part[x_T, n_Integer] := Pack[T][Tidy[T][x],#]&[Part[Data[x],n]] /; n!=0 && TypeQ[T][x];
  T /: Prepend[x_T, y_?(TypeQ[ST])] := T[Prepend[Data[x],UnPack[T][y,Info[x]]], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: ReplacePart[x_T, y_?(TypeQ[ST]), n_Integer] := T[ReplacePart[Data[x],UnPack[T][y,Info[x]],n], Sequence @@ Info[x]] /; n!=0 && TypeQ[T][x];
  T /: Rest[x_T] := T[Rest[Data[x]], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Scan[f_, x_T] := Scan[f, ST[Tidy[T][x]]] /; TypeQ[T][x];
  T /: Select[x_T, f_] := T[Select[ST[Tidy[T][x]], f], Sequence @@ Info[x]] /; TypeQ[T][x];
  T /: Take[x_T, n_] := T[Take[Data[x],n], Sequence @@ Info[x]] /; TypeQ[T][x];

  (* and then as sub *)
  DefineSub[T];
  );

End[]

Protect[
  ];

EndPackage[]
