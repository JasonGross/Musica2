(* :Title: Type *)

(* :Summary: Functions for Type *)

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

(* :Context: Musica2`Type` *)

(* :History:
  2004-09-18  bch :  renamed *Sup* to *Container* and *Sub* to *Element*
  2004-09-18  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Type`",
  {
    "Musica2`Common`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  ];

ContainerQ::usage = ""
CreateElement::usage = ""
CreateContainer::usage = ""
Data::usage = ""
DataQ::usage = ""
ElementType::usage = ""
Members::usage = ""
Opts::usage = ""
Pack::usage = ""
Pos::usage = ""
TypeQ::usage = ""
Tidy::usage = ""
UnPack::usage = ""
UnPackOpts::usage = ""

Begin["`Private`"]

CreateElement[T_Symbol, P_] := (
  DeclareElement[T, P];
  Begin["`Private`"];
  DefineElement[T, P];
  End[];
  );

CreateContainer[T_Symbol, ET_Symbol] := (
  DeclareContainer[T, ET];
  Begin["`Private`"];
  DefineContainer[T, ET];
  End[];
  );

M[T_Symbol, P_] := P[[Sequence @@ #]] & /@ (Append[Drop[#, -1], 1] & /@ Position[P, Pattern])

DeclareUsage[T_Symbol,text_String] := MessageName[T,"usage"] = If[StringQ[T::usage],T::usage<>"\[NewLine]\[NewLine]Musica2: ",""]<>text;

DeclareCommon[T_Symbol] := (
  DeclareUsage[T,SymbolName[T]<>", todo: generated usage to be written"];
  DeclareUsage[Symbol[SymbolName[T] <> "Q"],SymbolName[T]<>"Q, todo: generated usage to be written"];
  SetAttributes[T,ReadProtected];
  );

DeclareElement[T_Symbol, P_] := (
  DeclareCommon[T];
  );

DeclareContainer[T_Symbol, ET_Symbol] := (
  DeclareCommon[T];
  );

DefineCommon[T_Symbol] := (
  (* no options *)
  Options[T] = {};

  (* test-function *)
  TypeQ[T] = MatchQ[#, T[_?OptionQ, _?(DataQ[T])]]&;
  Symbol[SymbolName[T] <> "Q"][expr_] := TypeQ[T][expr];

  (* default constructor *)
  T[d_?(DataQ[T]),opts___?OptionQ] := T[{opts},d];

  (* basic format, the precondition (/; TypeQ[T][x]) might go away later, currentli its good for debugging *)
  Format[x_T] := "\[SkeletonIndicator]"<>SymbolName[T]<>"\[SkeletonIndicator]" /; TypeQ[T][x];

  (* the "backdoor" to the info and data *)
  T /: Opts[x_T] := ReplacePart[x,List,{0}][[1]] /; TypeQ[T][x];
  T /: Data[x_T] := ReplacePart[x,List,{0}][[2]] /; TypeQ[T][x];

  (* tidy it up *)
  Tidy[T] = #&;
  Tidy[x_T] := Tidy[T][x];

  T /: ContainerQ[x_T] := ContainerQ[T];
  );

DefineElement[T_Symbol, P_] :=
  Module[{i},
    DataQ[T] = MatchQ[#, P]&;

    (* get all members *)
    Members[T] = M[T,P];

    (* get the index for all members *)
    i = Drop[#, -1] & /@ Cases[Position[P /. {Pattern -> Function[{s, p}, s[Sequence @@ p]]}, _?(! MatchQ[#, List | Blank] &)], {__, 0}];
    MapThread[(Pos[T,#1] = #2)&,{Members[T],i}];

    T /: Part[x_T, s_Symbol] := Data[x][[Sequence @@ Pos[T,s]]];
    T /: ReplacePart[x_T, d_, s_Symbol] := T[ReplacePart[Data[x], d, Pos[T,s]],Sequence @@ Opts[x]];

    Scan[(
      T /: #[x_T] := x[[#]];
      )&,
      Members[T]
      ];

    ContainerQ[T] = False;

    DefineCommon[T];
    ];

DefineContainer[T_Symbol, ET_Symbol] :=
  Module[{t,et},
    Members[T] = Members[ET];
    ElementType[T] = ET;
    T /: ElementType[x_T] := ET;
    DataQ[T] = MatchQ[#, {(_?(DataQ[ET])|{_?OptionQ,_?(DataQ[ET])})...}]&;

    (* outgoing and incoming element's *)
    Pack[T] = Function[{container,element},If[DataQ[ET][element],ET[element],ET[element[[2]], Sequence @@ element[[1]]]]];
    UnPack[T] = Function[{element,opts},If[Opts[element]=={},Data[element],{Opts[element],Data[element]}]];
    UnPackOpts[T] = Function[{elements,opts},opts];

    (* constructors, container from element *)
    T[{y__?(TypeQ[ET])},opts___?OptionQ] := Function[o,T[(UnPack[T][#,o])& /@ {y}, Sequence @@ o]][UnPackOpts[T][{y},{opts}]];
    T[y_?(TypeQ[ET]),opts___?OptionQ] := T[{y}, opts];

    (* constructor, element from container *)
    T /: ET[x_T] := (Pack[T][x,#]& /@ Data[x]);
    For[t = ET,ContainerQ[t]===True,t=et,
      et = ElementType[t];
      T /: et[x_T] := et /@ x;
      ];

    (* get member data *)
    (T /: #[x_T] := # /@ x;)& /@ Members[T];

    (* handy list-manipulation-functions *)
    T /: Append[x_T, y_?(TypeQ[ET])] := T[Append[Data[x],UnPack[T][y,Opts[x]]], Sequence @@ Opts[x]];
    T /: Delete[x_T, n__] := T[Delete[Data[x],n], Sequence @@ Opts[x]];
    T /: Drop[x_T, n_] := T[Drop[Data[x],n], Sequence @@ Opts[x]];
    T /: Extract[x_T, n_Integer] := Part[x,n];
    T /: First[x_T] := Pack[T][x,#]&[First[Data[x]]];
    T /: Insert[x_T, y_?(TypeQ[ET]), n_Integer] := T[Insert[Data[x],UnPack[T][y,Opts[x]],n], Sequence @@ Opts[x]];
    T /: Last[x_T] := Pack[T][x,#]&[Last[Data[x]]];
    T /: Length[x_T] := Length[Data[x]];
    T /: Map[f_, x_T] := Module[{r=Map[f, ET[x]]},If[MatchQ[r,{y__?(TypeQ[ET])}],T[r,Sequence@@Opts[x]],r]];
    T /: MapIndexed[f_, x_T] := Module[{r=MapIndexed[f, ET[x]]},If[MatchQ[r,{y__?(TypeQ[ET])}],T[r,Sequence@@Opts[x]],r]];
    T /: Most[x_T] := T[Most[Data[x]], Sequence @@ Opts[x]];
    T /: Part[x_T, n_Integer] := Pack[T][x,#]&[Part[Data[x],n]] /; n!=0;
    T /: Part[x_T, n_Integer, m__Integer] := Part[x,n][[m]] /; n!=0;
    T /: Prepend[x_T, y_?(TypeQ[ET])] := T[Prepend[Data[x],UnPack[T][y,Opts[x]]], Sequence @@ Opts[x]];
    T /: ReplacePart[x_T, y_?(TypeQ[ET]), n_Integer] := T[ReplacePart[Data[x],UnPack[T][y,Opts[x]],n], Sequence @@ Opts[x]] /; n!=0;
    T /: Rest[x_T] := T[Rest[Data[x]], Sequence @@ Opts[x]];
    T /: Scan[f_, x_T] := Scan[f, ET[x]];
    T /: Select[x_T, f_] := T[Select[ET[x], f], Sequence @@ Opts[x]];
    T /: Take[x_T, n_] := T[Take[Data[x],n], Sequence @@ Opts[x]];

    (* extended list-manipulation-functions *)
    If[ContainerQ[ET],
      T /: MapIndexed[f_, x_T, s_Symbol] := MapIndexed[Function[{y,i},MapIndexed[f[#,Join[i,#2]]&,y,s]],x],
      T /: MapIndexed[f_, x_T, s_Symbol] := MapIndexed[ReplacePart[#,f[#[[s]],#2],s]&,x]
      ];
    If[ContainerQ[ET],
      T /: Map[f_, x_T, s_Symbol] := Map[Map[f,#,s]&,x],
      T /: Map[f_, x_T, s_Symbol] := Map[ReplacePart[#,f[#[[s]]],s]&,x]
      ];
    T /: Part[x_T, s_Symbol] := #[[s]]&/@ x;
    T /: Part[x_T, n_Integer, m___Integer, s_Symbol] := Part[x,n][[m,s]] /; n!=0;


    ContainerQ[T] = True;

    (* and then as element *)
    DefineCommon[T];
  ];

End[]

Protect[
  ];

EndPackage[]
