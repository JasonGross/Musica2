(* :Title: ObjectType *)

(* :Summary: Functions for ObjectType *)

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

(* :Context: Musica2`ObjectType` *)

(* :History:
  2005-01-28  bch :  OrderedQ now does N before comparison
  2005-01-27  bch :  moved the content from Type.m to here
                     renamed Type to ObjectType, Types to ObjectTypes and TypeQ to ObjectTypeQ
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`ObjectType`",
  {
    "Musica2`Common`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  ];

CreateContainer::usage = "CreateContainer is a function used to create all object-types that are containers in Musica2."
CreateElement::usage = "CreateContainer is a function used to create all object-types that are elements in Musica2."
ContainerQ::usage = "todo"
Data::usage = "todo"
DataToRules::usage = "todo"
DataQ::usage = "todo"
ElementType::usage = "todo"
(* EmptyQ::usage = "todo" *)
Members::usage = "todo"
ObjectType::usage = "todo"
ObjectTypes::usage = "todo"
ObjectTypeQ::usage = "todo"
Opts::usage = "todo"
Pack::usage = "todo"
Pos::usage = "todo"
Struct::usage = "todo"
Tidy::usage = "todo"
UnPack::usage = "todo"
UnPackOpts::usage = "todo"

Begin["`Private`"]

CreateElement[T_Symbol, P_, D_, U_String] := CreateElement[T, P, D, U, {}]
CreateElement[T_Symbol, P_, D_, U_String, K_] := (
  DeclareElement[T, P, D, U, K];
  Begin["`Private`"];
  DefineElement[T, P, D, K];
  End[];
  );

CreateContainer[T_Symbol, ET_Symbol, U_String] := (
  DeclareContainer[T, ET, U];
  Begin["`Private`"];
  DefineContainer[T, ET];
  End[];
  );

M[T_Symbol, P_] := P[[Sequence @@ #]] & /@ (Append[Drop[#, -1], 1] & /@ Position[P, Pattern])

(* we dont want to mess with the already present usage-text for Sound *)
DeclareUsage[T_Symbol,text_String] := MessageName[T,"usage"] = If[StringQ[T::usage],T::usage<>"\[NewLine]\[NewLine]Musica2: ",""]<>text;

DeclareCommon[T_Symbol, U_String] := (
  DeclareUsage[T,
    U<>"\[NewLine]\[NewLine]"<>
    SymbolName[T]<>" is a generated object-type (see ObjectType.m).\[NewLine]"<>
    "An object of type "<>SymbolName[T]<>" has Head "<>SymbolName[T]<>" and two parts; opts and data.\[NewLine]"<>
    "The opts part must make OptionQ[opts] return True.\[NewLine]"<>
    "The data part must make DataQ["<>SymbolName[T]<>"][data] return True.\[NewLine]"<>
    "\[NewLine]"
  ];
  DeclareUsage[Symbol[SymbolName[T] <> "Q"],SymbolName[T]<>"Q[expr] returns True if expr is an object of type "<>SymbolName[T]<>"\[NewLine]"];
  SetAttributes[T,ReadProtected];
  );

DeclareElement[T_Symbol, P_, D_, U_String, K_] := (
  DeclareCommon[T,U];
  );

DeclareContainer[T_Symbol, ET_Symbol, U_String] := (
  DeclareCommon[T,U];
  );

(* low level stuff, "backdoors" for setting opts and data (dont intercept the first two) *)
Data[x_, d_, pos_] := x[[0]][ReplacePart[Data[x], d, pos],Sequence @@ Opts[x]];
Opts[x_, d_, s_, False] := x[[0]][Data[x], Sequence @@ AddOpts[Opts[x],s->d]];
Data[x_, d_, s_, pos_] := Data[x,d,pos];
Opts[x_, d_, s_] := Opts[x, d, s, False];

DefineCommon[T_Symbol] := (
  ObjectTypes = Prepend[ObjectTypes,T];

  (* no options, but preparing a placeholder *)
  Options[T] = {};

  (* test-function *)
  ObjectTypeQ[T] = MatchQ[#, T[_?OptionQ, _?(DataQ[T])]]&;

  Symbol[SymbolName[T] <> "Q"][expr_] := ObjectTypeQ[T][expr];

  (* default constructor *)
  T[d_?(DataQ[T]),opts___?OptionQ] := T[{opts},d];

  (* basic format, the precondition (/; ObjectTypeQ[T][x]) might go away later, currentli its good for debugging *)
  Format[x_T] := "\[SkeletonIndicator]"<>SymbolName[T]<>"\[SkeletonIndicator]" /; ObjectTypeQ[T][x];

  (* the "backdoor" to the opts and data *)
  T /: Opts[x_T] := ReplacePart[x,List,{0}][[1]] /; ObjectTypeQ[T][x];
  T /: Data[x_T] := ReplacePart[x,List,{0}][[2]] /; ObjectTypeQ[T][x];

  (* tidy it up *)
  Tidy[T] = #&;
  Tidy[x_T] := Tidy[T][x];

  T /: ContainerQ[x_T] := ContainerQ[T];

  T[x_T, o___?OptionQ] :=
    Module[{r = x},
      Scan[(r = If[MemberQ[Members[T],#[[1]]],ReplacePart[r,#[[2]],#[[1]]],Opts[r,#[[2]],#[[1]]]])&,{o}];
      r
      ];
  T[x:{__T}, o__?OptionQ] := T[#,o]& /@ x;
  
  T /: TestSuite[T] := {};

  MessageName[T,"usage"] = T::usage <> "ObjectTypeQ["<>SymbolName[T]<>"] returns a function that returns True if the argument is of type "<>SymbolName[T]<>" by checking both Head and data.\[NewLine]";
  MessageName[T,"usage"] = T::usage <> "Opts[x_"<>SymbolName[T]<>"] returns the opts part of an object of type "<>SymbolName[T]<>".\[NewLine]";
  MessageName[T,"usage"] = T::usage <> "Data[x_"<>SymbolName[T]<>"] returns the data part of an object of type "<>SymbolName[T]<>".\[NewLine]";
  MessageName[T,"usage"] = T::usage <> "DataToRules[x_"<>SymbolName[T]<>"] returns the data part of an object of type "<>SymbolName[T]<>" as a list of Rules.\[NewLine]";
  MessageName[T,"usage"] = T::usage <> "Tidy[x_"<>SymbolName[T]<>"] returns x in a tidy shape. It might not do anything though...\[NewLine]";
  MessageName[T,"usage"] = T::usage <> "ContainerQ[x_"<>SymbolName[T]<>"] returns ContainerQ["<>SymbolName[T]<>"].\[NewLine]";
  );

DefineElement[T_Symbol, P_, D_, K_] :=
  Module[{i,k=Union[K,{List,Blank,BlankSequence,RepeatedNull,Repeated}],m},
(*
    Print["BEGIN: ",T];
*)
    DataQ[T] = MatchQ[#, P]&;

    Struct[T] = P;

    (* get all members *)
    Members[T] = M[T,P];
    Members[x_T] = Members[T];

    (* get the index for all members *)
    m = P /. {Pattern -> Function[{s, p}, s[Sequence @@ p]]};
    i = Drop[#, -1] & /@ Cases[Position[m, _?(! MatchQ[#, Alternatives @@ k] &)], {__, 0}];
    MapThread[(Pos[T,#1] = #2)&,{Members[T],i}];

(*
    Print["M ",Members[T]];
    Print["m ",m];
    Print["i ",i];
    Print["p ",m[[Sequence@@#,0]]&/@i];
*)

    T /: Part[x_T, s_Symbol] := Data[x][[Sequence @@ Pos[T,s]]];
    T /: Part[x_T, s_Symbol, n__Integer] := Part[x,s][[n]];
    T /: ReplacePart[x_T, d_, s_Symbol] := Data[x, d, s, Pos[T,s]];

    Scan[(
      T /: #[x_T] := x[[#]];
      )&,
      Members[T]
      ];

    ContainerQ[T] = False;

    T /: DataToRules[x_T] := (# -> #[x]) & /@ Members[T];

    T /: OrderedQ[x1_T, x2_T, s : {__}] :=
      Module[{f = s, y},
        While[0 < Length[f],
          y = f[[1]];
          If[OrderedQ[{y[x1], y[x2]}//N],
            If[y[x1] =!= y[x2],
              Return[True]
              ],
            Return[False]
            ];
          (*If[! OrderedQ[{y[x1], y[x2]}], Return[False]];*)
          f = Drop[f, 1];
          ];
        True
        ];

    MessageName[T,"usage"] = T::usage <> SymbolName[T]<>" is an element (not a container).\[NewLine]";
    MessageName[T,"usage"] = T::usage <> SymbolName[T]<>"[d_?(DataQ["<>SymbolName[T]<>"]),opts___?OptionQ] is the standard constructor and returns an object of type "<>SymbolName[T]<>".\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "DataQ["<>SymbolName[T]<>"] returns a function that returns True if the argument is valid data for "<>SymbolName[T]<>".\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Struct["<>SymbolName[T]<>"] returns the structure of the data of "<>SymbolName[T]<>" as "<>ToString[Struct[T]]<>", and is used by DataQ["<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Members["<>SymbolName[T]<>"] returns the names of the members of "<>SymbolName[T]<>" as a List of symbols: "<>ToString[Members[T]]<>".\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Members[x_"<>SymbolName[T]<>"] returns Members["<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "\[NewLine]";

    DefineCommon[T];

    If[D =!= Null,
      Module[{d=T[D]},
        T[] := T[D];
        Options[T] = Join[Options[T],(#->#[d])& /@ Members[T]];
        ];
      ];
    
    T /: TestSuite[T] = Join[TestSuite[T],{
      TestCase[ContainerQ[T],False],
      TestCase[Struct[T],P],
      TestCase[DataQ[T][Data[T[]]],True],
      TestCase[Opts[T[Data[T[]], test->42]],{test->42}],
      TestCase[Data[T[Data[T[]], test->42]],Data[T[]]]
      }];
    
    MessageName[T,"usage"] = T::usage <> "ContainerQ["<>SymbolName[T]<>"] returns False.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Part[x_"<>SymbolName[T]<>", s_Symbol] where s is a member of "<>ToString[Members[T]]<>" returns the value of member s in x.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Part[x_"<>SymbolName[T]<>", s_Symbol, n_Integer] where s is a member of "<>ToString[Members[T]]<>" returns the value of the n:th part of member s in x.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "ReplacePart[x_"<>SymbolName[T]<>", d_, s_Symbol] where s is a member of "<>ToString[Members[T]]<>" returns x with the value of member s replaced by d.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "s[x_"<>SymbolName[T]<>"] where s is a member of "<>ToString[Members[T]]<>" returns the value of that member in x.\[NewLine]";
(*
    Print["END: ",T];
*)
    ];

DefineContainer[T_Symbol, ET_Symbol] :=
  Module[{t,et},
    Members[T] = Members[ET];
    ElementType[T] = ET;
    T /: ElementType[x_T] := ET;
    DataQ[T] = MatchQ[#, {(_?(DataQ[ET])|{_?OptionQ,_?(DataQ[ET])})...}]&;

    (* outgoing and incoming element's *)
    Pack[T] = Function[{container,element},If[MatchQ[element,{{__?OptionQ},_?(DataQ[ET])}],ET[element[[2]], Sequence @@ element[[1]]],ET[element]]];
    UnPack[T] = Function[{element,opts},If[Opts[element]==={},Data[element],{Opts[element],Data[element]}]];
    UnPackOpts[T] = Function[{elements,opts},opts];

    (* constructors, container from element *)
    T[{y__?(ObjectTypeQ[ET])},opts___?OptionQ] := Function[o,T[(UnPack[T][#,o])& /@ {y}, Sequence @@ o]][UnPackOpts[T][{y},{opts}]];
    T[y_?(ObjectTypeQ[ET]),opts___?OptionQ] := T[{y}, opts];

    (* constructor, element from container *)
    T /: ET[x_T] := (Pack[T][x,#]& /@ Data[x]);
    For[t = ET,ContainerQ[t]===True,t=et,
      et = ElementType[t];
      T /: et[x_T] := et /@ x;
      ];

    (* get member data *)
    Scan[(T /: #[x_T] := # /@ ET[x])&,Members[T]];

    T /: DataToRules[x_T] := (# -> #[x]) & /@ Members[T];

    (* T /: EmptyQ[x_T] := Length[x]==0; *)

    (* handy list-manipulation-functions *)
    T /: Append[x_T, y_ET] := T[Append[ET[x],y], Sequence @@ Opts[x]];
    T /: Delete[x_T, n__] := T[Delete[Data[x],n], Sequence @@ Opts[x]];
    T /: Drop[x_T, n_] := T[Drop[Data[x],n], Sequence @@ Opts[x]];
    T /: Extract[x_T, n_Integer] := Part[x,n];
    T /: First[x_T] := Part[x,1];
    T /: Insert[x_T, y_ET, n_Integer] := T[Insert[ET[x],y,n], Sequence @@ Opts[x]];
    T /: Last[x_T] := Part[x,-1];
    T /: Length[x_T] := Length[Data[x]];
    T /: Map[f_, x_T] := Module[{r=Map[f, ET[x]]},If[MatchQ[r,{___ET}],T[r,Sequence@@Opts[x]],r]];
    T /: MapIndexed[f_, x_T] := Module[{r=MapIndexed[f, ET[x]]},If[MatchQ[r,{___ET}],T[r,Sequence@@Opts[x]],r]];
    T /: Most[x_T] := T[Most[Data[x]], Sequence @@ Opts[x]];
    T /: Part[x_T, n_Integer] := Pack[T][x,#]&[Part[Data[x],n]] /; n!=0;
    T /: Part[x_T, n_Integer, m__Integer] := Part[x,n][[m]] /; n!=0;
    T /: Prepend[x_T, y_ET] := T[Prepend[ET[x],y], Sequence @@ Opts[x]];
    T /: ReplacePart[x_T, y_ET, n_Integer] := T[ReplacePart[ET[x],y,n], Sequence @@ Opts[x]] /; n!=0;
    T /: Reverse[x_T] := T[Reverse[ET[x]], Sequence @@ Opts[x]];
    T /: Rest[x_T] := T[Rest[Data[x]], Sequence @@ Opts[x]];
    T /: Scan[f_, x_T] := Scan[f, ET[x]];
    T /: Select[x_T, f_] := T[Select[ET[x], f], Sequence @@ Opts[x]];
    T /: Take[x_T, n_] := T[Take[Data[x],n], Sequence @@ Opts[x]];


    (* extended list-manipulation-functions *)
    If[ContainerQ[ET],
      T /: Map[f_, x_T, s_Symbol] := Map[Map[f,#,s]&,x],
      T /: Map[f_, x_T, s_Symbol] := Map[ReplacePart[#,f[#[[s]]],s]&,x]
      ];
    If[ContainerQ[ET],
      T /: MapIndexed[f_, x_T, s_Symbol] := MapIndexed[Function[{y,i},MapIndexed[f[#,Join[i,#2]]&,y,s]],x],
      T /: MapIndexed[f_, x_T, s_Symbol] := MapIndexed[ReplacePart[#,f[#[[s]],#2],s]&,x]
      ];
    If[ContainerQ[ET],
      T /: Sort[x_T, s:{__Symbol}] := Sort[#,s]& /@ x;
      T /: Sort[x_T, s_Symbol] := Sort[#,s]& /@ x,
      T /: Sort[x_T, s:{__Symbol}] := T[Sort[ET[x],OrderedQ[#1,#2,s]&], Sequence @@ Opts[x]];
      T /: Sort[x_T, s_Symbol] := T[Sort[ET[x],OrderedQ[#1, #2,{s}]&], Sequence @@ Opts[x]]
      ];

    T /: ReplacePart[x_T, y_, s_Symbol] :=
      If[ListQ[y] && Length[y]==Length[x],
        MapIndexed[ReplacePart[#,y[[#2[[1]]]],s]&,x],
        Map[ReplacePart[#,y,s]&,x]
        ];
    T /: ReplacePart[x_T, y_, n__Integer, s_Symbol] :=
      ReplacePart[x,ReplacePart[x[[{n}[[1]]]],y,Sequence@@Drop[{n},1],s],{n}[[1]]];
    
    T /: Part[x_T, s_Symbol] := #[[s]]&/@ x;
    T /: Part[x_T, n_Integer, m___Integer, s_Symbol] := Part[x,n][[m,s]] /; n!=0;

    ContainerQ[T] = True;

    MessageName[T,"usage"] = T::usage <> SymbolName[T]<>" is a container of "<>SymbolName[ET]<>"'s.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> SymbolName[T]<>"[{y__"<>SymbolName[ET]<>"},opts___?OptionQ] is the standard constructor and returns an object of type "<>SymbolName[T]<>" containing the "<>SymbolName[ET]<>"'s {y}.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> SymbolName[T]<>"[y_"<>SymbolName[ET]<>",opts___?OptionQ] is another constructor and returns an object of type "<>SymbolName[T]<>" containing the "<>SymbolName[ET]<>" y.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "The data part of a "<>SymbolName[T]<>" is thus a list of the data parts of "<>SymbolName[ET]<>"'s, and their opts if any.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "DataQ["<>SymbolName[T]<>"] returns a function that returns True if the argument is a List of data for "<>SymbolName[ET]<>"'s.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Members["<>SymbolName[T]<>"] returns the names of the members of the elements of "<>SymbolName[T]<>" as a List of symbols: "<>ToString[Members[T]]<>".\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "ElementType["<>SymbolName[T]<>"] returns "<>SymbolName[ET]<>".\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "ElementType[x_"<>SymbolName[T]<>"] returns "<>SymbolName[ET]<>".\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "\[NewLine]";
    MessageName[T,"usage"] = T::usage <> SymbolName[ET]<>"[x_"<>SymbolName[T]<>"] returns a List of "<>SymbolName[ET]<>"'s contained in x.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "s[x_"<>SymbolName[T]<>"], where s is a member of "<>ToString[Members[T]]<>", returns a list of the values of the member s in the elements of x.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "\[NewLine]";

    DefineCommon[T];

    T /: TestSuite[T] = Join[TestSuite[T],{
      TestCase[ContainerQ[T],True],
      TestCase[ElementType[T],ET]
      }];
    
    Tidy[T] = (Tidy /@ #)&;

    MessageName[T,"usage"] = T::usage <> "ContainerQ["<>SymbolName[T]<>"] returns True.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "List-like functions for "<>SymbolName[T]<>"'s are:\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Append[x_"<>SymbolName[T]<>", y_"<>SymbolName[ET]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Delete[x_"<>SymbolName[T]<>", n__].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Drop[x_"<>SymbolName[T]<>", n_].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Extract[x_"<>SymbolName[T]<>", n_Integer].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "First[x_"<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Insert[x_"<>SymbolName[T]<>", y_"<>SymbolName[ET]<>", n_Integer].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Last[x_"<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Length[x_"<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Map[f_, x_"<>SymbolName[T]<>"] where f takes a "<>SymbolName[ET]<>" as argument.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "MapIndexed[f_, x_"<>SymbolName[T]<>"] where f takes a "<>SymbolName[ET]<>" as the first argument.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Most[x_"<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Part[x_"<>SymbolName[T]<>", n_Integer].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Part[x_"<>SymbolName[T]<>", n_Integer, m__Integer].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Prepend[x_"<>SymbolName[T]<>", y_"<>SymbolName[ET]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "ReplacePart[x_"<>SymbolName[T]<>", y_"<>SymbolName[ET]<>", n_Integer].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Reverse[x_"<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Rest[x_"<>SymbolName[T]<>"].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Scan[f_, x_"<>SymbolName[T]<>"] where f takes a "<>SymbolName[ET]<>" as argument.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Select[x_"<>SymbolName[T]<>", f_] where f takes a "<>SymbolName[ET]<>" as argument.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Take[x_"<>SymbolName[T]<>", n_].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Extended List-like functions for "<>SymbolName[T]<>"'s are:\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Map[f_, x_"<>SymbolName[T]<>", s_Symbol] where f takes member s as argument.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "MapIndexed[f_, x_"<>SymbolName[T]<>", s_Symbol] where f takes member s as the first argument.\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Sort[x_"<>SymbolName[T]<>", s_Symbol].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Sort[x_"<>SymbolName[T]<>", s:{__Symbol}].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Part[x_"<>SymbolName[T]<>", s_Symbol].\[NewLine]";
    MessageName[T,"usage"] = T::usage <> "Part[x_"<>SymbolName[T]<>", n_Integer, m___Integer, s_Symbol].\[NewLine]";
  ];

ObjectTypes = {}

ObjectType /: TestSuite[ObjectType] := TestSuite /@ ObjectTypes

End[]

Protect[
  ];

EndPackage[]