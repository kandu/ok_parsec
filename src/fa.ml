open Printf
open Graph

let epsilon= "ϵ"

module CharSet = Set.Make(Char)

module Node =
  struct
    type t= {id: < >; mutable isEnd: bool}
    let compare a b= compare (Oo.id a.id) (Oo.id b.id)
    let hash= Hashtbl.hash
    let equal= (=)
    let id t= Oo.id t.id
    let isEnd t= t.isEnd
  end

module NodeSet = Set.Make(Node)
module NodeMap = Map.Make(Node)

module NodeSetMap = Map.Make(NodeSet)

module Edge =
  struct
    type t=
      | E
      | C of char
    let compare= compare
    let hash= Hashtbl.hash
    let equal= (=)
    let string_of_t= function
      | E-> epsilon
      | C c-> String.make 1 c
    let default= E
  end

module G = struct
  include Imperative.Digraph.ConcreteLabeled(Node)(Edge)
  let mergeFrom g1 g2= iter_edges_e (add_edge_e g2) g1
  let merge g1 g2=
    let g= create () in
    iter_edges_e (add_edge_e g) g1;
    iter_edges_e (add_edge_e g) g2;
    g
end

(*module Dot = Graphviz.Dot(
  struct
    include G
    let vertex_name v = string_of_int @@ Oo.id (V.label v)
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape `Circle]
    let vertex_attributes v=
      if Node.getEnd v
      then [`Shape `Doublecircle]
      else []
    let default_edge_attributes _ = []
    let edge_attributes e = [`Label (Edge.string_of_t (E.label e))]
    let get_subgraph _ = None
  end)*)

type nfa= {
  graph: G.t;
  mutable head: Node.t;
  mutable tail: Node.t;
}

type dfa= {
  graph: G.t;
  mutable head: Node.t;
}

let genDot_n (fa:nfa)=
  let open Node in
  "digraph G {\n  node [shape=circle]\n"
  ^ (sprintf "  %d\n"
    (Oo.id (G.V.label fa.head).id))
  ^ (sprintf "  %d [shape=doublecircle]\n"
    (Oo.id (G.V.label fa.tail).id))
  ^ (G.fold_edges_e
    (fun e s-> s ^ (
      sprintf "  %d -> %d [label=\"%s\"]\n"
        (Oo.id (G.V.label (G.E.src e)).id)
        (Oo.id (G.V.label (G.E.dst e)).id)
        (Edge.string_of_t (G.E.label e))
      ))
    fa.graph
    "")
  ^ "}\n"

let genDot_d (fa:dfa)=
  let open Node in
  "digraph G {\n  node [shape=circle]\n"
  ^ (sprintf "  %d [%s]\n"
    (Oo.id (G.V.label fa.head).id)
    (if fa.head.isEnd then "shape=doublecircle" else ""))
  ^ (G.fold_edges_e
    (fun e s-> s ^ (
      sprintf "  %d -> %d [label=\"%s\"]\n"
        (Oo.id (G.V.label (G.E.src e)).id)
        (Oo.id (G.V.label (G.E.dst e)).id)
        (Edge.string_of_t (G.E.label e)))
      ^ (
        if (G.V.label (G.E.dst e)).isEnd then
          sprintf "%d [shape=doublecircle]" (Oo.id (G.V.label (G.E.dst e)).id)
        else "")
      )
    fa.graph
    "")
  ^ "}\n"

module CharMap = Map.Make(Char)

type sm=
  { id: < >;
    isEnd: bool;
    mutable next: sm CharMap.t;
  }

module SmSet = Set.Make(
  struct
    type t= sm
    let compare a b= compare (Oo.id a.id) (Oo.id b.id)
  end)

let dot_output_n g f =
  let oc = open_out f
  and dot= genDot_n g in
  output_string oc dot;
  close_out oc

let dot_output_d g f =
  let oc = open_out f
  and dot= genDot_d g in
  output_string oc dot;
  close_out oc

let genDot_sm (sm:sm)=
  let rec gen node traversed=
    if SmSet.mem node traversed then
      ("", traversed)
    else
      let traversed= SmSet.add node traversed in
      let (dot, set)=
        (CharMap.fold
          (fun c next (s, traversed)->
            let (dot, traversed)= (gen next traversed) in
            let s=
              (Printf.sprintf "%d -> %d [label=\"%c\"]"
                (Oo.id node.id)
                (Oo.id next.id)
                c)
              ^ dot
              ^ s
            in
            (s, traversed))
          node.next
          ("", traversed)
        )
      in
      ( (Printf.sprintf "  %d [%s]\n"
          (Oo.id node.id)
          (if node.isEnd then "shape=doublecircle" else ""))
        ^ dot
        , set)
  in
  let (dot, _)= gen sm SmSet.empty in
  "digraph G {\n  node [shape=circle]\n"
  ^ dot
  ^ "}\n"

let dot_output_sm g f =
  let oc = open_out f
  and dot= genDot_sm g in
  output_string oc dot;
  close_out oc

let display_svg_n g=
  let tmp_dot= Filename.temp_file "graph" ".dot" in
  let tmp_svg= Filename.temp_file "fa" ".svg" in
  dot_output_n g tmp_dot;
  sprintf "dot -Tsvg -o%s %s; xdg-open %s"
    tmp_svg tmp_dot tmp_svg
    |> Sys.command
    |> ignore

let display_svg_d g=
  let tmp_dot= Filename.temp_file "graph" ".dot" in
  let tmp_svg= Filename.temp_file "fa" ".svg" in
  dot_output_d g tmp_dot;
  sprintf "dot -Tsvg -o%s %s; xdg-open %s"
    tmp_svg tmp_dot tmp_svg
    |> Sys.command
    |> ignore

let display_svg_sm g=
  let tmp_dot= Filename.temp_file "graph" ".dot" in
  let tmp_svg= Filename.temp_file "fa" ".svg" in
  dot_output_sm g tmp_dot;
  sprintf "dot -Tsvg -o%s %s; xdg-open %s"
    tmp_svg tmp_dot tmp_svg
    |> Sys.command
    |> ignore

let closureE graph node=
  let rec closureE node traversed=
    let traversed= NodeSet.add node traversed in
    let suss=
      List.filter
        (fun e->
          G.E.label e = Edge.E && not (NodeSet.mem (G.E.dst e) traversed))
        (G.succ_e graph node)
      |> List.map G.E.dst
    in
    let traversed= NodeSet.union traversed (NodeSet.of_list suss) in
    List.fold_left
      (fun traversed node->
        NodeSet.union traversed (closureE node traversed))
      traversed
      suss
  in
  closureE node NodeSet.empty

let closuresE graph nodes=
  NodeSet.fold
    (fun node set-> NodeSet.union set (closureE graph node))
    nodes
    NodeSet.empty

let move graph nodes l=
  closuresE graph
    (NodeSet.fold
      (fun node set->
        NodeSet.union
          set
          (G.succ_e graph node
            |> List.filter (fun e-> G.E.label e = l)
            |> List.map G.E.dst
            |> NodeSet.of_list))
      (closuresE graph nodes)
      NodeSet.empty)

let labels graph nodes=
  let nodes= closuresE graph nodes in
  NodeSet.fold
    (fun node set->
      CharSet.union
        set
        (List.fold_left
          (fun cs e->
            let open Edge in
            match G.E.label e with
            | E-> cs
            | C c-> CharSet.add c cs)
          CharSet.empty
          (G.succ_e graph node))
      )
    nodes
    CharSet.empty

