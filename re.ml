open Fa

type buffer= {
  str: string;
  len: int;
  pos: int;
}

let initBuffer str=
  { str;
    len= String.length str;
    pos= 0;
  }

type token=
  | OpC | OpO
  | TkE | TkC of char | TkS of string

type regexp=
  | Ce
  | C of char             (* char *)
  | A of regexp * regexp  (* concatenation *)
  | O of regexp * regexp  (* alternation *)
  | Cl of regexp          (* closure aka Kleene Star *)

let getToken buf=
  let rec paired str count pos=
    if count > 0 then
      match str.[pos] with
      | '(' -> paired str (count+1) (pos+1)
      | ')' -> paired str (count-1) (pos+1)
      | _ -> paired str count (pos+1)
    else pos-1
  in
  match buf.str.[buf.pos] with
  | '('->
    let tail= paired buf.str 1 (buf.pos+1) in
    let paren= String.sub buf.str (buf.pos+1) (tail-buf.pos-1)
    in (TkS paren, {buf with pos= tail+1})
  | '*'-> (OpC, {buf with pos= buf.pos+1})
  | '|'-> (OpO, {buf with pos= buf.pos+1})
  | '#'-> (TkE, {buf with pos= buf.pos+1})
  | '\\' ->
    (match buf.str.[buf.pos+1] with
    | 'x'->
      let c= "0x" ^ (String.sub buf.str (buf.pos+2) 2)
        |> int_of_string
        |> char_of_int
      in (TkC c, {buf with pos= buf.pos+4})
    | c -> (TkC c, {buf with pos= buf.pos+2}))
  | c -> (TkC c, {buf with pos= buf.pos+1})

let str2reg str=
  let rec buf2reg buf=
    let getItem= function
      | TkE-> Ce
      | TkC c-> C c
      | TkS s -> buf2reg @@ initBuffer s
      | other -> raise @@ Failure
          (Printf.sprintf "match %s"
            (match other with OpC -> "OpC" | OpO -> "OpO" | _ -> "Tk"))
    in
    let rec parse curr nextbuf=
      if nextbuf.pos >= nextbuf.len then
        curr
      else
        let nextTk, lastbuf= getToken nextbuf in
        match nextTk with
        | OpO -> O (curr, buf2reg lastbuf)
        | OpC-> let curr= match curr with Cl r-> curr | _ -> Cl curr in
          parse curr lastbuf
        | _ ->
          let next= getItem nextTk in
          if lastbuf.pos >= lastbuf.len then
            A (curr, next)
          else
            let lastTk, tmpbuf= getToken lastbuf in
            match lastTk with
            | OpC-> parse (A (curr, Cl next)) tmpbuf
            | OpO->
              O (A (curr, next), buf2reg tmpbuf)
            | _ -> parse (A (curr, next)) lastbuf
    in
    let curr, buf= getToken buf in
    parse (getItem curr) buf
  in
  initBuffer str |> buf2reg

let rec reg2nfa:regexp->nfa= fun reg->
  match reg with
  | Ce->
    let g= G.create () in
    let head= G.V.create Node.{id= object end; isEnd= false}
    and tail= G.V.create Node.{id= object end; isEnd= false} in
    let e= G.E.create head Edge.E tail in
    G.add_edge_e g e;
    {graph= g; head; tail}
  | C c->
    let g= G.create () in
    let head= G.V.create Node.{id= object end; isEnd= false}
    and tail= G.V.create Node.{id= object end; isEnd= false} in
    let e= G.E.create head (Edge.C c) tail in
    G.add_edge_e g e;
    {graph= g; head; tail}
  | A (r1, r2)->
    let nfa1= reg2nfa r1
    and nfa2= reg2nfa r2
    in
    let g= G.merge nfa1.graph nfa2.graph in
    G.iter_succ_e
      (fun e->
        let l= G.E.label e
        and dst= G.E.dst e
        in
        (G.add_edge_e g (G.E.create nfa1.tail l dst)))
      nfa2.graph
      nfa2.head;
    G.remove_vertex g nfa2.head;
    { graph= g;
      head= nfa1.head;
      tail= nfa2.tail;}
  | O (r1, r2)->
    let nfa1= reg2nfa r1
    and nfa2= reg2nfa r2
    in
    let g= G.merge nfa1.graph nfa2.graph
    and head= G.V.create Node.{id= object end; isEnd= false}
    and tail= G.V.create Node.{id= object end; isEnd= false} in
    (G.add_edge_e g (G.E.create head Edge.E nfa1.head));
    (G.add_edge_e g (G.E.create head Edge.E nfa2.head));
    (G.add_edge_e g (G.E.create nfa1.tail Edge.E tail));
    (G.add_edge_e g (G.E.create nfa2.tail Edge.E tail));
    { graph= g;
      head;
      tail;}
  | Cl reg->
    let nfa= reg2nfa reg
    and head= G.V.create Node.{id= object end; isEnd= false}
    and tail= G.V.create Node.{id= object end; isEnd= false}
    in
    (G.add_edge_e nfa.graph (G.E.create head Edge.E nfa.head));
    (G.add_edge_e nfa.graph (G.E.create head Edge.E tail));
    (G.add_edge_e nfa.graph (G.E.create nfa.tail Edge.E tail));
    (G.add_edge_e nfa.graph (G.E.create nfa.tail Edge.E nfa.head));
    { graph= nfa.graph;
      head;
      tail;}

let nfa2dfa (nfa:nfa)=
  let nfaG= nfa.graph
  and dfaG= G.create ()
  and ifEnd= NodeSet.mem nfa.tail in
  let rec nfa2dfa transTbl nodes=
    if NodeSetMap.mem nodes transTbl then
      (NodeSetMap.find nodes transTbl, transTbl)
    else
      let curr= Node.{id= object end; isEnd= ifEnd nodes} in
      let transTbl= NodeSetMap.add nodes curr transTbl
      and labels= labels nfaG nodes in
      let combTransTbl= CharSet.fold
        (fun c s->
          let next_nfaNodes= move nfaG nodes (Edge.C c) in
          let (next, nextTransTbl)= nfa2dfa s next_nfaNodes in
          G.add_edge_e dfaG (G.E.create curr (Edge.C c) next);
          NodeSetMap.merge
            (fun k v1 v2->
              match v1 with
              | Some _-> v1
              | None -> v2)
            s
            nextTransTbl)
        labels
        transTbl
      in
      (curr, combTransTbl)
  in
  let start= closureE nfaG nfa.head in
  let head,_= nfa2dfa NodeSetMap.empty start in
  { graph= dfaG;
    head; }

