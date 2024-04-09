select poh
scan for status="S" and abs(nact_cost5-pol.nact_cst5) > 0.9
  select posln
  repl rest nAct_cst5 with nAct_cst5 + ((poh.nAct_cost5-pol.nAct_cst5)/poshdr.receive) while cstytype+po+style+STR(lineno,6)+trancd="P"+Poh.po FOR trancd="2"
endscan