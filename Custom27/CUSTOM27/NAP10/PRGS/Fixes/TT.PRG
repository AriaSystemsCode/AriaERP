select invpos
scan for abs(tot-bompos.tot) <> 0
  select testcst
  appen blank
  repl po with invpos.ctktno, type with "1", bocstdif with 1.99
  appen blank
  repl po with invpos.ctktno, type with "2", bocstdif with 1.99
  appen blank
  repl po with invpos.ctktno, type with "3", bocstdif with 1.99
  appen blank
  repl po with invpos.ctktno, type with "4", bocstdif with 1.99
  
endscan  