(
 (env)
 (val)
 (
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
  entry2 
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (save continue)
  (save proc)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
  compiled-branch4 
  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch5 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call3 
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
  compiled-branch7 
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch8 (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call6 
  (restore argl) 
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
  compiled-branch10 
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch11 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call9
  after-lambda1
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
  )
 )
