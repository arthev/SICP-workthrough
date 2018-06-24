(
 (env) 
 (val) 
 (
  ;;Construct the procedure and skip over the procedure body
  (assign val (op make-compiled-procedure) (label entry2) (reg env)) 
  (goto (label after-lambda1))
  entry2 ;;Calls to factorial will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))
  entry7;;Calls to iter will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  ;;Time for the big test of whether to return or not.
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
  compiled-branch21 ;;This section should be essentially dead weight, since > is primitive...
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch22;;The primitive calculation itself, whoah! Is counter > n?!
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call20 ;;section branches depending on whether val holds true or false after (> counter n).
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch9))
  true-branch10 ;;If it is... just return 'product'. By storing in val. and jumping to (reg continue).
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
  false-branch9 ;(iter (* counter product) (+ counter 1))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env);(+ counter 1)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
  compiled-branch15 
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch16 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call14 
  (assign argl (op list) (reg val))
  (restore env)
  (save argl) ;save the new counter value
  ;;calculate (* counter product)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
  compiled-branch12 
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call11;Have now calculated both new counter and product values, time to call iter again.
  ;;Calculating the values so you always have a partial result is of course the secret behind iteration.
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
  compiled-branch18 
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ;;And now... back to the next iter call!
  primitive-branch19 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call17 
  after-if8 
  after-lambda6 ;;This part defines 'iter' and prepares the call by setting argl to (1 1) before jumping in.
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
  compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ;;This is the place where execution goes into the 'iter' loop.
  primitive-branch5 
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call3 
  after-lambda1 ;;This section defines the 'factorial' variable and returns after defining with 'ok.
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
  ))
