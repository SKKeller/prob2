import de.prob.animator.domainobjects.*
import de.prob.statespace.*
import de.prob.check.*

m = api.eventb_load(dir + File.separator + "machines" + File.separator + "InvalidModel" + File.separator +"createErrors.bcm")
s = m as StateSpace

model_check = { job ->
	checker = new ModelChecker(job)
	checker.start()
	checker.getResult()
}

res = model_check(new CBCDeadlockChecker(s))
assert res instanceof CBCDeadlockFound
t_deadlock = res.getTrace(s)
ops = t_deadlock.getTransitionList(true)
assert ops.size() == 1
assert ops[0].getName() == "deadlock_check"

res = model_check(new CBCDeadlockChecker(s, "deadlocked = FALSE" as EventB))
assert res instanceof ModelCheckOk // whoops!!! =)
assert res.message == "No deadlock was found"

res = model_check(new CBCInvariantChecker(s))
assert res instanceof CBCInvariantViolationFound
t_invV = res.getTrace(s)
ops = t_invV.getTransitionList(true)
assert ops.size() == 2
assert ops[0].getName() == "invariant_check_violate_invariant"
assert ops[1].getName() == "violate_invariant"

res = model_check(new CBCInvariantChecker(s,["deadlock"]))
assert res instanceof ModelCheckOk
assert res.message == "No Invariant violation was found"

s.animator.cli.shutdown();
"constraint based deadlock and invariant checking works correctly"