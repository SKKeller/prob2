c = api.b_load(dir+"/machines/Lift.mch")
assert c != null
s = c.getStatespace()

s.animator.cli.shutdown();
"Machine Lift.mch was loaded"