import java.security.cert.X509Certificate;

import de.prob.model.representation.*

m = api.b_load(dir+File.separator+"machines"+File.separator+"scheduler.mch")
x = ModelRep.translate(m)
assert x.size() == 1
scheduler = x[0]
assert scheduler.label == "scheduler"
assert scheduler.children.size() == 3

(m as StateSpace).animator.cli.shutdown();
"Tests the model representation generated by ModelRep"