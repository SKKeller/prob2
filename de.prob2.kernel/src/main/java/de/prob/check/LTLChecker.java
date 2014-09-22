package de.prob.check;

import de.prob.animator.command.LTLCheckingJob;
import de.prob.animator.domainobjects.LTL;
import de.prob.exception.ProBError;
import de.prob.statespace.StateSpace;
import de.prob.web.views.ModelCheckingUI;

public class LTLChecker implements IModelCheckJob {

	private final StateSpace s;
	private final ModelCheckingUI ui;
	private final String jobId;
	private final LTLCheckingJob job;
	private final LTL formula;

	public LTLChecker(final StateSpace s, final LTL formula) {
		this(s, formula, null);
	}

	public LTLChecker(final StateSpace s, final LTL formula,
			final ModelCheckingUI ui) {
		if (formula == null) {
			throw new IllegalArgumentException(
					"Cannot perform LTL checking without a correctly parsed LTL Formula");
		}
		this.s = s;
		this.formula = formula;
		this.ui = ui;
		this.jobId = ModelChecker.generateJobId();
		job = new LTLCheckingJob(s, formula, jobId, ui);
	}

	@Override
	public IModelCheckingResult call() throws Exception {
		long time = System.currentTimeMillis();
		try {
			s.execute(job);
		} catch (ProBError e) {
			// If a type error occured, a ProBError will be thrown. But this is
			// already taken care of because it is wrapped in an LTLError
			if (!(job.getResult() instanceof LTLError)) {
				throw e;
			}
		}
		IModelCheckingResult result = job.getResult();
		if (ui != null) {
			ui.isFinished(jobId, System.currentTimeMillis() - time, result,
					null);
		}
		return result;
	}

	@Override
	public IModelCheckingResult getResult() {
		return job.getResult() == null ? new LTLNotYetFinished(formula) : job
				.getResult();
	}

	@Override
	public String getJobId() {
		return jobId;
	}

	@Override
	public StateSpace getStateSpace() {
		return s;
	}

}