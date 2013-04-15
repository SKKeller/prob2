/**
 * 
 */
package de.prob.ui.constraint;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;

import de.prob.animator.IAnimator;
import de.prob.animator.command.ConstraintBasedInvariantCheckCommand;
import de.prob.animator.command.AbstractCommand;
import de.prob.check.ConstraintBasedCheckingResult.Result;
import de.prob.ui.ProBJobFinishedListener;

/**
 * 
 * @author plagge
 * 
 */
public class InvariantCheckFinishedListener extends ProBJobFinishedListener {
	private final Shell shell;

	public InvariantCheckFinishedListener(final Shell shell) {
		this.shell = shell;
	}

	@Override
	protected void showResult(final AbstractCommand command,
			final IAnimator animator) {
		final ConstraintBasedInvariantCheckCommand invCmd = (ConstraintBasedInvariantCheckCommand) command;
		final Result result = invCmd.getResult().getResult();
		final int dialogType;
		final String dialogTitle;
		final String message;
		switch (result) {
		case interrupted:
			dialogType = MessageDialog.WARNING;
			dialogTitle = "User Interrupt";
			message = "The invariant check has been interrupted by the user.";
			break;
		case no_invariant_violation_found:
			dialogType = MessageDialog.INFORMATION;
			dialogTitle = "No Invariant Violation found";
			message = "No possible invariant violation has been found.";
			break;
		case invariant_violation:
			dialogType = MessageDialog.ERROR;
			dialogTitle = "Invariant Violation found";
			message = "An invariant violation has been found.";//\nThe first found example will be shown in the history.";
			// Implement way to display
			//displayViolation(invCmd, animator);
			break;
		default:
			//Logger.notifyUser("Unexpected result: " + result);
			return;
		}


		if (shell.isDisposed()) {
			System.out.println("Invariant Check finished: " + dialogTitle);
		} else {
			final Runnable runnable = new Runnable() {
				@Override
				public void run() {
					MessageDialog.open(dialogType, shell, dialogTitle, message,
							SWT.NONE);
				}
			};
			shell.getDisplay().asyncExec(runnable);
		}
	}

	//	private void displayViolation(
	//			final ConstraintBasedInvariantCheckCommand cmd,
	//			final Animator animator) {
	//		final ConstraintBasedInvariantCheckCommand.InvariantCheckCounterExample example = cmd
	//				.getCounterExamples().iterator().next();
	//		final Operation step1 = example.getStep1();
	//		final Operation step2 = example.getStep2();
	//		try {
	//			// we do not reset the history because we want to keep the root
	//			// state, we just start a new path from root
	//			animator.getHistory().gotoPos(0);
	//			ExecuteOperationCommand.executeOperation(animator, step1);
	//			ExecuteOperationCommand.executeOperation(animator, step2);
	//		} catch (ProBException e) {
	//			e.notifyUserOnce();
	//		}
	//	}

}
