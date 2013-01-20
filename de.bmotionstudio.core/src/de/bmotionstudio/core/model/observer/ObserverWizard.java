/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.bmotionstudio.core.model.observer;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import de.bmotionstudio.core.BMotionAbstractWizard;
import de.bmotionstudio.core.model.control.BControl;

/**
 * 
 * The BMotion Studio provides an easy way to handle Observers. For this,
 * Observers can have a corresponding wizard. The user can open it by calling
 * the context menu of a Control.
 * 
 * @author Lukas Ladenberger
 * 
 */
public abstract class ObserverWizard extends BMotionAbstractWizard {

	private Observer observer;

	public ObserverWizard(Shell shell, BControl control, Observer observer) {
		super(shell, control);
		this.observer = observer;
	}

	public Observer getObserver() {
		return this.observer;
	}

	public abstract Point getSize();
	
	@Override
	public String getName() {
		return observer.getName();
	}

	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		PlatformUI.getWorkbench().getHelpSystem()
				.setHelp(newShell, observer.getID());
	}

}
