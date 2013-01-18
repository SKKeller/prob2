/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.bmotionstudio.core.editor.view.controlpanel;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.PlatformUI;

import de.bmotionstudio.core.BMotionImage;
import de.bmotionstudio.core.model.Simulation;
import de.bmotionstudio.core.util.PerspectiveUtil;

public class RunSimulationAction extends Action {

	private TreeViewer viewer;

	public RunSimulationAction(TreeViewer viewer) {
		this.viewer = viewer;
		setText("Run Simulation");
		setImageDescriptor(BMotionImage
				.getImageDescriptor("icons/icon_run.png"));
	}

	@Override
	public void run() {

		// TODO: Reimplement me!!!
//		IStructuredSelection sel = (IStructuredSelection) viewer.getSelection();
//		Object firstElement = sel.getFirstElement();
//		if (firstElement instanceof Simulation) {
//
//			Simulation simulation = (Simulation) firstElement;
//
//			IPerspectiveRegistry perspectiveRegistry = PlatformUI
//					.getWorkbench().getPerspectiveRegistry();
//
//			String perspectiveId = PerspectiveUtil
//					.getPerspectiveIdFromFile(simulation.getProjectFile());
//			IPerspectiveDescriptor perspectiveDescriptor = perspectiveRegistry
//					.findPerspectiveWithId(perspectiveId);
//			if (perspectiveDescriptor != null)
//				PerspectiveUtil.switchPerspective(perspectiveDescriptor);
//
//			// simulation.start();
//
//		}

	}

}
