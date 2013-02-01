package de.bmotionstudio.core.editor;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorLauncher;
import org.eclipse.ui.IPerspectiveDescriptor;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.mapper.MapperWrapper;

import de.bmotionstudio.core.BMotionEditorPlugin;
import de.bmotionstudio.core.BMotionStudio;
import de.bmotionstudio.core.editor.action.SaveSimulationAction;
import de.bmotionstudio.core.model.Simulation;
import de.bmotionstudio.core.model.control.Visualization;
import de.bmotionstudio.core.util.PerspectiveUtil;


public class BMotionStudioLauncher implements IEditorLauncher {

	private IFile file;

	@Override
	public void open(IPath path) {
		
		file = ResourcesPlugin.getWorkspace().getRoot()
				.getFileForLocation(path);

		Simulation simulation = null;
		
		// Check if a simulation is already open
		Simulation currentSimulation = BMotionStudio.getCurrentSimulation();
		IPerspectiveDescriptor currentPerspective = BMotionStudio
				.getCurrentPerspective();

		// Check if the simulation is dirty and ask the user for saving it
		if (currentSimulation != null && currentSimulation.isDirty()) {

			MessageDialog dg = new MessageDialog(
					Display.getDefault().getActiveShell(),
					"You made changes to your visualization.",
					null,
					"Your visualization has beed modified. Save changes? Please note: The current visualization will be closed!",
					MessageDialog.QUESTION_WITH_CANCEL, new String[] {
							IDialogConstants.YES_LABEL,
							IDialogConstants.NO_LABEL,
							IDialogConstants.CANCEL_LABEL }, 0);
			switch (dg.open()) {
			case 0:
				// yes - save the visualization and perspective
				SaveSimulationAction saveSimulationAction = new SaveSimulationAction(
						currentSimulation,
						BMotionStudio.getCurrentProjectFile());
				saveSimulationAction.run();
				break;
			case 1:
				// no - do nothing
				break;
			case 2:
				// cancel - return
				return;
			}

		}

		// If a simulation is already open, close the corresponding perspective
		if (currentPerspective != null) {
			PerspectiveUtil.closePerspective(currentPerspective);
			PerspectiveUtil.deletePerspective(currentPerspective);
		}
			
		InputStream inputStream = null;

		try {

			inputStream = file.getContents();

			XStream xstream = new XStream() {
				@Override
				protected MapperWrapper wrapMapper(final MapperWrapper next) {
					return new MapperWrapper(next) {
						@Override
						public boolean shouldSerializeMember(
								@SuppressWarnings("rawtypes") final Class definedIn,
								final String fieldName) {
							if (definedIn == Object.class)
								return false;
							return super.shouldSerializeMember(definedIn,
									fieldName);
						}
					};
				}
			};

			BMotionEditorPlugin.setAliases(xstream);
			Object obj = xstream.fromXML(inputStream);
			
			// Set the correct image path. In this case the image path is a
			// subfolder called "images" in the corresponding project
			IFolder imageFolder = file.getProject().getFolder("images");
			if (!imageFolder.exists())
				imageFolder.create(true, true, new NullProgressMonitor());
			String imageFolderUrl = imageFolder.getLocationURI().toString()
					.replace("file:/", "");
			BMotionStudio.setImagePath(imageFolderUrl);
			// -------------------------------------------------------------
			
			if (obj instanceof Visualization) {

				// TODO: We need a converter for "old" visualizations
				// Visualization visualization = (Visualization) obj;
				//
				// simulation = new Simulation();
				//
				// String secId = UUID.randomUUID().toString();
				//
				// VisualizationView visualizationView = new VisualizationView(
				// "New Visualization View", secId, visualization);
				//
				// simulation.addVisualizationView(visualizationView);

			} else if (obj instanceof Simulation) {
				simulation = (Simulation) obj;
			}

			if (simulation != null) {
				BMotionStudio.setCurrentSimulation(simulation);
				BMotionStudio.setCurrentPerspective(PerspectiveUtil
						.openPerspective(file));
				BMotionStudio.setCurrentProjectFile(file);
				PerspectiveUtil.initViews(simulation);
			}

		} catch (CoreException e) {
			e.printStackTrace();
		} finally {
			try {
				if (inputStream != null)
					inputStream.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

	}

}
