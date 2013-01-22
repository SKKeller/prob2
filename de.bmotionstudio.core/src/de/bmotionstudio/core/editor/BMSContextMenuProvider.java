/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.bmotionstudio.core.editor;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.gef.ContextMenuProvider;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.editparts.AbstractEditPart;
import org.eclipse.gef.ui.actions.ActionRegistry;
import org.eclipse.gef.ui.actions.GEFActionConstants;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.actions.ActionFactory;

import de.bmotionstudio.core.AttributeConstants;
import de.bmotionstudio.core.BMotionEditorPlugin;
import de.bmotionstudio.core.BMotionImage;
import de.bmotionstudio.core.BMotionStudio;
import de.bmotionstudio.core.IBControlService;
import de.bmotionstudio.core.IInstallMenu;
import de.bmotionstudio.core.model.control.BControl;

public class BMSContextMenuProvider extends ContextMenuProvider {

	private ActionRegistry actionRegistry;

	private IExtensionRegistry registry = Platform.getExtensionRegistry();

	private String[] eventIDs = { AttributeConstants.EVENT_MOUSECLICK };

	public BMSContextMenuProvider(EditPartViewer viewer, ActionRegistry registry) {
		super(viewer);
		setActionRegistry(registry);
	}

	@Override
	public void buildContextMenu(IMenuManager menu) {

		IAction action;

		GEFActionConstants.addStandardActionGroups(menu);

		action = getActionRegistry().getAction(ActionFactory.UNDO.getId());
		menu.appendToGroup(GEFActionConstants.GROUP_UNDO, action);

		action = getActionRegistry().getAction(ActionFactory.REDO.getId());
		menu.appendToGroup(GEFActionConstants.GROUP_UNDO, action);

		action = getActionRegistry().getAction(ActionFactory.COPY.getId());
		menu.appendToGroup(GEFActionConstants.GROUP_COPY, action);

		action = getActionRegistry().getAction(ActionFactory.PASTE.getId());
		menu.appendToGroup(GEFActionConstants.GROUP_COPY, action);

		action = getActionRegistry().getAction(ActionFactory.DELETE.getId());
		menu.appendToGroup(GEFActionConstants.GROUP_EDIT, action);

		Object sel = ((IStructuredSelection) getViewer().getSelection())
				.getFirstElement();

		if (sel instanceof AbstractEditPart) {
			AbstractEditPart editPart = (AbstractEditPart) sel;
			// buildCustomMenu(menu, editPart);
			buildObserverMenu(menu, editPart);
			//buildEventMenu(menu, editPart);
		}

	}

	private void buildCustomMenu(IMenuManager menu, AbstractEditPart editPart) {

		Object model = editPart.getModel();

		if (model instanceof BControl) {

			IExtensionPoint extensionPoint = registry
					.getExtensionPoint("de.bmotionstudio.core.installMenu");
			for (IExtension extension : extensionPoint.getExtensions()) {
				for (IConfigurationElement configurationElement : extension
						.getConfigurationElements()) {

					if ("menu".equals(configurationElement.getName())) {

						try {

							IInstallMenu installMenuClass = (IInstallMenu) configurationElement
									.createExecutableExtension("class");

							installMenuClass.installMenu(menu,
									getActionRegistry());

						} catch (final CoreException e) {
							e.printStackTrace();
						}

					}

				}

			}

		}

	}

	private void buildObserverMenu(IMenuManager menu, AbstractEditPart editPart) {

		Object model = editPart.getModel();

		BControl control = null;

		if (model instanceof BControl)
			control = (BControl) model;
		else
			return;

		final MenuManager handleObserverMenu = new MenuManager("New Observer",
				BMotionImage.getImageDescriptor(BMotionEditorPlugin.PLUGIN_ID,
						"icons/icon_observer.gif"), "observerMenu");
		menu.appendToGroup(GEFActionConstants.GROUP_ADD, handleObserverMenu);

		IExtensionPoint extensionPoint = Platform.getExtensionRegistry()
				.getExtensionPoint(
						"de.bmotionstudio.core.includeObserver");

		for (IExtension extension : extensionPoint.getExtensions()) {
			for (IConfigurationElement configurationElement : extension
					.getConfigurationElements()) {

				if ("include".equals(configurationElement.getName())) {

					String langID = configurationElement
							.getAttribute("language");

					if (langID != null
							&& langID.equals(BMotionStudio
									.getCurrentSimulation().getLanguage())) {
					
						for (IConfigurationElement configC : configurationElement
								.getChildren("control")) {

							String cID = configC.getAttribute("id");
							
							IBControlService controlService = BMotionEditorPlugin
									.getControlServicesId().get(cID);
			
							if (controlService != null
									&& control.getClass().equals(
											controlService.getControlClass())) {
								
								for (IConfigurationElement configO : configC
										.getChildren("observer")) {

									String oID = configO.getAttribute("id");
									IAction action = getActionRegistry()
											.getAction(
													"de.bmotionstudio.core.observerAction."
															+ oID);

									// TODO: Get correct name of observer
									String name = oID;
									
									action.setText(name);

									handleObserverMenu.add(action);

								}

							}

						}

					}

				}

			}
		}

	}

	// TODO: Reimplement me!!!
//	private void buildEventMenu(IMenuManager menu, AbstractEditPart editPart) {
//
//		Object model = editPart.getModel();
//
//		if (model instanceof BControl && !(model instanceof Visualization)) {
//
//			MenuManager handleEventMenu = new MenuManager("Events",
//					BMotionStudioImage.getImageDescriptor(
//							BMotionEditorPlugin.PLUGIN_ID,
//							"icons/icon_event.png"), "eventMenu");
//			menu.appendToGroup(GEFActionConstants.GROUP_ADD, handleEventMenu);
//
//			BControl bcontrol = (BControl) model;
//
//			// Has event
//			if (bcontrol.hasEvent(eventIDs[0])) {
//
//				SchedulerEvent event = bcontrol.getEvent(eventIDs[0]);
//
//				OpenSchedulerEventAction action = (OpenSchedulerEventAction) getActionRegistry()
//						.getAction(
//								"de.bmotionstudio.gef.editor.SchedulerEventAction."
//										+ event.getID());
//				action.setEventID(eventIDs[0]);
//				action.setText(event.getName());
//				action.setImageDescriptor(BMotionStudioImage
//						.getImageDescriptor(BMotionEditorPlugin.PLUGIN_ID,
//								"icons/icon_chop.gif"));
//				handleEventMenu.add(action);
//
//			} else { // Has no event
//
//				IExtensionPoint schedulerExtensionPoint = registry
//						.getExtensionPoint("de.bmotionstudio.gef.editor.schedulerEvent");
//				for (IExtension schedulerExtension : schedulerExtensionPoint
//						.getExtensions()) {
//					for (IConfigurationElement configSchedulerElement : schedulerExtension
//							.getConfigurationElements()) {
//
//						if ("schedulerEvent".equals(configSchedulerElement
//								.getName())) {
//
//							String sClassName = configSchedulerElement
//									.getAttribute("class");
//							Boolean show = Boolean
//									.valueOf(configSchedulerElement
//											.getAttribute("menu"));
//
//							if (show) {
//
//								OpenSchedulerEventAction action = (OpenSchedulerEventAction) getActionRegistry()
//										.getAction(
//												"de.bmotionstudio.gef.editor.SchedulerEventAction."
//														+ sClassName);
//								action.setEventID(eventIDs[0]);
//								action.setText(configSchedulerElement
//										.getAttribute("name"));
//
//								action.setImageDescriptor(null);
//
//								// }
//
//								handleEventMenu.add(action);
//
//							}
//
//						}
//
//					}
//
//				}
//
//			}
//		}
//
//	}

	private ActionRegistry getActionRegistry() {
		return actionRegistry;
	}

	private void setActionRegistry(ActionRegistry registry) {
		actionRegistry = registry;
	}

}