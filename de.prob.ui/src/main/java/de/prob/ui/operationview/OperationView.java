package de.prob.ui.operationview;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.services.ISourceProviderService;

import com.google.common.base.Joiner;
import com.google.inject.Injector;

import de.prob.model.representation.AbstractElement;
import de.prob.model.representation.AbstractModel;
import de.prob.model.representation.BEvent;
import de.prob.model.representation.Machine;
import de.prob.statespace.AnimationSelector;
import de.prob.statespace.History;
import de.prob.statespace.IHistoryChangeListener;
import de.prob.statespace.OpInfo;
import de.prob.ui.services.HistoryActiveProvider;
import de.prob.ui.services.ModelLoadedProvider;
import de.prob.webconsole.ServletContextListener;

/**
 * This sample class demonstrates how to plug-in a new workbench view. The view
 * shows data obtained from the model. The sample creates a dummy model on the
 * fly, but a real implementation would connect to the model available either in
 * this or another plug-in (e.g. the workspace). The view is connected to the
 * model using a content provider.
 * <p>
 * The view uses a label provider to define how model objects should be
 * presented in the view. Each view can present the same model objects using
 * different labels and icons, if needed. Alternatively, a single label provider
 * can be shared between views in order to ensure that objects of the same type
 * are presented in the same way everywhere.
 * <p>
 */

public class OperationView extends ViewPart implements IHistoryChangeListener {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "de.prob.ui.operationview.OperationView";

	private TableViewer viewer;
	private History currentHistory;
	private AbstractModel currentModel;
	private boolean modelLoaded;

	Injector injector = ServletContextListener.INJECTOR;

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	@Override
	public void createPartControl(final Composite parent) {
		final AnimationSelector selector = injector
				.getInstance(AnimationSelector.class);
		selector.registerHistoryChangeListener(this);
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL);
		createColumns();
		viewer.setContentProvider(new OperationsContentProvider());
		viewer.setLabelProvider(new OperationViewLabelProvider());
		viewer.setSorter(new ViewerSorter());
		viewer.setInput(getViewSite());

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem()
				.setHelp(viewer.getControl(), "de.prob.ui.viewer");
		hookContextMenu();
		hookDoubleClickAction();
		modelLoaded = false;

		final Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
	}

	private void hookContextMenu() {
		final OperationView x = this;
		final TableViewer viewer = this.viewer;
		final MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		final IMenuListener listener = new IMenuListener() {
			@Override
			public void menuAboutToShow(final IMenuManager manager) {
				x.fillContextMenu(manager);
			}
		};
		menuMgr.addMenuListener(listener);
		final Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void fillContextMenu(final IMenuManager manager) {
		final List<OpInfo> selectedOperations = getSelectedOperations();
		for (final OpInfo opInfo : selectedOperations) {
			final Action executeOp = new Action() {
				@Override
				public void run() {
					final History newHistory = currentHistory.add(opInfo.id);
					newHistory
							.notifyAnimationChange(currentHistory, newHistory);
				}
			};
			executeOp.setText(Joiner.on(",").join(opInfo.params));
			manager.add(executeOp);
		}
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new OTVDoubleClickListener());
	}

	private void createColumns() {
		final TableViewerColumn column1 = new TableViewerColumn(viewer,
				SWT.NONE);
		column1.getColumn().setText("Event");
		column1.getColumn().setResizable(true);
		column1.getColumn().pack();

		final TableViewerColumn column2 = new TableViewerColumn(viewer,
				SWT.NONE);
		column2.getColumn().setText("Parameter(s)");
		column2.getColumn().setResizable(true);
		column2.getColumn().pack();
	}

	/**
	 * Recalculate size of all columns
	 */
	private void packTableColumns() {
		for (final TableColumn column : viewer.getTable().getColumns()) {
			column.pack();
		}
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	@Override
	public void historyChange(final History history) {
		currentHistory = history;
		if (history == null) {
			updateModelLoadedProvider(false);
			modelLoaded = false;
		} else if (!modelLoaded) {
			updateModelLoadedProvider(true);
			modelLoaded = true;
		}
		if (history != null) {
			final AbstractModel model = history.getModel();
			if (currentModel != model && viewer != null) {
				updateModel(model);
			}
		}
		Display.getDefault().syncExec(new Runnable() {

			@Override
			public void run() {
				if (!viewer.getTable().isDisposed()) {
					viewer.setInput(history);
					packTableColumns();
				}
			}
		});
		try {
			updateHistoryEnabled(history);
		} catch (final Exception e) {
		}
	}

	@SuppressWarnings("unchecked")
	public List<OpInfo> getSelectedOperations() {
		if (viewer.getSelection() != null
				&& viewer.getSelection() instanceof IStructuredSelection) {
			final IStructuredSelection ssel = (IStructuredSelection) viewer
					.getSelection();
			if (ssel.getFirstElement() instanceof ArrayList<?>) {
				final List<OpInfo> opList = (ArrayList<OpInfo>) ssel
						.getFirstElement();
				return opList;
			} else {
				System.out.println("Selection is: "
						+ ssel.getFirstElement().getClass());
			}
		}
		return null;
	}

	private class OTVDoubleClickListener implements IDoubleClickListener {

		@Override
		public void doubleClick(final DoubleClickEvent event) {
			final List<OpInfo> selectedOperations = getSelectedOperations();
			if (selectedOperations != null && !selectedOperations.isEmpty()) {
				try {
					final History newHistory = currentHistory
							.add(selectedOperations.get(0).id);
					newHistory
							.notifyAnimationChange(currentHistory, newHistory);
				} catch (IllegalArgumentException e) {
					// Happens when the user tries to execute too many
					// operations in the OperationView too quickly
				}
			}
		}
	}

	private void updateModelLoadedProvider(final boolean b) {
		IWorkbenchPartSite site = getSite();
		final ISourceProviderService service = (ISourceProviderService) site
				.getService(ISourceProviderService.class);
		final ModelLoadedProvider sourceProvider = (ModelLoadedProvider) service
				.getSourceProvider(ModelLoadedProvider.SERVICE);
		sourceProvider.setEnabled(b);
	}

	private void updateHistoryEnabled(final History history) {
		final ISourceProviderService service = (ISourceProviderService) this
				.getSite().getService(ISourceProviderService.class);
		final HistoryActiveProvider sourceProvider = (HistoryActiveProvider) service
				.getSourceProvider(HistoryActiveProvider.FORWARD_SERVICE);
		sourceProvider.historyChange(history);
	}

	private void updateModel(final AbstractModel model) {
		currentModel = model;
		((OperationsContentProvider) viewer.getContentProvider())
				.setAllOperations(getOperationNames(model));
	}

	private Map<String, Object> getOperationNames(final AbstractModel model) {
		final Map<String, Object> names = new HashMap<String, Object>();
		final AbstractElement component = model.getMainComponent();
		if (component instanceof Machine) {
			final Machine machine = (Machine) component;
			Set<BEvent> childrenOfType = machine
					.getChildrenOfType(BEvent.class);
			for (BEvent event : childrenOfType) {
				names.put(event.getName(), event);
			}
		}
		return names;
	}
}