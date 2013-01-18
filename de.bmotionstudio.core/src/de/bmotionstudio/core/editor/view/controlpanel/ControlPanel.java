package de.bmotionstudio.core.editor.view.controlpanel;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collection;
import java.util.EventObject;
import java.util.Map;

import org.eclipse.draw2d.ColorConstants;
import org.eclipse.gef.commands.CommandStackListener;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.part.ViewPart;

import de.bmotionstudio.core.BMotionEditorPlugin;
import de.bmotionstudio.core.ISimulationListener;
import de.bmotionstudio.core.model.Simulation;
import de.bmotionstudio.core.model.VisualizationView;

public class ControlPanel extends ViewPart implements ISimulationListener,
		CommandStackListener, PropertyChangeListener {

	public static String ID = "de.bmotionstudio.core.ControlPanel";

	private Composite container;

	private TreeViewer treeViewer;

	@Override
	public void createPartControl(Composite parent) {

		container = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		container.setLayout(layout);

		Tree tree = new Tree(container, SWT.BORDER | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.RESIZE);
		tree.setHeaderVisible(true);
		treeViewer = new TreeViewer(tree);

		TreeViewerColumn column1 = new TreeViewerColumn(treeViewer, SWT.LEFT);
		tree.setLinesVisible(true);
		column1.getColumn().setAlignment(SWT.LEFT);
		column1.getColumn().setText("Name");
		column1.getColumn().setWidth(300);
		column1.setEditingSupport(new EditingSupport(treeViewer) {

			@Override
			protected void setValue(Object element, Object value) {
				if (element instanceof VisualizationView) {

					VisualizationView visView = (VisualizationView) element;
					visView.setName(String.valueOf(value));

					ITreeSelection selection = ((ITreeSelection) treeViewer
							.getSelection());
					Object parent = selection.getPaths()[0].getParentPath()
							.getLastSegment();
					if (parent instanceof Simulation)
						((Simulation) parent).setDirty(true);
					treeViewer.update(element, null);

				}
			}

			@Override
			protected Object getValue(Object element) {
				if (element instanceof VisualizationView)
					return ((VisualizationView) element).getName();
				return null;
			}

			@Override
			protected CellEditor getCellEditor(Object element) {
				return new TextCellEditor(treeViewer.getTree());
			}

			@Override
			protected boolean canEdit(Object element) {
				if (element instanceof VisualizationView)
					return true;
				return false;
			}

		});

		TreeViewerColumn column2 = new TreeViewerColumn(treeViewer, SWT.RIGHT);
		column2.getColumn().setAlignment(SWT.LEFT);
		column2.getColumn().setText("Dirty");
		column2.getColumn().setWidth(40);

		TreeViewerColumn column3 = new TreeViewerColumn(treeViewer, SWT.RIGHT);
		column3.getColumn().setAlignment(SWT.LEFT);
		column3.getColumn().setText("Running");
		column3.getColumn().setWidth(40);

		treeViewer.setContentProvider(new ITreeContentProvider() {

			@Override
			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}

			@Override
			public void dispose() {
			}

			@Override
			public boolean hasChildren(Object element) {
				if (element instanceof Simulation)
					return true;
				return false;
			}

			@Override
			public Object getParent(Object element) {
				return null;
			}

			@Override
			public Object[] getElements(Object inputElement) {
				if (inputElement instanceof Collection<?>) {
					Collection<?> l = (Collection<?>) inputElement;
					return l.toArray(new Object[l.size()]);
				}
				return null;
			}

			@Override
			public Object[] getChildren(Object parentElement) {
				if (parentElement instanceof Simulation) {
					Map<String, VisualizationView> visualizationViews = ((Simulation) parentElement)
							.getVisualizationViews();
					return visualizationViews.values().toArray(
							new VisualizationView[visualizationViews.values()
									.size()]);
				}
				return null;
			}

		});

		// TODO: Reimplement me!!!
//		treeViewer.setLabelProvider(new ControlLabelProvider());
//		treeViewer.addDoubleClickListener(new IDoubleClickListener() {
//			@Override
//			public void doubleClick(DoubleClickEvent event) {
//				IStructuredSelection sel = (IStructuredSelection) event
//						.getSelection();
//				Object firstElement = sel.getFirstElement();
//				if (firstElement instanceof Simulation) {
//					Simulation simulation = (Simulation) firstElement;
//					PerspectiveUtil.switchPerspective(PerspectiveUtil
//							.getPerspectiveIdFromFile(simulation
//									.getProjectFile()));
//				}
//
//			}
//		});

//		if (!BMotionEditorPlugin.getOpenSimulations().values().isEmpty()) {
//			treeViewer.setInput(BMotionEditorPlugin.getOpenSimulations()
//					.values());
//			treeViewer.expandAll();
//		}

		final CloseSimulationAction closeSimulationAction = new CloseSimulationAction(
				treeViewer);
		final SaveSimulationAction saveSimulationAction = new SaveSimulationAction(
				treeViewer);
		final RunSimulationAction runSimulationAction = new RunSimulationAction(
				treeViewer);
		final AddVisualizationViewAction addVisualizationViewAction = new AddVisualizationViewAction(
				treeViewer);
		final RemoveVisualizationViewAction deleteVisualizationViewAction = new RemoveVisualizationViewAction(
				treeViewer);
		final Separator separator = new Separator();

		MenuManager manager = new MenuManager();
		manager.setRemoveAllWhenShown(true);
		manager.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				IStructuredSelection selection = (IStructuredSelection) treeViewer
						.getSelection();
				Object firstElement = selection.getFirstElement();
				if (firstElement instanceof Simulation) {
					manager.add(closeSimulationAction);
					manager.add(saveSimulationAction);
					manager.add(separator);
					manager.add(addVisualizationViewAction);
					manager.add(separator);
					manager.add(runSimulationAction);
				} else if (firstElement instanceof VisualizationView) {
					manager.add(deleteVisualizationViewAction);
				}
			}
		});

		treeViewer.getControl().setMenu(
				manager.createContextMenu(treeViewer.getControl()));

		GridData layoutData = new GridData(GridData.FILL_BOTH);
		treeViewer.getControl().setLayoutData(layoutData);

		BMotionEditorPlugin.openSimulationListeners.add(this);

	}

	@Override
	public void setFocus() {
	}

	@Override
	public void openSimulation(Simulation simulation) {
		setInput(simulation);
		simulation.addPropertyChangeListener(this);
	}

	@Override
	public void closeSimulation(Simulation simulation) {
		setInput(simulation);
		simulation.removePropertyChangeListener(this);
	}

	private void setInput(Simulation simulation) {
		// TODO: Reimplement me!!!
//		if (treeViewer != null && !treeViewer.getControl().isDisposed()) {
//			treeViewer.setInput(BMotionEditorPlugin.getOpenSimulations()
//					.values());
//			treeViewer.expandAll();
//		}
	}

	@Override
	public void commandStackChanged(EventObject event) {
		treeViewer.refresh();
	}

	private class ControlLabelProvider implements ITableLabelProvider,
			ITableColorProvider {

		@Override
		public void addListener(ILabelProviderListener listener) {
		}

		@Override
		public void dispose() {
		}

		@Override
		public boolean isLabelProperty(Object element, String property) {
			return false;
		}

		@Override
		public void removeListener(ILabelProviderListener listener) {
			}

		@Override
		public Color getForeground(Object element, int columnIndex) {
			return null;
		}

		@Override
		public Color getBackground(Object element, int columnIndex) {
			switch (columnIndex) {
			case 1:
				if (element instanceof Simulation) {
					Simulation simulation = (Simulation) element;
					if (simulation.isDirty())
						return ColorConstants.red;
					else
						return ColorConstants.green;

				}
			case 2:
				if (element instanceof Simulation) {
					Simulation simulation = (Simulation) element;
					if (simulation.isRunning())
						return ColorConstants.green;
					else
						return ColorConstants.red;

				}
			}
			return null;
		}

		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		@Override
		public String getColumnText(Object element, int columnIndex) {
			// TODO: Reimplement me!!!
//			switch (columnIndex) {
//			case 0:
//				if (element instanceof Simulation) {
//					Simulation simulation = (Simulation) element;
//					String prefix = "";
//					if (simulation.isDirty())
//						prefix = "* ";
//					return prefix
//							+ ((Simulation) element).getProjectFile().getName();
//				} else if (element instanceof VisualizationView)
//					return ((VisualizationView) element).getName();
//			}
			return null;
		}

	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if (evt.getPropertyName().equals("dirty")
				|| evt.getPropertyName().equals("running"))
			treeViewer.refresh();
	}

}
