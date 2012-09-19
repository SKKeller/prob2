package de.prob.ui.operationview;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.services.ISourceProviderService;

import com.google.common.base.Joiner;
import com.google.inject.Injector;

import de.prob.animator.domainobjects.OpInfo;
import de.prob.statespace.AnimationSelector;
import de.prob.statespace.History;
import de.prob.statespace.IHistoryChangeListener;
import de.prob.ui.services.HistoryActiveProvider;
import de.prob.ui.services.ModelLoadedProvider;
import de.prob.webconsole.ServletContextListener;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class OperationView extends ViewPart implements IHistoryChangeListener{

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "de.prob.ui.operationview.OperationView";

	private TableViewer viewer;
	private History currentHistory;
	
	Injector injector = ServletContextListener.INJECTOR;
	 
	class ViewLabelProvider extends LabelProvider implements ITableLabelProvider {
		public String getColumnText(Object obj, int index) {
			if(index == 0) {
				if(obj instanceof OpInfo) {
					OpInfo op = (OpInfo) obj;
					return op.name;
				} else {
					return obj.getClass().toString();
				}
			}
			
			if(index == 1) {
				if(obj instanceof OpInfo) {
					OpInfo op = (OpInfo) obj;
					return Joiner.on(",").join(op.params);
				} else {
					return obj.getClass().toString();
				}
			}
			return "";
		}
		public Image getColumnImage(Object obj, int index) {
			if( index == 1 )
				return null;
			return getImage(obj);
		}
		public Image getImage(Object obj) {
			return PlatformUI.getWorkbench().
					getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
		}
	}
	class NameSorter extends ViewerSorter {
	}

	/**
	 * The constructor.
	 */
	public OperationView() {
		AnimationSelector selector = injector.getInstance(AnimationSelector.class);
		selector.registerHistoryChangeListener(this);
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		createColumns();
		viewer.setContentProvider(new OperationsContentProvider());
		viewer.setLabelProvider(new ViewLabelProvider());
		viewer.setSorter(new NameSorter());
		viewer.setInput(getViewSite());

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "de.prob.ui.viewer");
		hookDoubleClickAction();
		updateModelLoadedProvider();
		
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new OTVDoubleClickListener());
	}

	private void createColumns() {
		TableViewerColumn column1 = new TableViewerColumn(viewer, SWT.NONE);
		column1.getColumn().setText("Event");
		column1.getColumn().setResizable(true);
		column1.getColumn().pack();

		TableViewerColumn column2 = new TableViewerColumn(viewer, SWT.NONE);
		column2.getColumn().setText("Parameter(s)");
		column2.getColumn().setResizable(true);
		column2.getColumn().pack();
	}

	/**
	 * Recalculate size of all columns
	 */
	private void packTableColumns() {
		for (TableColumn column : viewer.getTable().getColumns()) {
			column.pack();
		}
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		viewer.getControl().setFocus();
	}
	
	@Override
	public void historyChange(final History history) {
		currentHistory = history;
		Display.getDefault().asyncExec(new Runnable() {
			
			@Override
			public void run() {
				viewer.setInput(history);
				packTableColumns();
			}
		});
		updateHistoryEnabled(history);
	}
	
	public OpInfo getSelectedOperation() {
		if (viewer.getSelection() != null
				&& viewer.getSelection() instanceof IStructuredSelection) {
			final IStructuredSelection ssel = (IStructuredSelection) viewer
					.getSelection();
			if (ssel.getFirstElement() instanceof OpInfo)
				return (OpInfo) ssel.getFirstElement();
			else
				System.out.println("Selection is: "+ssel.getFirstElement().getClass());
		}
		return null;
	}
	
	private class OTVDoubleClickListener implements IDoubleClickListener {

		public void doubleClick(final DoubleClickEvent event) {
			if (getSelectedOperation() != null) {
				executeSingleOperation(getSelectedOperation());
			} 
		}
	}
	
	private void executeSingleOperation(OpInfo op) {
		currentHistory.add(op.id);
	}
	
	private void updateModelLoadedProvider() {
		ISourceProviderService service = (ISourceProviderService) this
				.getSite().getService(ISourceProviderService.class);
		ModelLoadedProvider sourceProvider = (ModelLoadedProvider) service
				.getSourceProvider(ModelLoadedProvider.SERVICE);
		sourceProvider.setEnabled(true);
	}
	
	private void updateHistoryEnabled(History history) {
		ISourceProviderService service = (ISourceProviderService) this
				.getSite().getService(ISourceProviderService.class);
		HistoryActiveProvider sourceProvider = (HistoryActiveProvider) service
				.getSourceProvider(HistoryActiveProvider.FORWARD_SERVICE);
		sourceProvider.historyChange(history);
	}
}