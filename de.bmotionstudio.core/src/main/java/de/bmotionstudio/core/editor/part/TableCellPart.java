/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.bmotionstudio.core.editor.part;

import java.beans.PropertyChangeEvent;

import org.eclipse.draw2d.IFigure;
import org.eclipse.gef.EditPolicy;
import org.eclipse.gef.Request;
import org.eclipse.gef.RequestConstants;
import org.eclipse.swt.graphics.RGB;

import de.bmotionstudio.core.AttributeConstants;
import de.bmotionstudio.core.editor.edit.TextCellEditorLocator;
import de.bmotionstudio.core.editor.edit.TextEditManager;
import de.bmotionstudio.core.editor.editpolicy.CustomDirectEditPolicy;
import de.bmotionstudio.core.editor.editpolicy.RenamePolicy;
import de.bmotionstudio.core.editor.figure.TableCellFigure;
import de.bmotionstudio.core.model.control.BControl;

public class TableCellPart extends BMSAbstractEditPart {

	@Override
	protected IFigure createEditFigure() {
		return new TableCellFigure();
	}

	@Override
	protected void prepareEditPolicies() {
		installEditPolicy(EditPolicy.DIRECT_EDIT_ROLE,
				new CustomDirectEditPolicy());
		installEditPolicy(EditPolicy.NODE_ROLE, new RenamePolicy());
	}

	@Override
	public void refreshEditFigure(IFigure figure, BControl model,
			PropertyChangeEvent evt) {

		Object value = evt.getNewValue();
		String aID = evt.getPropertyName();

		if (aID.equals(AttributeConstants.ATTRIBUTE_TEXT))
			((TableCellFigure) figure).setText(value.toString());

		if (aID.equals(AttributeConstants.ATTRIBUTE_BACKGROUND_COLOR))
			((TableCellFigure) figure).setBackgroundColor((RGB) value);

		if (aID.equals(AttributeConstants.ATTRIBUTE_TEXT_COLOR))
			((TableCellFigure) figure).setTextColor((RGB) value);

		if (aID.equals(AttributeConstants.ATTRIBUTE_FOREGROUND_COLOR))
			((TableCellFigure) figure).setForegroundColor((RGB) value);

		if (aID.equals(AttributeConstants.ATTRIBUTE_FONT))
			((TableCellFigure) figure).setFont((value.toString()));

	}

	@Override
	protected void refreshEditLayout(IFigure figure, BControl control) {

		// Set size of parent column
		int width = control.getDimension().width;
		control.getParent().setAttributeValue(
				AttributeConstants.ATTRIBUTE_WIDTH, width);

		super.refreshEditLayout(figure, control);

	}

	private void performDirectEdit() {
		new TextEditManager(this, new TextCellEditorLocator(
				(IFigure) getFigure())).show();
	}

	@Override
	public void performRequest(Request request) {
		super.performRequest(request);
		if (request.getType() == RequestConstants.REQ_DIRECT_EDIT)
			performDirectEdit();
	}

}
