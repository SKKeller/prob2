/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.bmotionstudio.core.model.control;

import de.bmotionstudio.core.model.attribute.BAttributeAlpha;
import de.bmotionstudio.core.model.attribute.BAttributeImage;

/**
 * @author Lukas Ladenberger
 * 
 */
public class BImage extends BControl {

	@Override
	protected void initAttributes() {
		initAttribute(new BAttributeImage(null));
		initAttribute(new BAttributeAlpha(255));
	}

}
