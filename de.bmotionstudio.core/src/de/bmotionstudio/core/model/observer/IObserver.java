/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.bmotionstudio.core.model.observer;

import de.bmotionstudio.core.model.control.BControl;
import de.prob.statespace.History;

public interface IObserver {

	/**
	 * This method is called after every state change. The method tells the
	 * control how it has to look like and to behave.
	 * 
	 * @param history
	 *            The running animation
	 * @param bcontrol
	 *            The corresponding control
	 */
	public void check(History history, BControl control);
	
	public String getIdentifier();
	
	public void setIdentifier(String identifier);
	
	public String getDescription();
	
	public String getID();
	
	public String getName();
	
}
