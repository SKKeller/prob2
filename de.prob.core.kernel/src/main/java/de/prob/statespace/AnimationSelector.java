package de.prob.statespace;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import jline.History;

import com.google.inject.Singleton;

import de.prob.model.representation.AbstractElement;

/**
 * This class provides a registry of all currently running animations. It
 * provides the user to communicate between the UI and the console, and provides
 * a listener framework so that the user can animate machines using
 * {@link Trace} objects to represent the different animations. It also
 * maintains a pointer to one {@link Trace} object which is the current
 * animation.
 * 
 * @author joy
 * 
 */
@Singleton
public class AnimationSelector {

	List<WeakReference<IAnimationChangedListener>> traceListeners = new CopyOnWriteArrayList<WeakReference<IAnimationChangedListener>>();
	List<WeakReference<IModelChangedListener>> modelListeners = new CopyOnWriteArrayList<WeakReference<IModelChangedListener>>();

	List<StateSpace> statespaces = new ArrayList<StateSpace>();
	List<Trace> histories = new ArrayList<Trace>();

	Trace currentTrace = null;
	StateSpace currentStateSpace = null;

	/**
	 * An {@link IAnimationChangedListener} can register itself via this method
	 * when it wants to receive updates about any changes in the current state.
	 * 
	 * @param listener
	 */
	public void registerHistoryChangeListener(
			final IAnimationChangedListener listener) {

		traceListeners.add(new WeakReference<IAnimationChangedListener>(
				listener));
		if (currentTrace != null) {
			notifyHistoryChange(currentTrace);
		}
	}

	public void registerModelChangedListener(
			final IModelChangedListener listener) {
		modelListeners.add(new WeakReference<IModelChangedListener>(listener));
		if (currentStateSpace != null) {
			notifyModelChanged(currentStateSpace);
		}
	}

	public void unregisterModelChangedListener(
			final IModelChangedListener listener) {
		modelListeners.remove(listener);
	}

	/**
	 * Changes the current history to the specified {@link Trace} and notifies a
	 * history change ({@link AnimationSelector#notifyHistoryChange(Trace)})
	 * 
	 * @param history
	 */
	public void changeCurrentHistory(final Trace history) {
		currentTrace = history;
		notifyHistoryChange(history);

		if (currentTrace != null
				&& currentTrace.getStatespace() != currentStateSpace) {
			currentStateSpace = currentTrace.getStatespace();
			notifyModelChanged(currentStateSpace);
		}
	}

	/**
	 * Adds the specified {@link Trace} history to the registry, registers
	 * itself as the {@link IAnimationListener} within the history, sets the
	 * current history to history, and notifies a history change (
	 * {@link AnimationSelector#notifyHistoryChange(Trace)})
	 * 
	 * @param history
	 */
	public void addNewHistory(final Trace history) {
		histories.add(history);
		currentTrace = history;
		notifyHistoryChange(history);

		StateSpace s = history.getStatespace();
		if (!statespaces.contains(s)) {
			statespaces.add(s);
		}
		if (s != null && !s.equals(currentStateSpace)) {
			currentStateSpace = s;
			notifyModelChanged(s);
		}
	}

	/**
	 * Let all {@link IAnimationChangedListener}s know that the current history
	 * has changed
	 * 
	 * @param history
	 */
	public void notifyHistoryChange(final Trace history) {
		for (final WeakReference<IAnimationChangedListener> listener : traceListeners) {
			IAnimationChangedListener historyChangeListener = listener.get();
			if (historyChangeListener != null) {
				historyChangeListener.historyChange(history);
			}
		}
	}

	private void notifyModelChanged(final StateSpace s) {
		for (WeakReference<IModelChangedListener> listener : modelListeners) {
			IModelChangedListener modelChangedListener = listener.get();
			if (modelChangedListener != null) {
				modelChangedListener.modelChanged(s);
			}
		}
	}

	/**
	 * @return the current {@link Trace}
	 */
	public Trace getCurrentHistory() {
		return currentTrace;
	}

	/**
	 * @return the list of {@link Trace} objects in the registry.
	 */
	public List<Trace> getHistories() {
		return histories;
	}

	public List<StateSpace> getStatespaces() {
		return statespaces;
	}

	/**
	 * @param history
	 * @return the {@link AbstractElement} model that corresponds to the given
	 *         {@link Trace}
	 */
	public AbstractElement getModel(final Trace history) {
		return history.getModel();
	}

	@Override
	public String toString() {
		return "Animations Registry";
	}

	/**
	 * notify all of the listeners using the current history
	 * {@link AnimationSelector#notifyHistoryChange(Trace)}
	 */
	public void refresh() {
		notifyHistoryChange(currentTrace);
		notifyModelChanged(currentStateSpace);
	}

	/**
	 * Lets the AnimationSelector know that the {@link Trace} object with
	 * reference oldTrace has been changed to newTrace so that the
	 * AnimationSelector can update its registry.
	 * 
	 * @param oldTrace
	 * @param newTrace
	 */
	public void replaceTrace(final Trace oldTrace, final Trace newTrace) {
		if (oldTrace.equals(currentTrace)) {
			notifyHistoryChange(newTrace);
		}
		int indexOf = histories.indexOf(oldTrace);
		histories.set(indexOf, newTrace);
		currentTrace = newTrace;

		if (currentTrace != null
				&& currentTrace.getStatespace() != currentStateSpace) {
			currentStateSpace = currentTrace.getStatespace();
			notifyModelChanged(currentStateSpace);
		}
	}

	/**
	 * Lets the {@link IAnimationListener} know that it should remove the
	 * {@link History} object from its registry.
	 * 
	 * @param history
	 */
	public void removeTrace(final Trace trace) {
		remove(trace);
		refresh();
	}

	private void remove(final Trace trace) {
		if (!histories.contains(trace)) {
			return;
		}
		if (currentTrace == trace) {
			int indexOf = histories.indexOf(trace);
			histories.remove(trace);
			if (histories.isEmpty()) {
				currentTrace = null;
				return;
			}
			if (indexOf == histories.size()) {
				currentTrace = histories.get(indexOf - 1);
				return;
			}
			currentTrace = histories.get(indexOf);
			return;
		}
		histories.remove(trace);
	}

}