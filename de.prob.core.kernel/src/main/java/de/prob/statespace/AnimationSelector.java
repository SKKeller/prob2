package de.prob.statespace;

import java.util.ArrayList;
import java.util.List;

import com.google.inject.Singleton;

import de.prob.model.representation.AbstractElement;

/**
 * This class provides a registry of all currently running animations. It
 * provides the user to communicate between the UI and the console, and provides
 * a listener framework so that the user can animate machines using
 * {@link History} objects to represent the different animations. It also
 * maintains a pointer to one {@link History} object which is the current
 * animation.
 * 
 * @author joy
 * 
 */
@Singleton
public class AnimationSelector implements IAnimationListener {

	List<IHistoryChangeListener> listeners = new ArrayList<IHistoryChangeListener>();
	List<History> histories = new ArrayList<History>();
	History currentHistory = null;

	/**
	 * An {@link IHistoryChangeListener} can register itself via this method
	 * when it wants to receive updates about any changes in the current state.
	 * 
	 * @param listener
	 */
	public void registerHistoryChangeListener(
			final IHistoryChangeListener listener) {
		listeners.add(listener);
		if (currentHistory != null) {
			notifyHistoryChange(currentHistory);
		}
	}

	/**
	 * An {@link IHistoryChangeListener} can unregister itself via this method
	 * when it no longer wants to receive updates
	 * 
	 * @param listener
	 */
	public void unregisterHistoryChangeListener(
			final IHistoryChangeListener listener) {
		listeners.remove(listener);
	}

	/**
	 * Changes the current history to the specified {@link History} and notifies
	 * a history change ({@link AnimationSelector#notifyHistoryChange(History)})
	 * 
	 * @param history
	 */
	public void changeCurrentHistory(final History history) {
		currentHistory = history;
		notifyHistoryChange(history);
	}

	/**
	 * Adds the specified {@link History} history to the registry, registers
	 * itself as the {@link IAnimationListener} within the history, sets the
	 * current history to history, and notifies a history change (
	 * {@link AnimationSelector#notifyHistoryChange(History)})
	 * 
	 * @param history
	 */
	public void addNewHistory(final History history) {
		histories.add(history);
		history.registerAnimationListener(this);
		currentHistory = history;
		notifyHistoryChange(history);
	}

	/**
	 * Let all {@link IHistoryChangeListener}s know that the current history has
	 * changed
	 * 
	 * @param history
	 */
	public void notifyHistoryChange(final History history) {
		for (final IHistoryChangeListener listener : listeners) {
			listener.historyChange(history);
		}
	}

	/**
	 * @return the current {@link History}
	 */
	public History getCurrentHistory() {
		return currentHistory;
	}

	/**
	 * @return the list of {@link History} objects in the registry.
	 */
	public List<History> getHistories() {
		return histories;
	}

	/**
	 * @param history
	 * @return the {@link AbstractElement} model that corresponds to the given
	 *         {@link History}
	 */
	public AbstractElement getModel(final History history) {
		return history.getModel();
	}

	@Override
	public String toString() {
		return "Animations Registry";
	}

	/**
	 * notify all of the listeners using the current history
	 * {@link AnimationSelector#notifyHistoryChange(History)}
	 */
	public void refresh() {
		notifyHistoryChange(currentHistory);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * de.prob.statespace.IAnimationListener#currentStateChanged(de.prob.statespace
	 * .History, de.prob.statespace.History)
	 */
	@Override
	public void currentStateChanged(final History oldHistory,
			final History newHistory) {
		if (oldHistory.equals(currentHistory)) {
			notifyHistoryChange(newHistory);
		}
		int indexOf = histories.indexOf(oldHistory);
		histories.set(indexOf, newHistory);
		currentHistory = newHistory;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * de.prob.statespace.IAnimationListener#removeHistory(de.prob.statespace
	 * .History)
	 */
	@Override
	public void removeHistory(final History history) {
		remove(history);
		refresh();
	}

	private void remove(final History history) {
		if (!histories.contains(history)) {
			return;
		}
		if (currentHistory == history) {
			int indexOf = histories.indexOf(history);
			histories.remove(history);
			if (histories.isEmpty()) {
				currentHistory = null;
				return;
			}
			if (indexOf == histories.size()) {
				currentHistory = histories.get(indexOf - 1);
				return;
			}
			currentHistory = histories.get(indexOf);
			return;
		}
		histories.remove(history);
	}

}