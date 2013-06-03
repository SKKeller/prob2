package de.prob.statespace

import static org.mockito.Mockito.*
import spock.lang.Specification

class AnimationSelectorTest extends Specification {

	def history
	def selector
	def listener

	def setup() {
		history = mock(Trace.class);
		selector = new AnimationSelector();
		listener = new IAnimationChangedListener() {
					def count = 0;
					void historyChange(Trace arg0) {
						count++;
					};
				}
		selector.registerHistoryChangeListener(listener)
	}

	def "It is possible to register a listener"() {
		expect:
		selector.historyListeners.size() == 1
		selector.historyListeners.get(0).get() == listener
	}

	def "It is possible to notify the listener"() {
		when:
		selector.notifyHistoryChange(null)

		then:
		listener.count == 1
	}

	def "It is possible to add a new History"() {
		when:
		selector.addNewHistory(history)

		then:
		selector.histories.contains(history)
	}

	def "It is possible to change the current history"() {
		when:
		selector.changeCurrentHistory(history)

		then:
		selector.currentHistory == history
	}
}
