package de.prob.model.eventb;

import java.util.List;

import de.prob.model.representation.Action;
import de.prob.model.representation.BEvent;
import de.prob.model.representation.Guard;

public class Event extends BEvent {

	private final EventType type;

	public enum EventType {
		ORDINARY, CONVERGENT, ANTICIPATED
	}

	public Event(final String name, final EventType type) {
		super(name);
		this.type = type;
	}

	public void addRefines(final List<Event> refines) {
		put(Event.class, refines);
	}

	public void addGuards(final List<EventBGuard> guards) {
		put(Guard.class, guards);
	}

	public void addActions(final List<EventBAction> actions) {
		put(Action.class, actions);
	}

	public void addWitness(final List<Witness> witness) {
		put(Witness.class, witness);
	}

	public void addParameters(final List<EventParameter> parameters) {
		put(EventParameter.class, parameters);
	}

	public EventType getType() {
		return type;
	}
}