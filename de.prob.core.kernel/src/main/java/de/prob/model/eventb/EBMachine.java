package de.prob.model.eventb;

import java.util.Arrays;

import de.prob.animator.domainobjects.EventB;
import de.prob.model.representation.IEntity;
import de.prob.model.representation.Label;

public class EBMachine extends Label {

	final public Label variables = new Label("Variables");
	final public Label invariants = new Label("Invariants");
	final public Label variant = new Label("Variant");
	final public Label events = new Label("Events");

	public EBMachine(final String name) {
		super(name);
		children.addAll(Arrays.asList(new IEntity[] { variables, invariants,
				variant, events }));
	}

	public void addVariable(final String variable) {
		variables.addChild(new EventB(variable));
	}

	public void addInvariant(final String invariant) {
		invariants.addChild(new EventB(invariant));
	}

	public void addVariant(final String variant) {
		this.variant.addChild(new EventB(variant));
	}

	public void addEvent(final String event) {
		events.addChild(new EBEvent(event));
	}
}
