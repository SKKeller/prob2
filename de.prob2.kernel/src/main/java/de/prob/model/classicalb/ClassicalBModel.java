package de.prob.model.classicalb;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import com.google.inject.Inject;
import com.google.inject.Provider;

import de.be4.classicalb.core.parser.analysis.prolog.RecursiveMachineLoader;
import de.be4.classicalb.core.parser.node.Start;
import de.prob.animator.domainobjects.ClassicalB;
import de.prob.animator.domainobjects.IEvalElement;
import de.prob.model.representation.AbstractElement;
import de.prob.model.representation.AbstractModel;
import de.prob.model.representation.DependencyGraph;
import de.prob.model.representation.Machine;
import de.prob.model.representation.ModelElementList;
import de.prob.statespace.FormalismType;
import de.prob.statespace.StateSpace;

public class ClassicalBModel extends AbstractModel {

	private ClassicalBMachine mainMachine = null;
	private final HashSet<String> done = new HashSet<String>();

	@Inject
	public ClassicalBModel(final Provider<StateSpace> ssProvider) {
		super(ssProvider);
	}

	public DependencyGraph initialize(final Start mainast,
			final RecursiveMachineLoader rml, final File modelFile) {

		this.modelFile = modelFile;

		final DependencyGraph graph = new DependencyGraph();

		final DomBuilder d = new DomBuilder(false);
		mainMachine = d.build(mainast);

		extractModelDir(modelFile, mainMachine.getName());

		graph.addVertex(mainMachine.getName());
		ModelElementList<ClassicalBMachine> machines = new ModelElementList<ClassicalBMachine>();
		machines.add(mainMachine);

		boolean fpReached;

		do {
			fpReached = true;
			final Set<String> vertices = graph.getVertices();
			for (final String machineName : vertices) {
				final Start ast = rml.getParsedMachines().get(machineName);
				if (!done.contains(machineName)) {
					ast.apply(new DependencyWalker(machineName, machines,
							graph, rml.getParsedMachines()));
					done.add(machineName);
					fpReached = false;
				}
			}
		} while (!fpReached);
		this.graph = graph;

		put(Machine.class, machines);

		for (ClassicalBMachine classicalBMachine : machines) {
			components.put(classicalBMachine.getName(), classicalBMachine);
		}

		freeze();
		return graph;
	}

	public ClassicalBMachine getMainMachine() {
		return mainMachine;
	}

	@Override
	public AbstractElement getMainComponent() {
		return getMainMachine();
	}

	@Override
	public IEvalElement parseFormula(final String formula) {
		return new ClassicalB(formula);
	}

	@Override
	public FormalismType getFormalismType() {
		return FormalismType.B;
	}
}
