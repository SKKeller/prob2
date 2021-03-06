/**
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, Heinrich
 * Heine Universitaet Duesseldorf This software is licenced under EPL 1.0
 * (http://www.eclipse.org/org/documents/epl-v10.html)
 * */

package de.prob.animator.command;

import static de.prob.animator.domainobjects.EvalElementType.PREDICATE;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.prob.animator.domainobjects.IEvalElement;
import de.prob.parser.BindingGenerator;
import de.prob.parser.ISimplifiedROMap;
import de.prob.prolog.output.IPrologTermOutput;
import de.prob.prolog.term.CompoundPrologTerm;
import de.prob.prolog.term.ListPrologTerm;
import de.prob.prolog.term.PrologTerm;
import de.prob.statespace.Transition;
import de.prob.statespace.StateSpace;

/**
 * Command to execute an event that has not been enumerated by ProB.
 * 
 * @author Jens Bendisposto
 * 
 */
public final class GetOperationByPredicateCommand extends AbstractCommand
		implements IStateSpaceModifier {

	Logger logger = LoggerFactory
			.getLogger(GetOperationByPredicateCommand.class);
	private static final String NEW_STATE_ID_VARIABLE = "NewStateID";
	private static final String ERRORS = "Errors";
	private final IEvalElement evalElement;
	private final String stateId;
	private final String name;
	private final List<Transition> operations = new ArrayList<Transition>();
	private final List<String> errors = new ArrayList<String>();
	private final int nrOfSolutions;
	private final StateSpace s;

	public GetOperationByPredicateCommand(final StateSpace s,
			final String stateId, final String name,
			final IEvalElement predicate, final int nrOfSolutions) {
		this.s = s;
		this.stateId = stateId;
		this.name = name;
		this.nrOfSolutions = nrOfSolutions;
		evalElement = predicate;
		if (!evalElement.getKind().equals(PREDICATE.toString())) {
			throw new IllegalArgumentException("Formula must be a predicate: "
					+ predicate);
		}
	}

	/**
	 * This method is called when the command is prepared for sending. The
	 * method is called by the Animator class, most likely it is not interesting
	 * for other classes.
	 * 
	 * @throws ProBException
	 * 
	 * @see de.prob.animator.command.AbstractCommand#writeCommand(de.prob.prolog.output.IPrologTermOutput)
	 */
	@Override
	public void writeCommand(final IPrologTermOutput pto) {
		pto.openTerm("prob2_execute_custom_operations")
				.printAtomOrNumber(stateId).printAtom(name);
		evalElement.printProlog(pto);
		pto.printNumber(nrOfSolutions);
		pto.printVariable(NEW_STATE_ID_VARIABLE);
		pto.printVariable(ERRORS).closeTerm();
	}

	/**
	 * This method is called to extract relevant information from ProB's answer.
	 * The method is called by the Animator class, most likely it is not
	 * interesting for other classes.
	 * 
	 * 
	 * 
	 * @see de.prob.animator.command.AbstractCommand#writeCommand(de.prob.prolog.output.IPrologTermOutput)
	 */
	@Override
	public void processResult(
			final ISimplifiedROMap<String, PrologTerm> bindings) {
		ListPrologTerm list = BindingGenerator.getList(bindings
				.get(NEW_STATE_ID_VARIABLE));

		for (PrologTerm prologTerm : list) {
			CompoundPrologTerm cpt = BindingGenerator.getCompoundTerm(
					prologTerm, 4);
			operations.add(Transition.createTransitionFromCompoundPrologTerm(s, cpt));
		}

		ListPrologTerm errors = BindingGenerator.getList(bindings.get(ERRORS));
		for (PrologTerm prologTerm : errors) {
			this.errors.add(prologTerm.getFunctor());
		}
	}

	@Override
	public List<Transition> getNewTransitions() {
		return operations;
	}

	public List<String> getErrors() {
		return errors;
	}

	public boolean hasErrors() {
		return !errors.isEmpty();
	}

}
