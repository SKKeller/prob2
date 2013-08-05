/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.prob.ui.eventb.internal;

import java.util.ArrayList;
import java.util.List;

import org.eventb.core.ISCInternalContext;
import org.eventb.core.ISCMachineRoot;
import org.rodinp.core.IRodinFile;
import org.rodinp.core.RodinDBException;

import de.prob.prolog.output.IPrologTermOutput;

public final class EventBMachineTranslator extends EventBTranslator {

	private final ISCMachineRoot machine;

	/*
	 * (non-Javadoc)
	 * 
	 * @see de.prob.core.translator.ITranslator#getPrologTerm()
	 */
	// public String getPrologTerm() {
	// return term;
	// }

	public static void create(final ISCMachineRoot machine,
			final IPrologTermOutput pto) throws IllegalArgumentException {
		EventBMachineTranslator translator = new EventBMachineTranslator(
				machine);
		translator.constructTranslation(pto);
	}

	// ############################################################################################

	private EventBMachineTranslator(final ISCMachineRoot machine) {
		super(machine);
		this.machine = machine;
	}

	private void constructTranslation(final IPrologTermOutput pto)
			throws IllegalArgumentException {
		// ISCMachineRoot currentMachine = machine;
		//
		// ModelTranslator root = ModelTranslator.create(currentMachine);
		//
		// final List<ModelTranslator> mchTranslators =
		// getModelTranslators(root);
		// final List<ContextTranslator> ctxTranslators =
		// getContextTranslators(root
		// .getContextDependencies());
		//
		// term = printProlog(mchTranslators, ctxTranslators);

		List<ISCMachineRoot> roots = collectRefinementChain();
		final List<ModelTranslator> mchTranslators = getModelTranslators(roots);
		final List<ContextTranslator> ctxTranslators = getContextTranslators(roots);
		printProlog(mchTranslators, ctxTranslators, pto);
	}

	private List<ISCMachineRoot> collectRefinementChain()
			throws IllegalArgumentException {
		List<ISCMachineRoot> roots = new ArrayList<ISCMachineRoot>();

		try {
			buildRefinementChain(machine, roots);
		} catch (RodinDBException e) {
			throw new IllegalArgumentException(e);
		}
		return roots;
	}

	private void buildRefinementChain(final ISCMachineRoot element,
			final List<ISCMachineRoot> list) throws RodinDBException {
		list.add(element);
		IRodinFile[] abst = element.getAbstractSCMachines();
		for (IRodinFile rodinFile : abst) {
			buildRefinementChain((ISCMachineRoot) rodinFile.getRoot(), list);
		}
	}

	private List<ModelTranslator> getModelTranslators(
			final List<ISCMachineRoot> roots) throws IllegalArgumentException {
		final List<ModelTranslator> mchTranslators = new ArrayList<ModelTranslator>();
		for (ISCMachineRoot iscMachineRoot : roots) {
			mchTranslators.add(ModelTranslator.create(iscMachineRoot));
		}
		return mchTranslators;
	}

	private List<ContextTranslator> getContextTranslators(
			final List<ISCMachineRoot> models)
			throws IllegalArgumentException {
		final List<ContextTranslator> translators = new ArrayList<ContextTranslator>();
		final List<String> processed = new ArrayList<String>();
		for (ISCMachineRoot m : models) {
			ISCInternalContext[] seenContexts;
			try {
				seenContexts = m.getSCSeenContexts();
				for (ISCInternalContext seenContext : seenContexts) {
					collectContexts(translators, processed, seenContext);
				}
			} catch (RodinDBException e) {
				throw new IllegalArgumentException(e);
			}
		}
		return translators;
	}

	private void collectContexts(final List<ContextTranslator> translatorMap,
			final List<String> processed, final ISCInternalContext context)
			throws IllegalArgumentException {
		String name = context.getElementName();
		if (!processed.contains(name)) {
			processed.add(name);
			translatorMap.add(ContextTranslator.create(context));
		}
	}

	@Override
	protected void printFlowInformation(final IPrologTermOutput pout) {
//		try {
//			FlowAnalysis flowAnalysis = new FlowAnalysis(this.machine);
//			flowAnalysis.printGraph(pout);
//		} catch (Exception e) {
//			final String message = "Error while constructing Flow Information: "
//					+ e.getLocalizedMessage();
//			Logger.notifyUser(message, e);
//		}
	}
}
