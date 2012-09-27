/** 
 * (c) 2009 Lehrstuhl fuer Softwaretechnik und Programmiersprachen, 
 * Heinrich Heine Universitaet Duesseldorf
 * This software is licenced under EPL 1.0 (http://www.eclipse.org/org/documents/epl-v10.html) 
 * */

package de.prob.ui.eventb.internal;

import java.util.ArrayList;
import java.util.List;

import org.eventb.core.ISCContext;
import org.eventb.core.ISCContextRoot;
import org.eventb.core.ISCExtendsContext;
import org.eventb.core.ISCInternalContext;
import org.rodinp.core.RodinDBException;
import de.prob.prolog.output.IPrologTermOutput;

public final class EventBContextTranslator extends EventBTranslator {

	private final ISCContext context;

	public static void create(final ISCContextRoot context,
			final IPrologTermOutput pto) throws IllegalArgumentException {
		EventBContextTranslator translator = new EventBContextTranslator(
				context);
		translator.constructTranslation(pto);
	}

	public static void create(final ISCInternalContext context,
			final IPrologTermOutput pto) throws IllegalArgumentException {
		EventBContextTranslator translator = new EventBContextTranslator(
				context);
		translator.constructTranslation(pto);
	}

	// ############################################################################################

	private EventBContextTranslator(final ISCInternalContext context) {
		super(context);
		this.context = context;
	}

	private EventBContextTranslator(final ISCContextRoot context) {
		super(context);
		this.context = context;
	}

	private void constructTranslation(final IPrologTermOutput pto)
			throws IllegalArgumentException {
		List<ContextTranslator> translators = new ArrayList<ContextTranslator>();
//		translators.add(ContextTranslator.create(context));
		if (context instanceof ISCContextRoot) {
			ISCContextRoot root = (ISCContextRoot) context;
			collectContexts(translators, new ArrayList<String>(), root);
		}
		printProlog(new ArrayList<ModelTranslator>(), translators, pto);
	}

	private void collectContexts(final List<ContextTranslator> translatorMap,
			final List<String> processed, final ISCContextRoot context)
			throws IllegalArgumentException {
		String name = context.getElementName();
		if (!processed.contains(name)) {
			processed.add(name);
			translatorMap.add(ContextTranslator.create(context));
			ISCExtendsContext[] clauses;
			try {
				clauses = context.getSCExtendsClauses();
				for (ISCExtendsContext extendsContext : clauses) {
					ISCContextRoot element = extendsContext
							.getAbstractSCContext();
					collectContexts(translatorMap, processed, element);
				}
			} catch (RodinDBException e) {
				throw new IllegalArgumentException(e);
			}
		}
	}

	@Override
	protected void printFlowInformation(final IPrologTermOutput pout) {
	}

}
