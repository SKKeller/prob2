package de.prob.webconsole.servlets.visualizations;

import java.util.Properties;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import de.prob.animator.domainobjects.EvalElementFactory;
import de.prob.animator.domainobjects.IEvalElement;
import de.prob.statespace.AnimationSelector;
import de.prob.visualization.AnimationNotLoadedException;
import de.prob.visualization.AnimationProperties;
import de.prob.visualization.HTMLResources;
import de.prob.visualization.VisualizationException;
import de.prob.visualization.VisualizationSelector;

@Singleton
public class PredicateServlet extends SessionBasedServlet {

	private final AnimationSelector animations;
	private final VisualizationSelector visualizations;
	private final AnimationProperties properties;
	private final EvalElementFactory deserializer;

	@Inject
	public PredicateServlet(final AnimationSelector animations,
			final VisualizationSelector visualizations,
			final AnimationProperties properties,
			final EvalElementFactory deserializer) {
		this.visualizations = visualizations;
		this.animations = animations;
		this.properties = properties;
		this.deserializer = deserializer;
	}

	public String openSession(final String sessionId, final IEvalElement formula)
			throws AnimationNotLoadedException, VisualizationException {
		if (animations.getCurrentHistory() == null) {
			throw new AnimationNotLoadedException("Could not visualize "
					+ formula.getCode() + " because no animation is loaded");
		}
		try {
			PredicateSession session = new PredicateSession(animations, formula);
			String propFile = properties.getPropFileFromModelFile(animations
					.getCurrentHistory().getModel().getModelFile()
					.getAbsolutePath());
			properties.setProperty(propFile, sessionId, formula.serialized());
			super.openSession(sessionId, session);
			visualizations.registerSession(sessionId, session);
		} catch (Throwable e) {
			throw new VisualizationException(
					"Could not create predicate visualization because "
							+ e.getClass().getSimpleName() + " with message "
							+ e.getMessage() + " was thrown.");
		}
		return sessionId;
	}

	@Override
	protected String getHTML(final String id, final String w, final String h) {
		return HTMLResources.getPredicateHTML(id, w, h);
	}

	@Override
	protected String loadSession(final String id) throws VisualizationException {
		if (animations.getCurrentHistory() != null) {
			String propFile = properties.getPropFileFromModelFile(animations
					.getCurrentHistory().getModel().getModelFile()
					.getAbsolutePath());
			Properties props = properties.getProperties(propFile);
			String formula = props.getProperty(id);
			System.out.println(formula);
			if (formula != null) {
				IEvalElement iEvalElement = deserializer.deserialize(formula);
				try {
					return openSession(id, iEvalElement);
				} catch (AnimationNotLoadedException e) {
					return null;
				}
			}

		}
		return null;
	}
}
