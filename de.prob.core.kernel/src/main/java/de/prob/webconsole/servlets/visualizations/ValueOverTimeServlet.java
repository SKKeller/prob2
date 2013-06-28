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

@SuppressWarnings("serial")
@Singleton
public class ValueOverTimeServlet extends SessionBasedServlet {

	private final AnimationSelector animations;
	private final VisualizationSelector visualizations;
	private final AnimationProperties properties;
	private final EvalElementFactory deserializer;

	@Inject
	public ValueOverTimeServlet(final AnimationSelector animations,
			final VisualizationSelector visualizations,
			final AnimationProperties properties,
			final EvalElementFactory deserializer) {
		this.animations = animations;
		this.visualizations = visualizations;
		this.properties = properties;
		this.deserializer = deserializer;
	}

	public String openSession(final String sessionId,
			final IEvalElement formula, final IEvalElement time)
			throws AnimationNotLoadedException, VisualizationException {
		if (animations.getCurrentTrace() == null) {
			throw new AnimationNotLoadedException("Could not visualize "
					+ formula.getCode() + " because no animation is loaded");
		}
		try {
			ValueOverTimeSession session = new ValueOverTimeSession(sessionId,
					formula, time, animations, properties);
			super.openSession(sessionId, session);
			visualizations.registerSession(sessionId, session);
		} catch (Exception e) {
			throw new VisualizationException(
					"Could not create Value Over Time Session. Thrown: "
							+ e.getClass().getSimpleName() + ": "
							+ e.getMessage());
		}
		return sessionId;
	}

	@Override
	protected String getHTML(final String id, final String w, final String h) {
		return HTMLResources.getValueVsTimeHTML(id, w, h);
	}

	@Override
	protected String loadSession(final String id) throws VisualizationException {
		if (animations.getCurrentTrace() != null) {
			String propFile = properties.getPropFileFromModel(animations
					.getCurrentTrace().getModel());
			Properties props = properties.getProperties(propFile);
			String json = props.getProperty(id);
			if (json != null) {
				// System.out.println(json);
				if (animations.getCurrentTrace() == null) {
					return null;
				}
				try {
					ValueOverTimeSession session = new ValueOverTimeSession(id,
							json, animations, properties, deserializer);
					super.openSession(id, session);
					visualizations.registerSession(id, session);
				} catch (Exception e) {
					throw new VisualizationException(
							"Could not create Value Over Time Session. Thrown: "
									+ e.getClass().getSimpleName() + ": "
									+ e.getMessage());
				}
				return id;
			}
		}
		return null;
	}

}
