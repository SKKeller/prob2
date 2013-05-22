package de.prob.webconsole.servlets.visualizations;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.inject.Inject;
import com.google.inject.Singleton;

import de.prob.statespace.AnimationSelector;
import de.prob.statespace.IModelChangedListener;
import de.prob.statespace.StateSpace;
import de.prob.visualization.AnimationNotLoadedException;
import de.prob.visualization.AnimationProperties;
import de.prob.visualization.DynamicTransformer;
import de.prob.visualization.HTMLResources;
import de.prob.visualization.Transformer;
import de.prob.visualization.VisualizationSelector;

@SuppressWarnings("serial")
@Singleton
public class StateSpaceServlet extends SessionBasedServlet implements
		IModelChangedListener {

	private StateSpace currentStateSpace;
	private final VisualizationSelector visualizations;
	private final AnimationProperties properties;

	@Inject
	public StateSpaceServlet(final AnimationSelector animations,
			final VisualizationSelector visualizations,
			final AnimationProperties properties) {
		this.visualizations = visualizations;
		this.properties = properties;
		animations.registerModelChangedListener(this);
	}

	@Override
	public void modelChanged(final StateSpace s) {
		currentStateSpace = s;
	}

	public String openSession(final String sessionId)
			throws AnimationNotLoadedException {
		if (currentStateSpace == null) {
			throw new AnimationNotLoadedException(
					"Could not start state space visualization because no animation is loaded");
		}
		StateSpaceSession session = new StateSpaceSession(sessionId,
				currentStateSpace, properties);
		super.openSession(sessionId, session);
		visualizations.registerSession(sessionId, session);
		return sessionId;
	}

	@Override
	protected String getHTML(final String id) {
		return HTMLResources.getSSVizHTML(id);
	}

	@Override
	protected String loadSession(final String id) {
		if (currentStateSpace == null) {
			return null;
		}

		String propFile = properties.getPropFileFromModelFile(currentStateSpace
				.getModel().getModelFile().getAbsolutePath());
		Properties props = properties.getProperties(propFile);
		String json = props.getProperty(id);

		JsonParser parser = new JsonParser();
		JsonObject parsed = parser.parse(json).getAsJsonObject();
		int mode = parsed.get("mode").getAsInt();

		List<String> disabled = new ArrayList<String>();
		for (JsonElement jsonElement : parsed.get("disabled").getAsJsonArray()) {
			disabled.add(jsonElement.getAsString());
		}

		String expression = parsed.get("expr").getAsString();

		List<Transformer> transformers = new ArrayList<Transformer>();
		for (JsonElement jsonElement : parsed.get("transformers")
				.getAsJsonArray()) {
			transformers.add(DynamicTransformer.deserialize(
					jsonElement.getAsString(), currentStateSpace));
		}

		StateSpaceSession session = new StateSpaceSession(id,
				currentStateSpace, mode, disabled, expression, transformers,
				properties);
		super.openSession(id, session);
		visualizations.registerSession(id, session);
		return id;
	}

}