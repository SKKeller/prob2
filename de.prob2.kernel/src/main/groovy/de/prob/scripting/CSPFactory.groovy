package de.prob.scripting


import com.google.inject.Inject
import com.google.inject.Provider

import de.be4.classicalb.core.parser.exceptions.BException
import de.prob.animator.command.ComposedCommand
import de.prob.animator.command.LoadCSPCommand
import de.prob.animator.command.SetPreferenceCommand
import de.prob.animator.command.StartAnimationCommand
import de.prob.model.representation.CSPModel

class CSPFactory extends ModelFactory {

	private final Provider<CSPModel> modelCreator;

	@Inject
	public CSPFactory(final Provider<CSPModel> modelProvider, FileHandler fileHandler) {
		super(fileHandler)
		this.modelCreator = modelProvider
	}

	public CSPModel load(final File f, Map<String, String> prefs) throws IOException, BException {
		CSPModel cspModel = modelCreator.get()

		cspModel.init(readFile(f),f)
		startAnimation(cspModel, f, getPreferences(cspModel, prefs))
		return cspModel;
	}

	private String readFile(File f) {
		return f.getText();
	}

	private void startAnimation(final CSPModel cspModel, final File f, final Map<String, String> prefs) {
		def cmds = [];

		prefs.each { k,v -> cmds << new SetPreferenceCommand(k, v) }

		def loadcmd = new LoadCSPCommand(f.getAbsolutePath());
		cmds << loadcmd
		cmds << new StartAnimationCommand()

		cspModel.getStatespace().execute(new ComposedCommand(cmds));
		cspModel.getStatespace().setLoadcmd(loadcmd);
	}
}