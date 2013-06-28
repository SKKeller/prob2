package de.prob.webconsole.servlets.visualizations;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import de.prob.visualization.VisualizationException;

/**
 * @author joy A basis class available to provide the management of sessions.
 *         When GET requests come to the {@link SessionBasedServlet}, these are
 *         forwarded to the specified {@link ISessionServlet}.
 * 
 */
public abstract class SessionBasedServlet extends HttpServlet {

	protected static int count = 0;
	Map<String, ISessionServlet> sessions = new HashMap<String, ISessionServlet>();

	@Override
	protected void doGet(final HttpServletRequest req,
			final HttpServletResponse resp) throws ServletException,
			IOException {
		String init = req.getParameter("init");
		String sId = req.getParameter("sessionId");
		if (init != null) {
			initializePage(req, resp);
		} else if (sId != null && sessions.containsKey(sId)) {
			sessions.get(sId).doGet(req, resp);
		} else {
			resp.sendError(HttpServletResponse.SC_NOT_FOUND);
		}
	}

	protected void initializePage(final HttpServletRequest req,
			final HttpServletResponse resp) throws IOException {
		resp.setContentType("text/html");

		String sId = req.getParameter("init");
		String w = req.getParameter("w");
		String h = req.getParameter("h");

		if (sessions.containsKey(sId)) {
			String html = getHTML(sId, w, h);

			PrintWriter out = resp.getWriter();
			out.print(html);
			out.close();
		} else {
			try {
				String s = loadSession(sId);
				if (s != null) {
					String html = getHTML(sId, w, h);
					PrintWriter out = resp.getWriter();
					out.print(html);
					out.close();
				} else {
					resp.sendError(HttpServletResponse.SC_NOT_FOUND);
				}
			} catch (VisualizationException e) {
				resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
						e.getMessage());
			}
		}
	}

	protected abstract String getHTML(String id, String w, String h);

	protected abstract String loadSession(String id)
			throws VisualizationException;

	protected String openSession(final String sessionId, final ISessionServlet s) {
		sessions.put(sessionId, s);
		return sessionId;
	}

	protected void closeSession(final String id) {
		sessions.remove(id);
	}

	public StateSpaceSession getSessionServlet(final String sessionId) {
		return (StateSpaceSession) sessions.get(sessionId);
	}
}
