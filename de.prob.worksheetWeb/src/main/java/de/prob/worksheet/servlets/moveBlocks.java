/**
 * 
 */
package de.prob.worksheet.servlets;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.inject.Inject;

import de.prob.worksheet.WorksheetDocument;

/**
 * @author Rene
 * 
 */
public class moveBlocks extends HttpServlet {
	/**
	 * 
	 */
	private static final long	serialVersionUID	= -9079077179155960240L;
	private WorksheetDocument	doc;

	/**
	 * 
	 */
	@Inject
	public moveBlocks() {
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
		this.doPost(req, resp);

		return;
	}

	/* (non-Javadoc)
	 * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doPost(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
		// Get Session and needed Attributes
		final HttpSession session = req.getSession();
		final String wsid = req.getParameter("worksheetSessionID");
		if (session.isNew()) {
			System.err.println("No worksheet Document is initialized (first a call to newDocument Servlet is needed)");
			resp.getWriter().write("Error: No document is initialized");
			return;
		}
		this.doc = (WorksheetDocument) req.getSession().getAttribute("WorksheetDocument" + wsid);

		final ObjectMapper mapper = new ObjectMapper();

		final String[] ids = mapper.readValue(req.getParameter("ids"), String[].class);
		final int index = Integer.parseInt(req.getParameter("index"));

		this.doc.moveBlocksTo(ids, index);

		// TODO add result msg
		return;
	}
}
