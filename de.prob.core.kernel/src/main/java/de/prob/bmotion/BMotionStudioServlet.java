package de.prob.bmotion;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import de.prob.statespace.AnimationSelector;
import de.prob.statespace.Trace;
import de.prob.web.WebUtils;

@SuppressWarnings("serial")
@Singleton
public class BMotionStudioServlet extends HttpServlet {

	private static final int DEFAULT_BUFFER_SIZE = 10240; // 10KB
	
	private AnimationSelector selector;
	
	@Inject
	public BMotionStudioServlet(AnimationSelector selector) {
		this.selector = selector;
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {

		req.setCharacterEncoding("UTF-8");

		String uri = req.getRequestURI();
		String furl = uri.replace("/bms/", "");
		String[] split = furl.split("/");
		String filename = split[split.length - 1];
		String filepath = furl;

		// Set correct mimeType
		String mimeType = getServletContext().getMimeType(filepath);
		resp.setContentType(mimeType);

		// Prepare streams.
		BufferedInputStream input = null;
		BufferedOutputStream output = null;
		InputStream stream = new FileInputStream(filepath);

		// TODO: This is ugly ... we need a better method to check the file
		// type
		Trace currentTrace = selector.getCurrentTrace();
		if (filename.endsWith(".html") && currentTrace != null) {
			Map<String, Object> jsonDataForRendering = BMotionStudioUtil
					.getJsonDataForRendering(currentTrace);
			String render = WebUtils.render(filepath, jsonDataForRendering);
			stream = new ByteArrayInputStream(render.getBytes());
		}

		try {
			// Open streams.
			input = new BufferedInputStream(stream, DEFAULT_BUFFER_SIZE);
			output = new BufferedOutputStream(resp.getOutputStream(),
					DEFAULT_BUFFER_SIZE);
			// Write file contents to response.
			byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
			int length;
			while ((length = input.read(buffer)) > 0) {
				output.write(buffer, 0, length);
			}
			output.flush();
		} finally {
			// Gently close streams.
			// close(output);
			// close(input);
		}

	}

	// private void close(Closeable resource) {
	// if (resource != null) {
	// try {
	// resource.close();
	// } catch (IOException e) {
	// // Do your thing with the exception. Print it, log it or mail
	// // it.
	// e.printStackTrace();
	// }
	// }
	// }
	
}