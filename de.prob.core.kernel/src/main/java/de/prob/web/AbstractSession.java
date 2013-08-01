package de.prob.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Callable;

import javax.servlet.AsyncContext;
import javax.servlet.ServletResponse;

import org.eclipse.jetty.io.UncheckedIOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.prob.web.data.Message;
import de.prob.web.data.SessionResult;

public abstract class AbstractSession implements ISession {

	private final UUID id;
	private final List<AsyncContext> clients = Collections
			.synchronizedList(new ArrayList<AsyncContext>());
	private final ArrayList<Message> responses = new ArrayList<Message>();

	private final Logger logger = LoggerFactory
			.getLogger(AbstractSession.class);

	public AbstractSession() {
		this.id = UUID.randomUUID();
	}

	public AbstractSession(UUID id) {
		this.id = id;
	}

	@Override
	public Callable<SessionResult> command(
			final Map<String, String[]> parameterMap) {
		String cmd = get(parameterMap, "cmd");
		final ISession delegate = this;
		Class<? extends AbstractSession> clazz = this.getClass();
		try {
			final Method method = clazz.getMethod(cmd, Map.class);
			return new Callable<SessionResult>() {
				@Override
				public SessionResult call() throws Exception {
					Object result = method.invoke(delegate, parameterMap);
					return new SessionResult(delegate, result);
				}
			};
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		}
		return new Callable<SessionResult>() {
			@Override
			public SessionResult call() throws Exception {
				return new SessionResult(delegate, "");
			}
		};
	}

	@Override
	public abstract String html(String clientid,
			Map<String, String[]> parameterMap);

	@Override
	public UUID getSessionUUID() {
		return id;
	}

	public String get(Map<String, String[]> parameterMap, String key) {
		String[] strings = parameterMap.get(key);
		if (strings.length != 1)
			throw new IllegalArgumentException(
					"get Method is only applicable to simple key-Value pairs");
		return strings[0];
	}

	@Override
	public void submit(Object result) {
		Message message = new Message(responses.size() + 1, result);
		responses.add(message);
		String json = WebUtils.toJson(message);
		synchronized (clients) {
			for (AsyncContext context : clients) {
				send(json, context);
			}
			clients.clear();
		}

		HashSet<String> a = new HashSet<String>();

		a.add("foo");
		a.size();
	}

	private void send(String json, AsyncContext context) {
		ServletResponse response = context.getResponse();
		try {
			PrintWriter writer = response.getWriter();
			writer.print(json);
			writer.flush();
			writer.close();
			context.complete();
		} catch (IOException e) {
			logger.error("Could not get the writer for connection.", e);
		} catch (UncheckedIOException e) {
			logger.debug("Exception occured while sending data. This happens if timeouts occured. Ignoring and continuing.");
		} catch (IllegalStateException e) {
			logger.debug("Exception occured while completing asynchronous call. This happens if timeouts occured. Ignoring and continuing.");
		}
	}

	@Override
	public void registerClient(String client, int lastinfo, AsyncContext context) {
		logger.trace("Register {} Lastinfo {}", client, lastinfo);
		if (lastinfo < responses.size()) {
			outOfDateCall(client, lastinfo, context);
		} else {
			registerContext(context);
		}
	}

	@Override
	public int getResponseCount() {
		return responses.size();
	}

	@Override
	public void outOfDateCall(String client, int lastinfo, AsyncContext context) {
		// Default is to not send old messages
		registerContext(context);
	}

	private void registerContext(AsyncContext context) {
		synchronized (clients) {
			clients.add(context);
		}
	}
}