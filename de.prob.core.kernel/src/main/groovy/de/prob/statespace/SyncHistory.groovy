package de.prob.statespace




class SyncHistory {
	def List<History> histories
	def List<String> syncedOps

	def SyncHistory head
	def SyncHistory current
	def SyncHistory prev

	def SyncHistory(List<StateSpace> statespaces, syncedOps) {
		histories = []
		statespaces.each {
			s ->
			histories << new History(s)
		}
		this.syncedOps = syncedOps
		this.head = this
		this.current = head
		this.prev = null
	}

	def SyncHistory(histories,prev,syncedOps) {
		this.histories = histories
		this.head = this
		this.current = head
		this.prev = prev
		this.syncedOps = syncedOps
	}

	def SyncHistory(histories,head,current,prev,syncedOps) {
		this.histories = histories
		this.head = head
		this.current = current
		this.prev = prev
		this.syncedOps = syncedOps
	}

	def SyncHistory add(String syncedOp, List<String> params) {
		if(!syncedOps.contains(syncedOp)) {
			throw new IllegalArgumentException("The given operation has not been specified as a syncronized operation")
		}
		def map = new HashMap<History, String>()
		histories.each {
			history ->
			def op = history.getOp(syncedOp,params)
			if(op==null) {
				throw new IllegalArgumentException("Operation cannot be synced across the given histories")
			}
			map.put(history, op)
		}
		def newHistories = []
		histories.each {
			history ->
			newHistories << history.add(map.get(history))
		}
		return new SyncHistory(newHistories,this,syncedOps)
	}

	def SyncHistory add(String op, List<String> params, int index) {
		if(syncedOps.contains(op)) {
			return add(op,params)
		}
		def history = histories.get(index)
		def operation = history.getOp(op,params)
		history = history.add(operation)
		def newHistories = []
		histories.each {
			newHistories << it
		}
		newHistories.set(index, history)
		return new SyncHistory(newHistories,this,syncedOps)
	}

	def SyncHistory add(String opId, int index) {
		def history = histories.get(index)
		def op = history.s.getOp(opId)
		if(syncedOps.contains(op.getName())) {
			return add(op.getName(),op.getParams())
		}
		history = history.add(opId)
		def newHistories = []
		histories.each {
			newHistories << it
		}
		newHistories.set(index, history)
		return new SyncHistory(newHistories,this,syncedOps)
	}

	def SyncHistory add(int opId, int index) {
		return add(String.valueOf(opId),index)
	}

	def SyncHistory back() {
		if(prev != null)
		return new SyncHistory(prev.histories,head,prev,prev.prev,syncedOps)
		return this
	}

	def SyncHistory forward() {
		if(current != head) {
			SyncHistory p = head
			while( p.prev != current ) {
				p = p.prev
			}
			return new SyncHistory(p.histories,head,p,p.prev,syncedOps)
		}
		return this
	}

	def SyncHistory addOp(String op) {
		def newSyncedOps = []
		syncedOps.each {
			newSyncedOps << it
		}
		newSyncedOps.add(op)
		return new SyncHistory(histories,head,current,prev,newSyncedOps)
	}

	def String toString() {
		def sb = new StringBuilder()

		histories.each {
			history ->
			sb.append("${histories.indexOf(history)}: ${history.getRep()}\n")
		}

		def h = histories.get(0)
		def currentOpsOnH = h.s.getOutEdges(h.current.getCurrentState())
		def copy = new HashSet<OpInfo>(currentOpsOnH)

		currentOpsOnH.each {
			op ->
			if(syncedOps.contains(op.getName())) {
				histories.each {
					history ->
					def op2 = history.getOp(op.getName(),op.getParams())
					if(op2==null) {
						copy.remove(op)
					}
				}
			} else {
				copy.remove(op)
			}
		}

		sb.append("Operations:\n")
		sb.append("synced: ${copy}\n")
		histories.each {
			history ->
			sb.append("${histories.indexOf(history)}: ")
			def o = history.s.getOutEdges(history.current.getCurrentState())
			def list = []
			o.each {
				if(!syncedOps.contains(it.getName())) {
				list << "${it.getId()}: ${it.getRep(s.getModel())}"
				}
			}
			sb.append(list)
			sb.append("\n")
		}
		return sb.toString()
	}
}
