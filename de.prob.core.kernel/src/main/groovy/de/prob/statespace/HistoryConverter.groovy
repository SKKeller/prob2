package de.prob.statespace

import de.prob.model.representation.AbstractModel


class HistoryConverter {
	def static File save(History history,String fileName) {
		def file = new File(fileName);
		file.newWriter()

		file << "<!-- Model for this trace has the following graph:"
		def model = history as AbstractModel;
		file << "${model.toString()} -->"

		file << "<trace>"
		file << "<model>"
		file << model.getModelFile().getAbsolutePath()
		file << "</model>"
		def trace = history as ArrayList
		trace.each {
			def op = it.edge
			if(op != null) {
				file << "<Operation name=\"${op.getName()}\">"
				op.getParams().each {
					param -> file << "<Parameter name=\"$param\"/>"
				}
				file << "<sha value=\"${op.sha()}\"/>"
				file << "</Operation>"
			}
		}
		file << "</trace>"

		file
	}

	def static File xmlToGroovy(String xmlFile, String outFile) {
		def file = new File(outFile)
		file.newWriter()

		file << '''{ m ->
    def next = { h, hash, name, args, strict ->
        def s = h as StateSpace
        def oldh = h
        def ns = s.getOutEdges(h.getCurrentState())
        def n = (ns.grep {it.sha() == hash})
	    if (n.isEmpty()) {
            if (strict) {
                assert false, 'Could not replay exact trace.'
            } else {
                println "Warning: Cannot find precise solution for nondeterministic assignments"
                h = h.add(name,args)
            }
        } else {
            h = h.add(n.first().id);
        }
        assert h != null, 'Could not find a sucessor state. Trace so far is ${oldh}. Missing successor state for ${name} with arguments ${args}'
        h
    }

    def h = m as History
'''

		def trace = new XmlSlurper().parse(xmlFile)

		trace.Operation.each {
			def sha = it.sha.getAt(0).@value.toString()
			def params = []
			it.Parameter.each {
				params << "\"${it.@name}\""
			}
			def name = "${it.@name}"
			if(name.startsWith("\$")) {
				name = "\\${name}"
			}
			file << "    h = next(h,\"${sha}\",\"${name}\",${params},true)\n"
		}

		file << "h  }"
	}

	def static History restore(AbstractModel model,String fileName) {
		def History h = model as History
		def StateSpace s = h as StateSpace

		def trace = new XmlSlurper().parse(fileName)

		trace.Operation.each {
			def sha = it.sha.getAt(0).@value.toString()
			def ops = s.getOutEdges(h.getCurrentState())
			def op = null
			ops.each {
				def state = s.getDest(it)
				if(state.hash == sha) {
					op = it
				}
			}

			if(op == null) {
				def params = []
						it.Parameter.each {
					params << "${it.@name}"
				}
				def name = "${it.@name}"
				h = h.add("${it.@name}", params)
			} else {
				h = h.add(op.id)
			}
		}

		h
	}
	
	def static xmlToSpock(def xmlFile) {
		def trace = new XmlSlurper().parse(xmlFile)
		
	}
	
	
}
