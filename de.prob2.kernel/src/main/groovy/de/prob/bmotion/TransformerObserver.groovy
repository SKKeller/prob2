package de.prob.bmotion

import com.google.gson.Gson
import groovy.transform.TupleConstructor

@TupleConstructor
class TransformerObserver extends BMotionObserver implements BMotionTransformer {

    def selector

    def attributes = [:]

    def styles = [:]

    def content

    private final Gson g = new Gson()

    def static TransformerObserver make(Closure cls) {
        new TransformerObserver().with cls
    }

    def TransformerObserver selector(selector) {
        this.selector = selector
        this
    }

    def TransformerObserver set(String name, Object value) {
        (name == "content" || name == "text") ? content = value : attributes.put(name, value)
        this
    }

    def TransformerObserver attr(String name, Object value) {
        set(name, value)
    }

    def TransformerObserver style(String name, Object value) {
        styles.put(name, value)
        this
    }

    def List<TransformerObject> update(BMotion bms) {
        def t = new TransformerObject((selector instanceof Closure) ? selector() : selector)
        t.attributes = attributes.collectEntries { kv ->
            (kv.value instanceof Closure) ? [kv.key, kv.value()] : [kv.key, kv.value
            ]
        }
        t.styles = styles.collectEntries { kv ->
            (kv.value instanceof Closure) ? [kv.key, kv.value()] : [kv.key, kv.value
            ]
        }
        t.content = (content instanceof Closure) ? content() : content
        [t]
    }

    @Override
    def apply(BMotion bms) {
        bms.submit([cmd: "bms.applyTransformers", transformers: g.toJson(update(bms))])
    }

}