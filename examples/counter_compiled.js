// Requires ClaudeLang runtime to be loaded



// Component definitions


ClaudeLang.defineComponent('Button', {
    props: {"text": {"type": "string", "required": true, "default": null}, "onClick": {"type": "function", "required": false, "default": null}},
    render: (props) => ClaudeLang.primitives['dom:h']("button", ClaudeLang.primitives['set']({}, "onClick", ClaudeLang.primitives['get'](props, "onClick")), [ClaudeLang.primitives['dom:text'](ClaudeLang.primitives['get'](props, "text"))])
})


ClaudeLang.defineComponent('Counter', {
    props: {"initial": {"type": "number", "required": false, "default": "85622d8b-e664-457e-8c0b-7a3d1b9c6517"}},
    render: (props) => (() => { const count = ClaudeLang.performEffect('STATE', 'reactive-ref', [ClaudeLang.primitives['get'](props, "initial")]); return (() => { const increment = () => ClaudeLang.performEffect('STATE', 'reactive-update', [count, (n) => ClaudeLang.primitives['+'](n, 1)]); const decrement = () => ClaudeLang.performEffect('STATE', 'reactive-update', [count, (n) => ClaudeLang.primitives['-'](n, 1)]); return ClaudeLang.primitives['dom:h']("div", {}, ClaudeLang.primitives['cons'](ClaudeLang.primitives['dom:h']("h2", {}, [ClaudeLang.primitives['dom:text'](ClaudeLang.primitives['concat']("Count: ", ClaudeLang.primitives['to-string'](ClaudeLang.performEffect('STATE', 'reactive-get', [count]))))]), ClaudeLang.primitives['cons'](ClaudeLang.primitives['ui:create']("Button", ClaudeLang.primitives['set']({}, "text", "+", "onClick", increment)), [ClaudeLang.primitives['ui:create']("Button", ClaudeLang.primitives['set']({}, "text", "-", "onClick", decrement))]))); })(); })()
})

ClaudeLang.performEffect('DOM', 'render', [ClaudeLang.primitives['ui:create']("Counter", ClaudeLang.primitives['set']({}, "initial", 10)), "#app"])