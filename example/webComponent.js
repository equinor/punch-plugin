// adapted from https://github.com/PixelsCommander/ReactiveElements

const camelize = str => {
    // adapted from https://stackoverflow.com/questions/2970525/converting-any-string-into-camel-case#2970667
    return str
        .toLowerCase()
        .replace(/[-_]+/g, ' ')
        .replace(/[^\w\s]/g, '')
        .replace(/ (.)/g, firstChar => firstChar.toUpperCase())
        .replace(/ /g, '')
}

const getProps = el => {
    const props = {}

    for (let i = 0; i < el.attributes.length; i++) {
        const attribute = el.attributes[i]
        const name = camelize(attribute.name)
        props[name] = attribute.value
    }
    return props
}

const webComponent = {
    register(
        name,
        ElmComponent,
        { staticFlags = {},
            setupPorts = () => { },
            onDetached = () => { },
            mapFlags = flags => flags,
            onSetupError,
        } = {}
    ) {
        class ElmElement extends HTMLElement {
            constructor() {
                const self = super();
                self._queue = []
                return self;
            }

            connectedCallback() {
                const context = {}
                try {
                    let props = Object.assign({}, getProps(this), staticFlags)
                    if (Object.keys(props).length === 0) props = undefined

                    const flags = mapFlags(props)
                    context.flags = flags
                    const parentDiv = this.attachShadow({ mode: 'open' });
                    const elmDiv = document.createElement('div')
                    parentDiv.innerHTML = ''
                    parentDiv.appendChild(elmDiv)
                    this._app =
                        ElmComponent.init({
                            flags,
                            node: elmDiv,
                        })
                    this.subscribe = this._app.ports.toJs.subscribe
                    this.send = this._app.ports.fromJs.send
                    setupPorts(this._app.ports)
                    this._queue.map(msg => this._app.ports.fromJs.send(msg))

                } catch (error) {
                    if (onSetupError) {
                        onSetupError(error, context)
                    } else {
                        console.error(
                            `Error from elm-web-components registering ${name}`,
                            'You can pass an `onSetupError` to handle these.',
                            error
                        )
                    }
                }
            }

            disconnectedCallback() {
                onDetached()
            }

            set punchlist(punchList) {
                if (this.hasOwnProperty("_app")) {
                    this._app.ports.fromJs.send({ topic: "punchlist", payload: punchList })
                } else {
                    this._queue.push({ topic: "punchlist", payload: punchList })
                }
            }
        }
        customElements.define(name, ElmElement)
    },
}


module.exports = webComponent