// ClaudeLang Visualizer Client

class Visualizer {
    constructor() {
        this.ws = null;
        this.graph = null;
        this.currentAST = null;
        this.currentVMState = null;
        this.debugEvents = [];
        this.maxDebugEvents = 100;
        
        this.initializeUI();
        this.connectWebSocket();
    }
    
    initializeUI() {
        // Control buttons
        document.getElementById('btn-start').addEventListener('click', () => {
            this.sendCommand({ command: 'Start' });
        });
        
        document.getElementById('btn-pause').addEventListener('click', () => {
            this.sendCommand({ command: 'Pause' });
        });
        
        document.getElementById('btn-step').addEventListener('click', () => {
            this.sendCommand({ command: 'Step' });
        });
        
        document.getElementById('btn-reset').addEventListener('click', () => {
            this.sendCommand({ command: 'Reset' });
        });
        
        document.getElementById('btn-load').addEventListener('click', () => {
            const source = document.getElementById('code-editor').value;
            this.sendCommand({ 
                command: 'LoadProgram',
                source: source
            });
        });
        
        // Initialize graph
        this.initializeGraph();
    }
    
    connectWebSocket() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${window.location.host}/ws`;
        
        this.ws = new WebSocket(wsUrl);
        
        this.ws.onopen = () => {
            this.setStatus('Connected', 'connected');
        };
        
        this.ws.onclose = () => {
            this.setStatus('Disconnected', 'error');
            // Reconnect after 2 seconds
            setTimeout(() => this.connectWebSocket(), 2000);
        };
        
        this.ws.onerror = (error) => {
            console.error('WebSocket error:', error);
            this.setStatus('Connection error', 'error');
        };
        
        this.ws.onmessage = (event) => {
            try {
                const message = JSON.parse(event.data);
                this.handleMessage(message);
            } catch (error) {
                console.error('Failed to parse message:', error);
            }
        };
    }
    
    handleMessage(message) {
        switch (message.type) {
            case 'Connected':
                console.log('Connected with session ID:', message.session_id);
                break;
                
            case 'ASTGraph':
                this.updateASTGraph(message.graph);
                break;
                
            case 'VMState':
                this.updateVMState(message.snapshot);
                break;
                
            case 'DebugEvent':
                this.addDebugEvent(message.event);
                break;
                
            case 'Error':
                this.showError(message.message);
                break;
        }
    }
    
    sendCommand(command) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({
                type: 'Control',
                command: command
            }));
        }
    }
    
    initializeGraph() {
        const container = d3.select('#ast-graph');
        const width = container.node().getBoundingClientRect().width;
        const height = container.node().getBoundingClientRect().height;
        
        const svg = container
            .attr('width', width)
            .attr('height', height);
        
        // Add zoom behavior
        const zoom = d3.zoom()
            .scaleExtent([0.1, 4])
            .on('zoom', (event) => {
                svg.select('g').attr('transform', event.transform);
            });
        
        svg.call(zoom);
        
        // Create main group for transformations
        svg.append('g');
        
        this.graph = {
            svg: svg,
            width: width,
            height: height
        };
    }
    
    updateASTGraph(graphData) {
        if (!this.graph) return;
        
        const g = this.graph.svg.select('g');
        
        // Clear existing graph
        g.selectAll('*').remove();
        
        // Create links
        const link = g.append('g')
            .selectAll('line')
            .data(graphData.edges)
            .enter()
            .append('line')
            .attr('class', 'link')
            .attr('x1', d => {
                const source = graphData.nodes.find(n => n.id === d.source);
                return source ? source.position.x + 60 : 0;
            })
            .attr('y1', d => {
                const source = graphData.nodes.find(n => n.id === d.source);
                return source ? source.position.y + 30 : 0;
            })
            .attr('x2', d => {
                const target = graphData.nodes.find(n => n.id === d.target);
                return target ? target.position.x + 60 : 0;
            })
            .attr('y2', d => {
                const target = graphData.nodes.find(n => n.id === d.target);
                return target ? target.position.y + 30 : 0;
            });
        
        // Create nodes
        const node = g.append('g')
            .selectAll('g')
            .data(graphData.nodes)
            .enter()
            .append('g')
            .attr('class', d => `node ${d.node_type}`)
            .attr('transform', d => `translate(${d.position.x}, ${d.position.y})`);
        
        // Add rectangles
        node.append('rect')
            .attr('width', 120)
            .attr('height', 60)
            .attr('rx', 5)
            .attr('ry', 5);
        
        // Add text
        node.append('text')
            .attr('x', 60)
            .attr('y', 35)
            .text(d => d.label);
        
        // Center the graph
        const bounds = g.node().getBBox();
        const centerX = (this.graph.width - bounds.width) / 2 - bounds.x;
        const centerY = (this.graph.height - bounds.height) / 2 - bounds.y;
        
        g.attr('transform', `translate(${centerX}, ${centerY})`);
        
        this.currentAST = graphData;
    }
    
    updateVMState(snapshot) {
        this.currentVMState = snapshot;
        
        // Update stack
        const stackView = document.getElementById('stack-view');
        stackView.innerHTML = snapshot.stack.map((value, i) => 
            `<div class="state-item">[${i}] ${value}</div>`
        ).join('');
        
        // Update locals
        const localsView = document.getElementById('locals-view');
        localsView.innerHTML = Object.entries(snapshot.locals).map(([name, value]) =>
            `<div class="state-item">${name}: ${value}</div>`
        ).join('');
        
        // Update call stack
        const callStackView = document.getElementById('callstack-view');
        callStackView.innerHTML = snapshot.call_stack.map((frame, i) =>
            `<div class="state-item">[${i}] ${frame.function_name || 'anonymous'} @ ${frame.pc}</div>`
        ).join('');
    }
    
    addDebugEvent(event) {
        this.debugEvents.push(event);
        
        // Limit number of events
        if (this.debugEvents.length > this.maxDebugEvents) {
            this.debugEvents.shift();
        }
        
        // Update UI
        const eventsView = document.getElementById('debug-events');
        const eventDiv = document.createElement('div');
        eventDiv.className = 'debug-event';
        
        if (event.type === 'Error') {
            eventDiv.className += ' error';
        }
        
        eventDiv.textContent = `[${new Date(event.timestamp / 1000).toISOString()}] ${event.type}`;
        
        if (event.type === 'InstructionPre') {
            eventDiv.textContent += ` - ${event.instruction.opcode}`;
        } else if (event.type === 'FunctionCall' && event.name) {
            eventDiv.textContent += ` - ${event.name}`;
        } else if (event.type === 'Error') {
            eventDiv.textContent += ` - ${event.message}`;
        }
        
        eventsView.appendChild(eventDiv);
        eventsView.scrollTop = eventsView.scrollHeight;
        
        // Limit displayed events
        while (eventsView.children.length > this.maxDebugEvents) {
            eventsView.removeChild(eventsView.firstChild);
        }
    }
    
    showError(message) {
        console.error('Error:', message);
        this.addDebugEvent({
            type: 'Error',
            timestamp: Date.now() * 1000,
            message: message
        });
    }
    
    setStatus(text, className = '') {
        const status = document.getElementById('status');
        status.textContent = text;
        status.className = className;
    }
}

// Initialize visualizer when page loads
document.addEventListener('DOMContentLoaded', () => {
    window.visualizer = new Visualizer();
});