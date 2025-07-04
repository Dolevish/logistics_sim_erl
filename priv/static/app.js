// Enhanced Logistics Simulator Dashboard with Map Support

// WebSocket connection
let ws = null;
let reconnectInterval = null;
let isConnected = false;
let reconnectAttempts = 0;
const MAX_RECONNECT_ATTEMPTS = 10;

// הגדרת האזורים הקבועים
const FIXED_ZONES = ['north', 'center', 'south'];

// Application state
const appState = {
    simulationState: 'idle', // idle, running, paused
    configuration: {},
    couriers: {},
    packages: {},
    zones: {},
    isPaused: false,
    mapEnabled: false,
    mapData: {
        locations: [],
        roads: [],
        courierPositions: {}
    }
};

// Map visualization instance
let mapVisualization = null;

// DOM Elements
const elements = {
    connectionStatus: document.getElementById('connectionStatus'),
    connectionText: document.getElementById('connectionText'),
    startSimBtn: document.getElementById('startSimBtn'),
    resetBtn: document.getElementById('resetBtn'),
    pauseSimBtn: document.getElementById('pauseSimBtn'),
    continueSimBtn: document.getElementById('continueSimBtn'),
    pauseOrderGenBtn: document.getElementById('pauseOrderGenBtn'),
    continueOrderGenBtn: document.getElementById('continueOrderGenBtn'),
    orderIntervalControl: document.getElementById('orderIntervalControl'),
    newOrderInterval: document.getElementById('newOrderInterval'),
    updateOrderIntervalBtn: document.getElementById('updateOrderIntervalBtn'),
    configControls: document.getElementById('configControls'),
    runtimeControls: document.getElementById('runtimeControls'),
    configPanel: document.getElementById('configPanel'),
    runtimePanels: document.getElementById('runtimePanels'),
    mapPanel: document.getElementById('mapPanel'),
    configForm: document.getElementById('configForm'),
    numCouriersInput: document.getElementById('numCouriers'),
    numHomesInput: document.getElementById('numHomes'),
    orderIntervalInput: document.getElementById('orderInterval'),
    minTravelTimeInput: document.getElementById('minTravelTime'),
    maxTravelTimeInput: document.getElementById('maxTravelTime'),
    enableMapView: document.getElementById('enableMapView'),
    numHomesContainer: document.getElementById('numHomesContainer'),
    ordersList: document.getElementById('ordersList'),
    couriersList: document.getElementById('couriersList'),
    totalOrders: document.getElementById('totalOrders'),
    pendingOrders: document.getElementById('pendingOrders'),
    inTransitOrders: document.getElementById('inTransitOrders'),
    deliveredOrders: document.getElementById('deliveredOrders'),
    zonesStatus: document.getElementById('zonesStatus'),
    configFooter: document.getElementById('configFooter'),
    zonesContainer: document.getElementById('zonesContainer'),
    mapCanvas: document.getElementById('mapCanvas')
};

// Initialize the application
function init() {
    console.log('Initializing Logistics Dashboard v2.3...');
    connectWebSocket();
    setupEventListeners();
    validateFormInputs();
    updateUIForSimulationState('idle');
}

// Setup all event listeners
function setupEventListeners() {
    elements.startSimBtn.addEventListener('click', startSimulation);
    elements.resetBtn.addEventListener('click', resetSimulation);
    elements.pauseSimBtn.addEventListener('click', pauseSimulation);
    elements.continueSimBtn.addEventListener('click', continueSimulation);
    elements.pauseOrderGenBtn.addEventListener('click', pauseOrderGenerator);
    elements.continueOrderGenBtn.addEventListener('click', continueOrderGenerator);
    elements.updateOrderIntervalBtn.addEventListener('click', updateOrderInterval);
    elements.configForm.addEventListener('input', validateFormInputs);

    elements.enableMapView.addEventListener('change', () => {
        elements.numHomesContainer.style.display = elements.enableMapView.checked ? 'flex' : 'none';
        validateFormInputs();
    });

    elements.minTravelTimeInput.addEventListener('change', () => {
        const min = parseInt(elements.minTravelTimeInput.value);
        const max = parseInt(elements.maxTravelTimeInput.value);
        if (min >= max) {
            elements.maxTravelTimeInput.value = min + 10;
        }
    });

    elements.maxTravelTimeInput.addEventListener('change', () => {
        const min = parseInt(elements.minTravelTimeInput.value);
        const max = parseInt(elements.maxTravelTimeInput.value);
        if (max <= min) {
            elements.minTravelTimeInput.value = max - 10;
        }
    });
}

// Validate form inputs and enable/disable start button
function validateFormInputs() {
    const numCouriers = parseInt(elements.numCouriersInput.value);
    const orderInterval = parseInt(elements.orderIntervalInput.value);
    const minTravel = parseInt(elements.minTravelTimeInput.value);
    const maxTravel = parseInt(elements.maxTravelTimeInput.value);
    const mapEnabled = elements.enableMapView.checked;

    let homesValid = true;
    if (mapEnabled) {
        const numHomes = parseInt(elements.numHomesInput.value);
        homesValid = numHomes >= 100 && numHomes <= 2000;
    }

    const isValid = homesValid &&
                   numCouriers > 0 && numCouriers <= 200 &&
                   orderInterval > 0 && orderInterval <= 300 &&
                   minTravel > 0 && maxTravel > minTravel;

    elements.startSimBtn.disabled = !isValid;
}

// Start simulation with configuration
function startSimulation() {
    const mapEnabled = elements.enableMapView.checked;
    const numHomes = mapEnabled ? parseInt(elements.numHomesInput.value) : 0;
    
    const config = {
        zones: FIXED_ZONES,
        num_couriers: parseInt(elements.numCouriersInput.value),
        num_homes: numHomes,
        order_interval: parseInt(elements.orderIntervalInput.value) * 1000,
        min_travel_time: parseInt(elements.minTravelTimeInput.value) * 1000,
        max_travel_time: parseInt(elements.maxTravelTimeInput.value) * 1000,
        enable_map: mapEnabled
    };

    appState.configuration = config;
    appState.mapEnabled = mapEnabled;
    generateZonePanels();
    if (mapEnabled && !mapVisualization) {
        mapVisualization = new MapVisualization(elements.mapCanvas);
    }
    sendCommand('start_simulation', config);
    elements.startSimBtn.disabled = true;
    elements.startSimBtn.textContent = 'Starting...';
}

// Map Visualization Class
class MapVisualization {
    constructor(canvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        this.locations = {};
        this.roads = [];
        this.courierPositions = {};
        this.scale = 1;
        this.offsetX = 0;
        this.offsetY = 0;
        this.colors = {
            background: '#f9f9f9', road: '#e0e0e0', home: '#3498db', business: '#2ecc71',
            courier: { idle: '#e74c3c', picking_up: '#f39c12', delivering: '#9b59b6' },
            zones: { north: 'rgba(52, 152, 219, 0.05)', center: 'rgba(46, 204, 113, 0.05)', south: 'rgba(231, 76, 60, 0.05)'}
        };
        this.setupCanvas();
        this.startAnimation();
    }
    setupCanvas() { this.canvas.width = 1200; this.canvas.height = 1000; this.draw(); }
    updateMapData(locations, roads) {
        this.locations = {};
        locations.forEach(loc => { this.locations[loc.id] = loc; });
        this.roads = roads;
        this.calculateTransform();
    }
    calculateTransform() {
        if (Object.keys(this.locations).length === 0) return;

        const locs = Object.values(this.locations);
        const minX = Math.min(...locs.map(l => l.x));
        const maxX = Math.max(...locs.map(l => l.x));
        const minY = Math.min(...locs.map(l => l.y));
        const maxY = Math.max(...locs.map(l => l.y));

        const mapWidth = maxX - minX;
        const mapHeight = maxY - minY;

        const padding = 200; // מרווח פנימי
        const scaleX = (this.canvas.width - padding * 2) / mapWidth;
        const scaleY = (this.canvas.height - padding * 2) / mapHeight;
        this.scale = Math.min(scaleX, scaleY);

        const newMapWidth = mapWidth * this.scale;
        const newMapHeight = mapHeight * this.scale;

        this.offsetX = (this.canvas.width - newMapWidth) / 2 - (minX * this.scale);
        this.offsetY = (this.canvas.height - newMapHeight) / 2 - (minY * this.scale);
    }
    updateCourierPosition(courierId, positionData) { this.courierPositions[courierId] = positionData; }
    draw() {
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
        this.ctx.fillStyle = this.colors.background;
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        this.drawZones(); this.drawRoads(); this.drawLocations(); this.drawCouriers(); this.drawLabels();
    }
    drawZones() {
        this.ctx.save();
        this.ctx.fillStyle = this.colors.zones.north; this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height / 3);
        this.ctx.fillStyle = this.colors.zones.center; this.ctx.fillRect(0, this.canvas.height / 3, this.canvas.width, this.canvas.height / 3);
        this.ctx.fillStyle = this.colors.zones.south; this.ctx.fillRect(0, 2 * this.canvas.height / 3, this.canvas.width, this.canvas.height / 3);
        this.ctx.restore();
    }
    drawRoads() {
        this.ctx.save();
        this.ctx.strokeStyle = this.colors.road; this.ctx.lineWidth = 1;
        this.roads.forEach(road => {
            const from = this.locations[road.from]; const to = this.locations[road.to];
            if (from && to) {
                this.ctx.beginPath();
                this.ctx.moveTo(from.x * this.scale + this.offsetX, from.y * this.scale + this.offsetY);
                this.ctx.lineTo(to.x * this.scale + this.offsetX, to.y * this.scale + this.offsetY);
                this.ctx.stroke();
            }
        });
        this.ctx.restore();
    }
    drawLocations() {
        Object.values(this.locations).forEach(loc => {
            const x = loc.x * this.scale + this.offsetX; const y = loc.y * this.scale + this.offsetY;
            this.ctx.save();
            if (loc.type === 'business') {
                this.ctx.fillStyle = this.colors.business; this.ctx.fillRect(x - 8, y - 8, 16, 16);
                this.ctx.strokeStyle = '#27ae60'; this.ctx.lineWidth = 2; this.ctx.strokeRect(x - 8, y - 8, 16, 16);
            } else {
                const homeNumber = loc.id.split('_')[1] || '';
                const radius = 9;
                this.ctx.fillStyle = this.colors.home;
                this.ctx.beginPath();
                this.ctx.arc(x, y, radius, 0, 2 * Math.PI);
                this.ctx.fill();
                this.ctx.font = 'bold 10px Arial';
                this.ctx.textAlign = 'center';
                this.ctx.textBaseline = 'middle';
                this.ctx.strokeStyle = 'rgba(0,0,0,0.6)';
                this.ctx.lineWidth = 2;
                this.ctx.strokeText(homeNumber, x, y);
                this.ctx.fillStyle = 'white';
                this.ctx.fillText(homeNumber, x, y);
            }
            this.ctx.restore();
        });
    }
    drawCouriers() {
        Object.entries(this.courierPositions).forEach(([courierId, data]) => {
            if (data.position) {
                const x = data.position.x * this.scale + this.offsetX;
                const y = data.position.y * this.scale + this.offsetY;

                this.ctx.save();

                // Determine color based on status
                let color = this.colors.courier.idle;
                if (data.status === 'moving') {
                    if (data.destination && this.locations[data.destination]) {
                        const destLoc = this.locations[data.destination];
                        if (destLoc.type === 'business') {
                            color = this.colors.courier.picking_up;
                        } else {
                            color = this.colors.courier.delivering;
                        }
                    }
                }

                // Draw courier as triangle pointing in direction of movement
                this.ctx.fillStyle = color;
                this.ctx.beginPath();
                this.ctx.arc(x, y, 8, 0, 2 * Math.PI);
                this.ctx.fill();
                
                // Draw courier ID
                this.ctx.fillStyle = 'white';
                this.ctx.font = 'bold 10px Arial';
                this.ctx.textAlign = 'center';
                this.ctx.textBaseline = 'middle';
                this.ctx.fillText(courierId.replace('courier', ''), x, y);

                // Draw progress bar if moving
                if (data.progress && data.progress < 1) {
                    this.ctx.strokeStyle = color;
                    this.ctx.lineWidth = 2;
                    this.ctx.beginPath();
                    this.ctx.arc(x, y, 12, -Math.PI/2, -Math.PI/2 + (2 * Math.PI * data.progress));
                    this.ctx.stroke();
                }

                this.ctx.restore();
            }
        });
    }

    drawLabels() {
        this.ctx.save();
        this.ctx.font = 'bold 18px Arial';
        this.ctx.fillStyle = 'rgba(0, 0, 0, 0.4)';
        this.ctx.textAlign = 'center';
        // --- הערה חדשה: התאמת מיקום התוויות לחלוקה החדשה ---
        this.ctx.fillText('NORTH ZONE', this.canvas.width / 2, this.canvas.height / 6);
        this.ctx.fillText('CENTER ZONE', this.canvas.width / 2, this.canvas.height / 2);
        this.ctx.fillText('SOUTH ZONE', this.canvas.width / 2, this.canvas.height * 5 / 6);
        this.ctx.restore();
    }
    startAnimation() { const animate = () => { this.draw(); requestAnimationFrame(animate); }; animate(); }
}


// פונקציות חדשות לטיפול בכפתורים
function pauseSimulation() {
    sendCommand('pause_simulation');
    appState.isPaused = true;
    updateUIForSimulationState('paused');
    elements.orderIntervalControl.style.display = 'flex';
    elements.pauseOrderGenBtn.disabled = true;
    elements.continueOrderGenBtn.disabled = true;
}

function continueSimulation() {
    sendCommand('continue_simulation');
    appState.isPaused = false;
    updateUIForSimulationState('running');
    elements.orderIntervalControl.style.display = 'none';
    elements.pauseOrderGenBtn.disabled = false;
    elements.continueOrderGenBtn.disabled = false;
}

function pauseOrderGenerator() {
    sendCommand('pause_order_generator');
    updateUIForOrderGeneratorState(true);
    elements.orderIntervalControl.style.display = 'flex';
}

function continueOrderGenerator() {
    sendCommand('continue_order_generator');
    updateUIForOrderGeneratorState(false);
    elements.orderIntervalControl.style.display = 'none';
}

function updateOrderInterval() {
    const newInterval = parseInt(elements.newOrderInterval.value);
    if (newInterval > 0) {
        sendCommand('update_order_interval', { interval: newInterval * 1000 });
    }
}

// Reset simulation
function resetSimulation() {
    if (confirm('Are you sure you want to reset the simulation? All current data will be lost.')) {
        sendCommand('stop_simulation');
        appState.couriers = {};
        appState.packages = {};
        appState.zones = {};
        appState.mapData = { locations: [], roads: [], courierPositions: {} };
        elements.ordersList.innerHTML = '';
        elements.couriersList.innerHTML = '';
        elements.zonesContainer.innerHTML = '';
        updateStats();
        if (mapVisualization) {
            mapVisualization = null;
        }
    }
}

// Update UI based on simulation state
function updateUIForSimulationState(state) {
    appState.simulationState = state;
    switch(state) {
        case 'idle':
            elements.configPanel.style.display = 'block';
            elements.runtimePanels.style.display = 'none';
            elements.mapPanel.style.display = 'none';
            elements.configControls.style.display = 'flex';
            elements.runtimeControls.style.display = 'none';
            elements.zonesStatus.style.display = 'none';
            elements.configFooter.style.display = 'block';
            elements.orderIntervalControl.style.display = 'none';
            elements.startSimBtn.disabled = false;
            elements.startSimBtn.textContent = 'Start Simulation';
            elements.enableMapView.checked = true;
            elements.numHomesContainer.style.display = 'flex';
            break;
        case 'running':
            elements.configPanel.style.display = 'none';
            elements.runtimePanels.style.display = 'flex';
            elements.configControls.style.display = 'none';
            elements.runtimeControls.style.display = 'flex';
            elements.zonesStatus.style.display = 'block';
            elements.configFooter.style.display = 'none';
            if (appState.mapEnabled) {
                elements.mapPanel.style.display = 'block';
            }
            elements.pauseSimBtn.style.display = 'inline-block';
            elements.continueSimBtn.style.display = 'none';
            elements.pauseOrderGenBtn.style.display = 'inline-block';
            elements.continueOrderGenBtn.style.display = 'none';
            elements.pauseOrderGenBtn.disabled = false;
            generateZonePanels();
            break;
        case 'paused':
            elements.pauseSimBtn.style.display = 'none';
            elements.continueSimBtn.style.display = 'inline-block';
            elements.pauseOrderGenBtn.disabled = true;
            break;
    }
}

function updateUIForOrderGeneratorState(isPaused) {
    if (isPaused) {
        elements.pauseOrderGenBtn.style.display = 'none';
        elements.continueOrderGenBtn.style.display = 'inline-block';
    } else {
        elements.pauseOrderGenBtn.style.display = 'inline-block';
        elements.continueOrderGenBtn.style.display = 'none';
    }
}

function generateZonePanels() {
    elements.zonesContainer.innerHTML = '';
    FIXED_ZONES.forEach(zone => {
        const zoneDiv = document.createElement('div');
        zoneDiv.className = 'zone-item';
        zoneDiv.setAttribute('data-zone', zone);
        zoneDiv.innerHTML = `
            <h4>${zone.charAt(0).toUpperCase() + zone.slice(1)} Zone</h4>
            <div class="zone-stats">
                <span>Total: <span class="zone-total">0</span></span>
                <span>Waiting: <span class="zone-waiting">0</span></span>
                <span>Active: <span class="zone-active">0</span></span>
                <span>Delivered: <span class="zone-delivered">0</span></span>
            </div>
        `;
        elements.zonesContainer.appendChild(zoneDiv);
    });
}

function connectWebSocket() {
    if (ws && ws.readyState === WebSocket.OPEN) return;
    const wsUrl = `ws://${window.location.host}/websocket`;
    try {
        ws = new WebSocket(wsUrl);
        const connectionTimeout = setTimeout(() => ws.close(), 10000);
        ws.onopen = () => {
            clearTimeout(connectionTimeout);
            isConnected = true;
            reconnectAttempts = 0;
            updateConnectionStatus(true);
            clearInterval(reconnectInterval);
            setTimeout(() => ws.send(JSON.stringify({ type: 'request_simulation_state' })), 100);
        };
        ws.onmessage = (event) => { try { handleWebSocketMessage(JSON.parse(event.data)); } catch (e) { console.error('WS msg parse error:', e); } };
        ws.onerror = (error) => { isConnected = false; updateConnectionStatus(false); console.error('WS error:', error); };
        ws.onclose = (event) => {
            clearTimeout(connectionTimeout);
            isConnected = false;
            updateConnectionStatus(false);
            attemptReconnect();
        };
    } catch (error) {
        isConnected = false;
        updateConnectionStatus(false);
        console.error('WS creation failed:', error);
    }
}

function attemptReconnect() {
    if (reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
        const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
        clearInterval(reconnectInterval);
        reconnectInterval = setTimeout(() => {
            reconnectAttempts++;
            connectWebSocket();
        }, delay);
    }
}

function handleWebSocketMessage(data) {
    switch (data.type) {
        case 'simulation_state': handleSimulationStateUpdate(data.state, data.config); break;
        case 'state_update': handleStateUpdate(data.update_type, data.data); break;
        case 'command_response': handleCommandResponse(data.command, data.success, data.message); break;
        case 'map_initialized': handleMapInitialized(data.data); break;
        case 'courier_position_update': handleCourierPositionUpdate(data.data); break;
        case 'heartbeat': if (ws && ws.readyState === WebSocket.OPEN) ws.send(JSON.stringify({type: 'pong'})); break;
    }
}

function handleMapInitialized(data) {
    if (mapVisualization && data.locations && data.roads) {
        mapVisualization.updateMapData(data.locations, data.roads);
    }
    appState.mapData.locations = data.locations || [];
    appState.mapData.roads = data.roads || [];
}

function handleCourierPositionUpdate(data) {
    if (mapVisualization && data.courier_id) {
        mapVisualization.updateCourierPosition(data.courier_id, data);
    }
}

function handleSimulationStateUpdate(state, config) {
    if (config) {
        config.zones = FIXED_ZONES;
        appState.configuration = config;
        appState.mapEnabled = config.enable_map || false;
    }
    updateUIForSimulationState(state);
    if (state === 'running') {
        setTimeout(() => sendCommand('request_full_state'), 500);
    }
}

function handleCommandResponse(command, success, message) {
    if (command === 'start_simulation' && !success) {
        updateUIForSimulationState('idle');
        alert(`Failed to start simulation: ${message}`);
    }
}

function handleStateUpdate(updateType, data) {
    try {
        switch (updateType) {
            case 'courier_update':
                if (data && data.id) {
                    const existing = appState.couriers[data.id] || {};
                    appState.couriers[data.id] = { ...existing, ...data };
                    updateCourierDisplay(appState.couriers[data.id]);
                }
                break;
            case 'package_update':
                if (data && data.id) {
                    appState.packages[data.id] = data;
                    updatePackageDisplay(data);
                    updateStats();
                }
                break;
            case 'zone_update':
                if (data && data.zone) {
                    if (elements.zonesContainer.children.length === 0) generateZonePanels();
                    appState.zones[data.zone] = data;
                    updateZoneDisplay(data);
                }
                break;
            case 'full_state':
                appState.couriers = {};
                appState.packages = {};
                appState.zones = {};
                if (data.couriers) data.couriers.forEach(c => appState.couriers[c.id] = c);
                if (data.packages) data.packages.forEach(p => appState.packages[p.id] = p);
                if (data.zones) data.zones.forEach(z => appState.zones[z.zone] = z);
                renderCouriers();
                renderPackages();
                if (elements.zonesContainer.children.length === 0) generateZonePanels();
                Object.values(appState.zones).forEach(updateZoneDisplay);
                updateStats();
                break;
        }
    } catch (e) { console.error('State update error:', e); }
}

function sendCommand(action, data = {}) {
    if (ws && ws.readyState === WebSocket.OPEN) {
        try {
            const message = action === 'request_full_state' 
                ? { type: 'request_full_state' } 
                : { type: 'command', action, ...data };
            ws.send(JSON.stringify(message));
        } catch (e) { console.error('Send command error:', e); }
    } else {
        connectWebSocket();
    }
}

function updateConnectionStatus(connected) {
    elements.connectionStatus.classList.toggle('connected', connected);
    elements.connectionStatus.classList.toggle('reconnecting', !connected && reconnectAttempts > 0 && reconnectAttempts < MAX_RECONNECT_ATTEMPTS);
    elements.connectionText.textContent = connected 
        ? 'Connected' 
        : (reconnectAttempts > 0 && reconnectAttempts < MAX_RECONNECT_ATTEMPTS ? `Reconnecting... (${reconnectAttempts}/${MAX_RECONNECT_ATTEMPTS})` : 'Disconnected');
}

function renderCouriers() {
    elements.couriersList.innerHTML = '';
    Object.values(appState.couriers).forEach(c => elements.couriersList.appendChild(createCourierElement(c)));
}

// --- הערה חדשה: התיקון המרכזי להצגת ה-ETA ---
function createCourierElement(courier) {
    const div = document.createElement('div');
    div.className = `courier-item ${courier.status || 'unknown'}`;
    div.id = `courier-${courier.id}`;

    const statusText = (courier.status || 'unknown').replace('_', ' ').toUpperCase();

    let detailsHTML = '<div class="courier-details">';
    if (courier.current_package) {
        detailsHTML += `<strong>Package:</strong> ${courier.current_package}<br>`;
        
        // בדיקה אם יש יעד
        if (courier.destination) {
            detailsHTML += `<strong>Destination:</strong> ${courier.destination}<br>`;
        }

        // בדיקה אם יש ETA והוא גדול מאפס
        if (courier.eta && courier.eta > 0) {
            detailsHTML += `<strong>ETA:</strong> ${formatETA(courier.eta)}`;
        }
    }
    detailsHTML += '</div>';

    const totalDelivered = courier.total_delivered || 0;

    div.innerHTML = `
        <div class="courier-header">
            <span class="courier-name">${(courier.id || 'unknown').toUpperCase()}</span>
            <span class="courier-status ${courier.status || 'unknown'}">${statusText}</span>
        </div>
        ${detailsHTML}
        <div class="courier-stats">
            <span><strong>Total Delivered:</strong> ${totalDelivered}</span>
        </div>
    `;

    return div;
}

// --- הערה חדשה: פונקציית עזר לפרמוט ה-ETA ---
function formatETA(eta) {
    try {
        if (!eta || eta <= 0) return 'N/A';
        const totalSeconds = Math.floor(eta / 1000);
        const minutes = Math.floor(totalSeconds / 60);
        const seconds = totalSeconds % 60;
        return `${minutes}m ${seconds.toString().padStart(2, '0')}s`;
    } catch (error) {
        console.error('Error formatting ETA:', eta, error);
        return 'N/A';
    }
}


function updateCourierDisplay(courier) {
    const existing = document.getElementById(`courier-${courier.id}`);
    const newElement = createCourierElement(courier);
    if (existing) {
        existing.replaceWith(newElement);
    } else {
        elements.couriersList.appendChild(newElement);
    }
}

function renderPackages() {
    elements.ordersList.innerHTML = '';
    Object.values(appState.packages)
        .sort((a, b) => (b.created_at || 0) - (a.created_at || 0))
        .forEach(p => elements.ordersList.appendChild(createPackageElement(p)));
}

function createPackageElement(pkg) {
    const div = document.createElement('div');
    div.className = `order-item ${pkg.status || 'unknown'}`;
    div.id = `package-${pkg.id}`;
    const statusText = (pkg.status || 'unknown').replace('_', ' ').toUpperCase();
    let detailsHTML = '<div class="order-details">';
    if (pkg.zone) detailsHTML += `Zone: ${pkg.zone}<br>`;
    if (pkg.courier) detailsHTML += `Courier: ${pkg.courier}`;
    detailsHTML += '</div>';
    div.innerHTML = `
        <div class="order-header">
            <span class="order-id">Order #${pkg.id || 'unknown'}</span>
            <span class="order-status ${pkg.status || 'unknown'}">${statusText}</span>
        </div>
        ${detailsHTML}`;
    return div;
}

function updatePackageDisplay(pkg) {
    const existing = document.getElementById(`package-${pkg.id}`);
    if (existing) {
        existing.replaceWith(createPackageElement(pkg));
    } else {
        renderPackages();
    }
}

function updateZoneDisplay(zone) {
    const zoneEl = document.querySelector(`[data-zone="${zone.zone}"]`);
    if (zoneEl) {
        zoneEl.querySelector('.zone-waiting').textContent = zone.waiting_packages || 0;
        zoneEl.querySelector('.zone-active').textContent = zone.active_deliveries || 0;
        zoneEl.querySelector('.zone-delivered').textContent = zone.total_delivered || 0;
        zoneEl.querySelector('.zone-total').textContent = zone.total_orders || 0;
    }
}

function updateStats() {
    const packages = Object.values(appState.packages);
    const stats = {
        total: packages.length,
        pending: packages.filter(p => p.status === 'ordered').length,
        inTransit: packages.filter(p => ['assigned', 'picking_up', 'in_transit', 'delivering'].includes(p.status)).length,
        delivered: packages.filter(p => p.status === 'delivered').length
    };
    elements.totalOrders.textContent = stats.total;
    elements.pendingOrders.textContent = stats.pending;
    elements.inTransitOrders.textContent = stats.inTransit;
    elements.deliveredOrders.textContent = stats.delivered;
}

document.addEventListener('DOMContentLoaded', init);
