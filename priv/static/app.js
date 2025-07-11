// Enhanced Logistics Simulator Dashboard with Configurable Map Support

document.addEventListener('DOMContentLoaded', () => {
    // --- הערה: הגדרת משתנים גלובליים לאפליקציה ---
    let ws = null;
    let reconnectInterval = null;
    let isConnected = false;
    let reconnectAttempts = 0;
    const MAX_RECONNECT_ATTEMPTS = 10;
    const FIXED_ZONES = ['north', 'center', 'south'];

    // --- הערה: אוביקט ראשי שמחזיק את כל מצב הסימולציה בצד הלקוח ---
    const appState = {
        simulationState: 'idle',
        configuration: {},
        couriers: {},
        packages: {},
        zones: {},
        isPaused: false,
        mapData: {
            mapElements: {},
            courierPositions: {}
        }
    };
    
    let mapVisualization = null;

    // --- הערה: איסוף כל האלמנטים מה-DOM לגישה נוחה ---
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
        orderIntervalInput: document.getElementById('orderInterval'),
        ordersList: document.getElementById('ordersList'),
        couriersList: document.getElementById('couriersList'),
        totalOrders: document.getElementById('totalOrders'),
        pendingOrders: document.getElementById('pendingOrders'),
        inTransitOrders: document.getElementById('inTransitOrders'),
        deliveredOrders: document.getElementById('deliveredOrders'),
        zonesStatus: document.getElementById('zonesStatus'),
        configFooter: document.getElementById('configFooter'),
        zonesContainer: document.getElementById('zonesContainer'),
        mapCanvas: document.getElementById('mapCanvas'),
        mapSizeSelect: document.getElementById('mapSize')
    };

    // --- הערה: מחלקת הוויזואליזציה של המפה ---
    // הערה חדשה: הקוד המלא של המחלקה הוחזר ותוקן. זהו התיקון המרכזי שפותר את בעיית היעלמות המפה.
    class MapVisualization {
        constructor(canvas) {
            this.canvas = canvas;
            this.ctx = canvas.getContext('2d');
            this.mapElements = {}; // יחזיק את כל רכיבי המפה (בתים, עסקים, כבישים)
            this.courierPositions = {}; // יחזיק את המיקומים העדכניים של השליחים
            this.colors = {
                idle: '#e74c3c',
                picking_up: '#f39c12',
                delivering: '#9b59b6',
                in_transit: '#9b59b6'
            };
            this.setupCanvas();
            this.startAnimation();
        }

        setupCanvas() { this.canvas.width = 1200; this.canvas.height = 800; }
        clearAll() { this.mapElements = {}; this.courierPositions = {}; this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height); }
        
        updateMapData(data) {
            if (data && data.elements) {
                this.mapElements = data.elements;
            }
        }

        updateCourierPosition(courierId, positionData) {
            this.courierPositions[courierId] = positionData;
        }

        draw() {
            this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
            this.drawZones();
            this.drawRoads('roads', '#cccccc', 6);
            this.drawRoads('driveways', '#e0e0e0', 2);
            this.drawLocations('homes', '#3498db', 8, true);
            this.drawLocations('businesses', '#27ae60', 10, false);
            this.drawCouriers();
            this.drawLabels();
        }

        drawZones() {
            if (!this.mapElements.zones) return;
            this.ctx.save();
            const colors = { north: 'rgba(236, 112, 99, 0.1)', center: 'rgba(247, 220, 111, 0.1)', south: 'rgba(133, 193, 233, 0.1)' };
            this.mapElements.zones.forEach(zone => {
                this.ctx.fillStyle = colors[zone.zone] || 'rgba(0,0,0,0.05)';
                this.ctx.fillRect(zone.x, zone.y, zone.width, zone.height);
            });
            this.ctx.restore();
        }

        drawRoads(type, color, lineWidth) {
            if (!this.mapElements[type]) return;
            this.ctx.save();
            this.ctx.strokeStyle = color;
            this.ctx.lineWidth = lineWidth;
            this.mapElements[type].forEach(road => {
                this.ctx.beginPath();
                this.ctx.moveTo(road.x1, road.y1);
                this.ctx.lineTo(road.x2, road.y2);
                this.ctx.stroke();
            });
            this.ctx.restore();
        }

        drawLocations(type, color, size, isCircle) {
            if (!this.mapElements[type]) return;
            this.mapElements[type].forEach((loc, index) => {
                const x = loc.x;
                const y = loc.y;
                this.ctx.save();
                this.ctx.fillStyle = color;
                if (isCircle) {
                    this.ctx.beginPath();
                    this.ctx.arc(x, y, size, 0, 2 * Math.PI);
                    this.ctx.fill();
                } else {
                    this.ctx.fillRect(x - size, y - size, size * 2, size * 2);
                }
                this.ctx.font = `bold ${size}px Arial`;
                this.ctx.fillStyle = 'white';
                this.ctx.textAlign = 'center';
                this.ctx.textBaseline = 'middle';
                const label = type === 'homes' ? index + 1 : 'B';
                this.ctx.fillText(label, x, y);
                this.ctx.restore();
            });
        }

        drawCouriers() {
             Object.entries(this.courierPositions).forEach(([courierId, positionData]) => {
                if (positionData.position && typeof positionData.position.x === 'number' && typeof positionData.position.y === 'number') {
                    const x = positionData.position.x;
                    const y = positionData.position.y;
                    const courierState = appState.couriers[courierId];
                    const status = courierState ? courierState.status : 'idle';
                    const color = this.colors[status] || this.colors.idle;

                    this.ctx.save();
                    this.ctx.fillStyle = color;
                    this.ctx.beginPath();
                    this.ctx.arc(x, y, 8, 0, 2 * Math.PI);
                    this.ctx.fill();

                    this.ctx.fillStyle = 'white';
                    this.ctx.font = 'bold 10px Arial';
                    this.ctx.textAlign = 'center';
                    this.ctx.textBaseline = 'middle';
                    this.ctx.fillText(courierId.replace('courier', ''), x, y);

                    if (positionData.progress && positionData.progress < 1) {
                        this.ctx.strokeStyle = color;
                        this.ctx.lineWidth = 3;
                        this.ctx.beginPath();
                        this.ctx.arc(x, y, 12, -Math.PI / 2, -Math.PI / 2 + (2 * Math.PI * positionData.progress));
                        this.ctx.stroke();
                    }
                    this.ctx.restore();
                }
            });
        }

        drawLabels() {
            this.ctx.save();
            this.ctx.font = 'bold 20px Arial';
            this.ctx.fillStyle = 'rgba(0, 0, 0, 0.2)';
            this.ctx.textAlign = 'center';
            this.ctx.fillText('NORTH ZONE', this.canvas.width / 2, 30);
            this.ctx.fillText('CENTER ZONE', this.canvas.width / 2, this.canvas.height / 2);
            this.ctx.fillText('SOUTH ZONE', this.canvas.width / 2, this.canvas.height - 10);
            this.ctx.restore();
        }

        startAnimation() {
            const animate = () => {
                if (appState.simulationState !== 'idle') {
                    this.draw();
                }
                requestAnimationFrame(animate);
            };
            animate();
        }
    }
    
    // --- הערה: כל שאר פונקציות האפליקציה ---
    // (קוד זה זהה לגרסה הקודמת, אך כעת הוא יכול להסתמך על כך שכל הפונקציות שהוא קורא להן כבר הוגדרו)

    const connectWebSocket = () => {
        if (ws && ws.readyState === WebSocket.OPEN) return;
        const wsUrl = `ws://${window.location.host}/websocket`;
        try {
            ws = new WebSocket(wsUrl);
            const connectionTimeout = setTimeout(() => ws.close(), 10000);
            ws.onopen = () => {
                clearTimeout(connectionTimeout);
                isConnected = true; reconnectAttempts = 0;
                updateConnectionStatus(true);
                clearInterval(reconnectInterval);
                setTimeout(() => ws.send(JSON.stringify({ type: 'request_simulation_state' })), 100);
            };
            ws.onmessage = (event) => { try { handleWebSocketMessage(JSON.parse(event.data)); } catch (e) { console.error('WS msg parse error:', e); } };
            ws.onerror = (error) => { isConnected = false; updateConnectionStatus(false); console.error('WS error:', error); };
            ws.onclose = () => { clearTimeout(connectionTimeout); isConnected = false; updateConnectionStatus(false); attemptReconnect(); };
        } catch (error) { isConnected = false; updateConnectionStatus(false); console.error('WS creation failed:', error); }
    };
    const attemptReconnect = () => { if (reconnectAttempts < MAX_RECONNECT_ATTEMPTS) { const delay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000); clearInterval(reconnectInterval); reconnectInterval = setTimeout(() => { reconnectAttempts++; connectWebSocket(); }, delay); } };
    const sendCommand = (action, data = {}) => { if (ws && ws.readyState === WebSocket.OPEN) { try { const message = { type: 'command', action, ...data }; ws.send(JSON.stringify(message)); } catch (e) { console.error('Send command error:', e); } } else { console.warn("WebSocket not connected. Attempting to reconnect."); connectWebSocket(); } };
    const handleWebSocketMessage = (data) => {
        switch (data.type) {
            case 'simulation_state': handleSimulationStateUpdate(data.state, data.config); break;
            case 'state_update': handleStateUpdate(data.update_type, data.data); break;
            case 'command_response': handleCommandResponse(data.command, data.success, data.message); break;
            case 'map_initialized': if (mapVisualization && data.data) { mapVisualization.updateMapData(data.data); } break;
            case 'courier_position_update': if (mapVisualization && data.data && data.data.courier_id) { mapVisualization.updateCourierPosition(data.data.courier_id, data.data); } break;
            case 'heartbeat': if (ws && ws.readyState === WebSocket.OPEN) ws.send(JSON.stringify({ type: 'pong' })); break;
        }
    };
    const handleSimulationStateUpdate = (state, config) => { if (config) { appState.configuration = config; } appState.isPaused = (state === 'paused'); updateUIForSimulationState(state); if (state === 'running' || state === 'paused') { setTimeout(() => sendCommand('request_full_state'), 500); } };
    const handleCommandResponse = (command, success, message) => { if (command === 'start_simulation' && !success) { updateUIForSimulationState('idle'); alert(`Failed to start simulation: ${message}`); } };
    const handleStateUpdate = (updateType, data) => {
        try {
            switch (updateType) {
                case 'courier_update': if (data && data.id) { appState.couriers[data.id] = { ...(appState.couriers[data.id] || {}), ...data }; updateCourierDisplay(appState.couriers[data.id]); } break;
                case 'package_update': if (data && data.id) { appState.packages[data.id] = data; updatePackageDisplay(data); updateStats(); } break;
                case 'zone_update': if (data && data.zone) { if (elements.zonesContainer.children.length === 0) generateZonePanels(); appState.zones[data.zone] = data; updateZoneDisplay(data); } break;
                case 'full_state':
                    appState.couriers = {}; appState.packages = {}; appState.zones = {};
                    if (data.couriers) data.couriers.forEach(c => appState.couriers[c.id] = c);
                    if (data.packages) data.packages.forEach(p => appState.packages[p.id] = p);
                    if (data.zones) data.zones.forEach(z => appState.zones[z.zone] = z);
                    renderAll();
                    break;
            }
        } catch (e) { console.error('State update error:', e, "data:", data); }
    };
    const startSimulation = () => {
        const config = {
            num_couriers: parseInt(elements.numCouriersInput.value),
            order_interval: parseInt(elements.orderIntervalInput.value) * 1000,
            map_size: parseInt(elements.mapSizeSelect.value)
        };
        if (!mapVisualization) {
            mapVisualization = new MapVisualization(elements.mapCanvas);
        }
        sendCommand('start_simulation', config);
        elements.startSimBtn.disabled = true;
        elements.startSimBtn.textContent = 'Starting...';
    };
    const resetSimulation = () => { if (confirm('Are you sure you want to reset the simulation?')) { sendCommand('stop_simulation'); } };
    const pauseSimulation = () => sendCommand('pause_simulation');
    const continueSimulation = () => sendCommand('continue_simulation');
    const pauseOrderGenerator = () => { sendCommand('pause_order_generator'); updateUIForOrderGeneratorState(true); };
    const continueOrderGenerator = () => { sendCommand('continue_order_generator'); updateUIForOrderGeneratorState(false); };
    const updateOrderInterval = () => { const newInterval = parseInt(elements.newOrderInterval.value); if (newInterval > 0) { sendCommand('update_order_interval', { interval: newInterval * 1000 }); } };
    const updateConnectionStatus = (connected) => { elements.connectionStatus.classList.toggle('connected', connected); elements.connectionStatus.classList.toggle('reconnecting', !connected && reconnectAttempts > 0); elements.connectionText.textContent = connected ? 'Connected' : (reconnectAttempts > 0 ? `Reconnecting...` : 'Disconnected'); };
    const updateUIForSimulationState = (state) => {
        appState.simulationState = state;
        const isIdle = state === 'idle';
        elements.configPanel.style.display = isIdle ? 'block' : 'none';
        elements.runtimePanels.style.display = isIdle ? 'none' : 'flex';
        elements.mapPanel.style.display = isIdle ? 'none' : 'block';
        elements.configControls.style.display = isIdle ? 'flex' : 'none';
        elements.runtimeControls.style.display = isIdle ? 'none' : 'flex';
        elements.zonesStatus.style.display = isIdle ? 'none' : 'block';
        elements.configFooter.style.display = isIdle ? 'block' : 'none';
        elements.pauseSimBtn.style.display = state === 'running' ? 'inline-block' : 'none';
        elements.continueSimBtn.style.display = state === 'paused' ? 'inline-block' : 'none';
        if (state === 'running') { updateUIForOrderGeneratorState(false); elements.pauseOrderGenBtn.disabled = false; }
        else if (state === 'paused') { elements.pauseOrderGenBtn.disabled = true; elements.continueOrderGenBtn.disabled = true; elements.orderIntervalControl.style.display = 'none'; }
        if (isIdle) { elements.startSimBtn.disabled = false; elements.startSimBtn.textContent = 'Start Simulation'; appState.couriers = {}; appState.packages = {}; appState.zones = {}; appState.mapData.courierPositions = {}; if (mapVisualization) { mapVisualization.clearAll(); } renderAll(); }
        else { if (elements.zonesContainer.children.length === 0) { generateZonePanels(); } }
    };
    const updateUIForOrderGeneratorState = (isPaused) => { elements.pauseOrderGenBtn.style.display = !isPaused ? 'inline-block' : 'none'; elements.continueOrderGenBtn.style.display = isPaused ? 'inline-block' : 'none'; elements.orderIntervalControl.style.display = isPaused ? 'flex' : 'none'; };
    const renderAll = () => { renderCouriers(); renderPackages(); if (elements.zonesContainer.children.length === 0 && appState.simulationState !== 'idle') { generateZonePanels(); } Object.values(appState.zones).forEach(updateZoneDisplay); updateStats(); };
    const createCourierElement = (courier) => { const div = document.createElement('div'); div.className = `courier-item ${courier.status || 'unknown'}`; div.id = `courier-${courier.id}`; const statusText = (courier.status || 'unknown').replace(/_/g, ' ').toUpperCase(); let detailsHTML = '<div class="courier-details">'; if (courier.current_package) { detailsHTML += `<strong>Package:</strong> ${courier.current_package}<br>`; if (courier.destination) { detailsHTML += `<strong>Destination:</strong> ${courier.destination}<br>`; } if (courier.eta && courier.eta > 0) { detailsHTML += `<strong>ETA:</strong> ${formatETA(courier.eta)}`; } } detailsHTML += '</div>'; const totalDelivered = courier.total_delivered || 0; div.innerHTML = `<div class="courier-header"><span class="courier-name">${(courier.id || 'unknown').toUpperCase()}</span><span class="courier-status ${courier.status || 'unknown'}">${statusText}</span></div>${detailsHTML}<div class="courier-stats"><span><strong>Total Delivered:</strong> ${totalDelivered}</span></div>`; return div; };
    const updateCourierDisplay = (courier) => { const existing = document.getElementById(`courier-${courier.id}`); if (existing) { existing.replaceWith(createCourierElement(courier)); } else { renderCouriers(); } };
    const renderCouriers = () => { elements.couriersList.innerHTML = ''; Object.values(appState.couriers).sort((a, b) => parseInt(a.id.replace('courier', '')) - parseInt(b.id.replace('courier', ''))).forEach(c => elements.couriersList.appendChild(createCourierElement(c))); };
    const createPackageElement = (pkg) => { const div = document.createElement('div'); div.className = `order-item ${pkg.status || 'unknown'}`; div.id = `package-${pkg.id}`; const statusText = (pkg.status || 'unknown').replace(/_/g, ' ').toUpperCase(); let detailsHTML = '<div class="order-details">'; if (pkg.zone) detailsHTML += `Zone: ${pkg.zone}<br>`; if (pkg.courier) detailsHTML += `Courier: ${pkg.courier}`; detailsHTML += '</div>'; div.innerHTML = `<div class="order-header"><span class="order-id">Order #${pkg.id || 'unknown'}</span><span class="order-status ${pkg.status || 'unknown'}">${statusText}</span></div>${detailsHTML}`; return div; };
    const updatePackageDisplay = (pkg) => { const existing = document.getElementById(`package-${pkg.id}`); if (existing) { existing.replaceWith(createPackageElement(pkg)); } else { renderPackages(); } };
    const renderPackages = () => { elements.ordersList.innerHTML = ''; Object.values(appState.packages).sort((a, b) => (b.created_at || 0) - (a.created_at || 0)).forEach(p => elements.ordersList.appendChild(createPackageElement(p))); };
    const updateStats = () => { const packages = Object.values(appState.packages); const stats = { total: packages.length, pending: packages.filter(p => ['ordered', 'assigned'].includes(p.status)).length, inTransit: packages.filter(p => ['picking_up', 'in_transit', 'delivering'].includes(p.status)).length, delivered: packages.filter(p => p.status === 'delivered').length }; elements.totalOrders.textContent = stats.total; elements.pendingOrders.textContent = stats.pending; elements.inTransitOrders.textContent = stats.inTransit; elements.deliveredOrders.textContent = stats.delivered; };
    const generateZonePanels = () => { elements.zonesContainer.innerHTML = ''; FIXED_ZONES.forEach(zone => { const zoneDiv = document.createElement('div'); zoneDiv.className = 'zone-item'; zoneDiv.setAttribute('data-zone', zone); zoneDiv.innerHTML = `<h4>${zone.charAt(0).toUpperCase() + zone.slice(1)} Zone</h4><div class="zone-stats"><span>Total: <span class="zone-total">0</span></span><span>Waiting: <span class="zone-waiting">0</span></span><span>Active: <span class="zone-active">0</span></span><span>Delivered: <span class="zone-delivered">0</span></span></div>`; elements.zonesContainer.appendChild(zoneDiv); }); };
    const updateZoneDisplay = (zone) => { const zoneEl = document.querySelector(`[data-zone="${zone.zone}"]`); if (zoneEl) { zoneEl.querySelector('.zone-waiting').textContent = zone.waiting_packages || 0; zoneEl.querySelector('.zone-active').textContent = zone.active_deliveries || 0; zoneEl.querySelector('.zone-delivered').textContent = zone.total_delivered || 0; zoneEl.querySelector('.zone-total').textContent = zone.total_orders || 0; } };
    const formatETA = (eta) => { if (!eta || eta <= 0) return 'N/A'; const totalSeconds = Math.floor(eta / 1000); const minutes = Math.floor(totalSeconds / 60); const seconds = totalSeconds % 60; return `${minutes}m ${seconds.toString().padStart(2, '0')}s`; };
    const validateFormInputs = () => {
        const numCouriers = parseInt(elements.numCouriersInput.value, 10);
        const orderInterval = parseInt(elements.orderIntervalInput.value, 10);
        const mapSize = parseInt(elements.mapSizeSelect.value, 10);
        const validMap = mapSize === 100 || mapSize === 200;
        elements.startSimBtn.disabled = !(
            numCouriers > 0 && numCouriers <= 200 &&
            orderInterval > 0 && orderInterval <= 300 &&
            validMap
        );
    };

    // --- הערה: אתחול האפליקציה ---
    const init = () => {
        setupEventListeners();
        validateFormInputs();
        updateUIForSimulationState('idle');
        connectWebSocket();
    };

    const setupEventListeners = () => {
        elements.startSimBtn.addEventListener('click', startSimulation);
        elements.resetBtn.addEventListener('click', resetSimulation);
        elements.pauseSimBtn.addEventListener('click', pauseSimulation);
        elements.continueSimBtn.addEventListener('click', continueSimulation);
        elements.pauseOrderGenBtn.addEventListener('click', pauseOrderGenerator);
        elements.continueOrderGenBtn.addEventListener('click', continueOrderGenerator);
        elements.updateOrderIntervalBtn.addEventListener('click', updateOrderInterval);
        elements.configForm.addEventListener('input', validateFormInputs);
    };
    
    // --- הערה: התחלת ריצת האפליקציה ---
    init();
});