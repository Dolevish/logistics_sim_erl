// Enhanced Logistics Simulator Dashboard with Configuration Support

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
    isPaused: false
};

// DOM Elements
const elements = {
    // Connection
    connectionStatus: document.getElementById('connectionStatus'),
    connectionText: document.getElementById('connectionText'),
    
    // Control buttons
    startSimBtn: document.getElementById('startSimBtn'),
    pauseBtn: document.getElementById('pauseBtn'),
    resumeBtn: document.getElementById('resumeBtn'),
    resetBtn: document.getElementById('resetBtn'),
    
    // Control panels
    configControls: document.getElementById('configControls'),
    runtimeControls: document.getElementById('runtimeControls'),
    
    // Main panels
    configPanel: document.getElementById('configPanel'),
    runtimePanels: document.getElementById('runtimePanels'),
    
    // Form elements
    configForm: document.getElementById('configForm'),
    // הוסר: zonesInput - כעת האזורים קבועים
    numCouriersInput: document.getElementById('numCouriers'),
    orderIntervalInput: document.getElementById('orderInterval'),
    minTravelTimeInput: document.getElementById('minTravelTime'),
    maxTravelTimeInput: document.getElementById('maxTravelTime'),
    
    // Runtime elements
    ordersList: document.getElementById('ordersList'),
    couriersList: document.getElementById('couriersList'),
    totalOrders: document.getElementById('totalOrders'),
    pendingOrders: document.getElementById('pendingOrders'),
    inTransitOrders: document.getElementById('inTransitOrders'),
    deliveredOrders: document.getElementById('deliveredOrders'),
    
    // Footer
    zonesStatus: document.getElementById('zonesStatus'),
    configFooter: document.getElementById('configFooter'),
    zonesContainer: document.getElementById('zonesContainer')
};

// Initialize the application
function init() {
    console.log('Initializing Enhanced Logistics Dashboard...');
    connectWebSocket();
    setupEventListeners();
    validateFormInputs();
    
    // Initial UI state
    updateUIForSimulationState('idle');
}

// Setup all event listeners
function setupEventListeners() {
    // Configuration controls
    elements.startSimBtn.addEventListener('click', startSimulation);
    
    // Runtime controls
    elements.pauseBtn.addEventListener('click', pauseSimulation);
    elements.resumeBtn.addEventListener('click', resumeSimulation);
    elements.resetBtn.addEventListener('click', resetSimulation);
    
    // Form validation
    elements.configForm.addEventListener('input', validateFormInputs);
    
    // Travel time validation
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
    // הוסר: בדיקת zones - כעת הם קבועים
    const numCouriers = parseInt(elements.numCouriersInput.value);
    const orderInterval = parseInt(elements.orderIntervalInput.value);
    const minTravel = parseInt(elements.minTravelTimeInput.value);
    const maxTravel = parseInt(elements.maxTravelTimeInput.value);
    
    const isValid = numCouriers > 0 && numCouriers <= 50 &&
                   orderInterval > 0 && orderInterval <= 300 &&
                   minTravel > 0 && maxTravel > minTravel;
    
    elements.startSimBtn.disabled = !isValid;
}

// Start simulation with configuration
function startSimulation() {
    // Build configuration object
    // עכשיו משתמשים באזורים הקבועים במקום לקרוא מהקלט
    const config = {
        zones: FIXED_ZONES, // שימוש באזורים הקבועים
        num_couriers: parseInt(elements.numCouriersInput.value),
        order_interval: parseInt(elements.orderIntervalInput.value) * 1000, // Convert to ms
        min_travel_time: parseInt(elements.minTravelTimeInput.value) * 1000, // Convert to ms
        max_travel_time: parseInt(elements.maxTravelTimeInput.value) * 1000 // Convert to ms
    };
    
    // Store configuration
    appState.configuration = config;
    
    // Generate zone panels immediately with the configuration
    generateZonePanels();
    
    // Send start command
    sendCommand('start_simulation', config);
    
    // Update UI immediately to show loading state
    elements.startSimBtn.disabled = true;
    elements.startSimBtn.textContent = 'Starting...';
}

// Pause simulation
function pauseSimulation() {
    sendCommand('pause_simulation');
    appState.isPaused = true;
    updateUIForSimulationState('paused');
}

// Resume simulation
function resumeSimulation() {
    sendCommand('resume_simulation');
    appState.isPaused = false;
    updateUIForSimulationState('running');
}

// Reset simulation
function resetSimulation() {
    if (confirm('Are you sure you want to reset the simulation? All current data will be lost.')) {
        sendCommand('stop_simulation');
        
        // Clear local state
        appState.couriers = {};
        appState.packages = {};
        appState.zones = {};
        
        // Clear UI
        elements.ordersList.innerHTML = '';
        elements.couriersList.innerHTML = '';
        elements.zonesContainer.innerHTML = '';
        updateStats();
    }
}

// Update UI based on simulation state
function updateUIForSimulationState(state) {
    appState.simulationState = state;
    
    switch(state) {
        case 'idle':
            // Show configuration panel
            elements.configPanel.style.display = 'block';
            elements.runtimePanels.style.display = 'none';
            elements.configControls.style.display = 'flex';
            elements.runtimeControls.style.display = 'none';
            elements.zonesStatus.style.display = 'none';
            elements.configFooter.style.display = 'block';
            
            // Reset start button
            elements.startSimBtn.disabled = false;
            elements.startSimBtn.textContent = 'Start Simulation';
            break;
            
        case 'running':
            // Show runtime panels
            elements.configPanel.style.display = 'none';
            elements.runtimePanels.style.display = 'flex';
            elements.configControls.style.display = 'none';
            elements.runtimeControls.style.display = 'flex';
            elements.zonesStatus.style.display = 'block';
            elements.configFooter.style.display = 'none';
            
            // Update button states
            elements.pauseBtn.disabled = false;
            elements.resumeBtn.disabled = true;
            
            // Always regenerate zone panels when running
            generateZonePanels();
            break;
            
        case 'paused':
            // Update button states
            elements.pauseBtn.disabled = true;
            elements.resumeBtn.disabled = false;
            break;
    }
}

// Generate zone status panels based on configuration
function generateZonePanels() {
    console.log('Generating zone panels...');
    elements.zonesContainer.innerHTML = '';
    
    // תמיד משתמשים באזורים הקבועים
    const zones = FIXED_ZONES;
    console.log('Zones to generate:', zones);
    
    zones.forEach(zone => {
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
        console.log('Created zone panel for:', zone);
    });
    
    console.log('Zone panels created:', elements.zonesContainer.children.length);
}

// Connect to WebSocket server
function connectWebSocket() {
    if (ws && ws.readyState === WebSocket.OPEN) {
        console.log('WebSocket already connected');
        return;
    }

    const wsUrl = `ws://${window.location.host}/websocket`;
    console.log(`Connecting to WebSocket at ${wsUrl}... (attempt ${reconnectAttempts + 1})`);
    
    try {
        ws = new WebSocket(wsUrl);
        
        const connectionTimeout = setTimeout(() => {
            console.log('WebSocket connection timeout');
            if (ws.readyState !== WebSocket.OPEN) {
                ws.close();
            }
        }, 10000);
        
        ws.onopen = () => {
            clearTimeout(connectionTimeout);
            console.log('WebSocket connected successfully');
            isConnected = true;
            reconnectAttempts = 0;
            updateConnectionStatus(true);
            clearInterval(reconnectInterval);
            
            // Request current simulation state
            setTimeout(() => {
                ws.send(JSON.stringify({
                    type: 'request_simulation_state'
                }));
            }, 100);
        };
        
        ws.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data);
                handleWebSocketMessage(data);
            } catch (error) {
                console.error('Error parsing WebSocket message:', error);
            }
        };
        
        ws.onerror = (error) => {
            console.error('WebSocket error:', error);
            isConnected = false;
            updateConnectionStatus(false);
        };
        
        ws.onclose = (event) => {
            clearTimeout(connectionTimeout);
            console.log(`WebSocket disconnected (code: ${event.code})`);
            isConnected = false;
            updateConnectionStatus(false);
            attemptReconnect();
        };
        
    } catch (error) {
        console.error('Failed to create WebSocket:', error);
        isConnected = false;
        updateConnectionStatus(false);
    }
}

// Handle reconnection attempts
function attemptReconnect() {
    if (reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
        const backoffDelay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
        console.log(`Attempting to reconnect in ${backoffDelay}ms...`);
        
        clearInterval(reconnectInterval);
        reconnectInterval = setTimeout(() => {
            reconnectAttempts++;
            connectWebSocket();
        }, backoffDelay);
    }
}

// Handle incoming WebSocket messages
function handleWebSocketMessage(data) {
    console.log('Received:', data.type, data);
    
    switch (data.type) {
        case 'simulation_state':
            handleSimulationStateUpdate(data.state, data.config);
            break;
            
        case 'state_update':
            handleStateUpdate(data.update_type, data.data);
            break;
            
        case 'command_response':
            handleCommandResponse(data.command, data.success, data.message);
            break;
            
        case 'heartbeat':
            // תגובה ל-heartbeat מהשרת
            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify({type: 'pong'}));
            }
            break;
            
        case 'pong':
            // תגובה מהשרת ל-ping שלנו - אין צורך לעשות כלום
            break;
            
        default:
            console.log('Unknown message type:', data.type);
    }
}

// Handle simulation state updates
function handleSimulationStateUpdate(state, config) {
    console.log('Simulation state:', state);
    console.log('Simulation config:', config);
    
    if (config) {
        // תמיד נשתמש באזורים הקבועים
        config.zones = FIXED_ZONES;
        appState.configuration = config;
    }
    
    updateUIForSimulationState(state);
    
    // If simulation just started, request full state
    if (state === 'running') {
        setTimeout(() => {
            sendCommand('request_full_state');
        }, 500);
    }
}

// Handle command responses
function handleCommandResponse(command, success, message) {
    console.log(`Command ${command}: ${success ? 'Success' : 'Failed'} - ${message}`);
    
    if (command === 'start_simulation' && !success) {
        // Reset UI if start failed
        updateUIForSimulationState('idle');
        alert(`Failed to start simulation: ${message}`);
    }
}

// Handle state updates (existing functionality)
function handleStateUpdate(updateType, data) {
    try {
        console.log('Handling state update:', updateType, data);
        
        switch (updateType) {
            case 'couriers_init':
                if (Array.isArray(data)) {
                    data.forEach(courier => {
                        appState.couriers[courier.id] = courier;
                    });
                    renderCouriers();
                }
                break;
                
            case 'packages_init':
                if (Array.isArray(data)) {
                    data.forEach(pkg => {
                        appState.packages[pkg.id] = pkg;
                    });
                    renderPackages();
                    updateStats();
                }
                break;
                
            case 'courier_update':
                if (data && data.id) {
                    const existingCourier = appState.couriers[data.id] || {};
                    appState.couriers[data.id] = {
                        ...existingCourier,
                        ...data,
                        delivered_packages: data.delivered_packages || existingCourier.delivered_packages || [],
                        total_delivered: data.total_delivered !== undefined ? 
                            data.total_delivered : 
                            (existingCourier.total_delivered || 0)
                    };
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
                    // Make sure zone panels exist
                    if (elements.zonesContainer.children.length === 0) {
                        console.log('Zone panels missing, generating them now...');
                        generateZonePanels();
                    }
                    
                    appState.zones[data.zone] = data;
                    updateZoneDisplay(data);
                }
                break;
                
            case 'full_state':
                console.log('Received full state:', data);
                
                // Clear existing data
                appState.couriers = {};
                appState.packages = {};
                appState.zones = {};
                
                if (data.couriers && Array.isArray(data.couriers)) {
                    data.couriers.forEach(courier => {
                        appState.couriers[courier.id] = courier;
                    });
                    renderCouriers();
                }
                
                if (data.packages && Array.isArray(data.packages)) {
                    data.packages.forEach(pkg => {
                        appState.packages[pkg.id] = pkg;
                    });
                    renderPackages();
                }
                
                if (data.zones && Array.isArray(data.zones)) {
                    // Make sure we have zone panels created
                    if (elements.zonesContainer.children.length === 0) {
                        generateZonePanels();
                    }
                    
                    data.zones.forEach(zone => {
                        appState.zones[zone.zone] = zone;
                        updateZoneDisplay(zone);
                    });
                }
                
                updateStats();
                break;
        }
    } catch (error) {
        console.error('Error in handleStateUpdate:', error);
    }
}

// Send command to server
function sendCommand(action, data = {}) {
    if (ws && ws.readyState === WebSocket.OPEN) {
        try {
            const message = action === 'request_full_state' ? 
                { type: 'request_full_state' } :
                { type: 'command', action: action, ...data };
                
            console.log('Sending command:', message);
            ws.send(JSON.stringify(message));
        } catch (error) {
            console.error('Error sending command:', error);
        }
    } else {
        console.warn('WebSocket not connected, cannot send command:', action);
        connectWebSocket();
    }
}

// Update connection status indicator
function updateConnectionStatus(connected) {
    if (connected) {
        elements.connectionStatus.classList.add('connected');
        elements.connectionStatus.classList.remove('reconnecting');
        elements.connectionText.textContent = 'Connected';
    } else {
        elements.connectionStatus.classList.remove('connected');
        if (reconnectAttempts > 0 && reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
            elements.connectionStatus.classList.add('reconnecting');
            elements.connectionText.textContent = `Reconnecting... (${reconnectAttempts}/${MAX_RECONNECT_ATTEMPTS})`;
        } else {
            elements.connectionStatus.classList.remove('reconnecting');
            elements.connectionText.textContent = 'Disconnected';
        }
    }
}

// Render all couriers
function renderCouriers() {
    elements.couriersList.innerHTML = '';
    Object.values(appState.couriers).forEach(courier => {
        try {
            const courierElement = createCourierElement(courier);
            elements.couriersList.appendChild(courierElement);
        } catch (error) {
            console.error('Error rendering courier:', courier, error);
        }
    });
}

// Create courier DOM element
function createCourierElement(courier) {
    const div = document.createElement('div');
    div.className = `courier-item ${courier.status || 'unknown'}`;
    div.id = `courier-${courier.id}`;
    
    const statusText = (courier.status || 'unknown').replace('_', ' ').toUpperCase();
    
    let detailsHTML = '';
    if (courier.current_package) {
        detailsHTML = `
            <div class="courier-details">
                <strong>Current Package:</strong> ${courier.current_package}<br>
                ${courier.zone ? `<strong>Zone:</strong> ${courier.zone}<br>` : ''}
                ${courier.eta ? `<strong>ETA:</strong> ${formatETA(courier.eta)}` : ''}
            </div>
        `;
    }
    
    let deliveredHTML = '';
    const deliveredPackages = courier.delivered_packages || [];
    const totalDelivered = courier.total_delivered || deliveredPackages.length || 0;
    
    if (deliveredPackages.length > 0) {
        const recentDeliveries = deliveredPackages.slice(-3);
        deliveredHTML = `<div class="delivered-list">Recent: ${recentDeliveries.join(', ')}</div>`;
    }
    
    div.innerHTML = `
        <div class="courier-header">
            <span class="courier-name">${(courier.id || 'unknown').toUpperCase()}</span>
            <span class="courier-status ${courier.status || 'unknown'}">${statusText}</span>
        </div>
        ${detailsHTML}
        <div class="courier-stats">
            <span><strong>Total Delivered:</strong> ${totalDelivered}</span>
        </div>
        ${deliveredHTML}
    `;
    
    return div;
}

// Update single courier display
function updateCourierDisplay(courier) {
    const existingElement = document.getElementById(`courier-${courier.id}`);
    if (existingElement) {
        existingElement.classList.add('updating');
        const newElement = createCourierElement(courier);
        existingElement.replaceWith(newElement);
        setTimeout(() => {
            const updatedElement = document.getElementById(`courier-${courier.id}`);
            if (updatedElement) {
                updatedElement.classList.remove('updating');
            }
        }, 1000);
    } else {
        renderCouriers();
    }
}

// Render all packages
function renderPackages() {
    elements.ordersList.innerHTML = '';
    
    const sortedPackages = Object.values(appState.packages)
        .sort((a, b) => (b.created_at || 0) - (a.created_at || 0));
    
    sortedPackages.forEach(pkg => {
        try {
            const packageElement = createPackageElement(pkg);
            elements.ordersList.appendChild(packageElement);
        } catch (error) {
            console.error('Error rendering package:', pkg, error);
        }
    });
}

// Create package DOM element
function createPackageElement(pkg) {
    const div = document.createElement('div');
    div.className = `order-item ${pkg.status || 'unknown'}`;
    div.id = `package-${pkg.id}`;
    
    const statusText = (pkg.status || 'unknown').replace('_', ' ').toUpperCase();
    
    let detailsHTML = '<div class="order-details">';
    if (pkg.zone) {
        detailsHTML += `Zone: ${pkg.zone}<br>`;
    }
    if (pkg.courier) {
        detailsHTML += `Courier: ${pkg.courier}<br>`;
        if (pkg.status === 'in_transit' || pkg.status === 'delivering') {
            detailsHTML += `Status: Courier is delivering to customer`;
        } else if (pkg.status === 'assigned' || pkg.status === 'picking_up') {
            detailsHTML += `Status: Courier is heading to restaurant`;
        }
    } else if (pkg.status === 'ordered') {
        detailsHTML += `Status: Waiting for courier assignment`;
    }
    detailsHTML += '</div>';
    
    div.innerHTML = `
        <div class="order-header">
            <span class="order-id">Order #${pkg.id || 'unknown'}</span>
            <span class="order-status ${pkg.status || 'unknown'}">${statusText}</span>
        </div>
        ${detailsHTML}
    `;
    
    return div;
}

// Update single package display
function updatePackageDisplay(pkg) {
    const existingElement = document.getElementById(`package-${pkg.id}`);
    if (existingElement) {
        existingElement.classList.add('updating');
        const newElement = createPackageElement(pkg);
        existingElement.replaceWith(newElement);
        setTimeout(() => {
            const updatedElement = document.getElementById(`package-${pkg.id}`);
            if (updatedElement) {
                updatedElement.classList.remove('updating');
            }
        }, 1000);
    } else {
        renderPackages();
    }
}

// Update zone display
function updateZoneDisplay(zone) {
    console.log('Updating zone display:', zone);
    const zoneElement = document.querySelector(`[data-zone="${zone.zone}"]`);
    console.log('Zone element found:', zoneElement);
    
    if (zoneElement) {
        const waitingEl = zoneElement.querySelector('.zone-waiting');
        const activeEl = zoneElement.querySelector('.zone-active');
        const deliveredEl = zoneElement.querySelector('.zone-delivered');
        const totalEl = zoneElement.querySelector('.zone-total');
        
        if (waitingEl) waitingEl.textContent = zone.waiting_packages || 0;
        if (activeEl) activeEl.textContent = zone.active_deliveries || 0;
        if (deliveredEl) deliveredEl.textContent = zone.total_delivered || 0;
        if (totalEl) totalEl.textContent = zone.total_orders || 0;
        
        console.log('Zone stats updated:', {
            zone: zone.zone,
            total: zone.total_orders || 0,
            waiting: zone.waiting_packages || 0,
            active: zone.active_deliveries || 0,
            delivered: zone.total_delivered || 0
        });
    } else {
        console.log('Zone element not found for:', zone.zone);
    }
}

// Update statistics
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

// Format ETA for display
function formatETA(eta) {
    try {
        if (!eta) return '';
        const seconds = Math.floor(eta / 1000);
        const minutes = Math.floor(seconds / 60);
        const remainingSeconds = seconds % 60;
        return `${minutes}m ${remainingSeconds}s`;
    } catch (error) {
        console.error('Error formatting ETA:', error);
        return '';
    }
}

// Start the application when DOM is loaded
document.addEventListener('DOMContentLoaded', init);