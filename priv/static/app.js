// Logistics Simulator Dashboard - Client Side JavaScript

// WebSocket connection
let ws = null;
let reconnectInterval = null;
let isConnected = false;

// Application state
const appState = {
    couriers: {},
    packages: {},
    zones: {},
    isPaused: false
};

// DOM Elements
const elements = {
    connectionStatus: document.getElementById('connectionStatus'),
    connectionText: document.getElementById('connectionText'),
    pauseBtn: document.getElementById('pauseBtn'),
    resumeBtn: document.getElementById('resumeBtn'),
    emergencyBtn: document.getElementById('emergencyBtn'),
    ordersList: document.getElementById('ordersList'),
    couriersList: document.getElementById('couriersList'),
    totalOrders: document.getElementById('totalOrders'),
    pendingOrders: document.getElementById('pendingOrders'),
    inTransitOrders: document.getElementById('inTransitOrders'),
    deliveredOrders: document.getElementById('deliveredOrders')
};

// Initialize the application
function init() {
    console.log('Initializing Logistics Dashboard...');
    connectWebSocket();
    setupEventListeners();
}

// Setup event listeners for control buttons
function setupEventListeners() {
    elements.pauseBtn.addEventListener('click', () => {
        sendCommand('pause_simulation');
        elements.pauseBtn.disabled = true;
        elements.resumeBtn.disabled = false;
        appState.isPaused = true;
    });

    elements.resumeBtn.addEventListener('click', () => {
        sendCommand('resume_simulation');
        elements.pauseBtn.disabled = false;
        elements.resumeBtn.disabled = true;
        appState.isPaused = false;
    });

    elements.emergencyBtn.addEventListener('click', () => {
        if (confirm('Are you sure you want to perform an emergency stop?')) {
            sendCommand('emergency_stop');
        }
    });
}

// Connect to WebSocket server
function connectWebSocket() {
    const wsUrl = `ws://${window.location.host}/websocket`;
    console.log(`Connecting to WebSocket at ${wsUrl}...`);
    
    ws = new WebSocket(wsUrl);
    
    ws.onopen = () => {
        console.log('WebSocket connected');
        isConnected = true;
        updateConnectionStatus(true);
        clearInterval(reconnectInterval);
        
        // Request full state on connection
        ws.send(JSON.stringify({
            type: 'request_full_state'
        }));
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
    };
    
    ws.onclose = () => {
        console.log('WebSocket disconnected');
        isConnected = false;
        updateConnectionStatus(false);
        
        // Attempt to reconnect every 5 seconds
        if (!reconnectInterval) {
            reconnectInterval = setInterval(() => {
                console.log('Attempting to reconnect...');
                connectWebSocket();
            }, 5000);
        }
    };
}

// Handle incoming WebSocket messages
function handleWebSocketMessage(data) {
    console.log('Received:', data.type, data);
    
    switch (data.type) {
        case 'state_update':
            handleStateUpdate(data.update_type, data.data);
            break;
        case 'heartbeat':
            // Keep connection alive
            break;
        case 'pong':
            // Response to ping
            break;
        default:
            console.warn('Unknown message type:', data.type);
    }
}

// Handle state updates
function handleStateUpdate(updateType, data) {
    switch (updateType) {
        case 'couriers_init':
            // Initialize all couriers
            data.forEach(courier => {
                appState.couriers[courier.id] = courier;
            });
            renderCouriers();
            break;
            
        case 'packages_init':
            // Initialize all packages
            data.forEach(pkg => {
                appState.packages[pkg.id] = pkg;
            });
            renderPackages();
            updateStats();
            break;
            
        case 'courier_update':
            // Update single courier
            appState.couriers[data.id] = data;
            updateCourierDisplay(data);
            break;
            
        case 'package_update':
            // Update single package
            appState.packages[data.id] = data;
            updatePackageDisplay(data);
            updateStats();
            break;
            
        case 'zone_update':
            // Update zone information
            appState.zones[data.zone] = data;
            updateZoneDisplay(data);
            break;
            
        case 'full_state':
            // Update entire state
            if (data.couriers) {
                data.couriers.forEach(courier => {
                    appState.couriers[courier.id] = courier;
                });
                renderCouriers();
            }
            if (data.packages) {
                data.packages.forEach(pkg => {
                    appState.packages[pkg.id] = pkg;
                });
                renderPackages();
            }
            if (data.zones) {
                data.zones.forEach(zone => {
                    appState.zones[zone.zone] = zone;
                    updateZoneDisplay(zone);
                });
            }
            updateStats();
            break;
    }
}

// Send command to server
function sendCommand(action) {
    if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({
            type: 'command',
            action: action
        }));
    }
}

// Update connection status indicator
function updateConnectionStatus(connected) {
    if (connected) {
        elements.connectionStatus.classList.add('connected');
        elements.connectionText.textContent = 'Connected';
    } else {
        elements.connectionStatus.classList.remove('connected');
        elements.connectionText.textContent = 'Disconnected';
    }
}

// Render all couriers
function renderCouriers() {
    elements.couriersList.innerHTML = '';
    
    Object.values(appState.couriers).forEach(courier => {
        const courierElement = createCourierElement(courier);
        elements.couriersList.appendChild(courierElement);
    });
}

// Create courier DOM element
function createCourierElement(courier) {
    const div = document.createElement('div');
    div.className = `courier-item ${courier.status}`;
    div.id = `courier-${courier.id}`;
    
    const statusText = courier.status.replace('_', ' ').toUpperCase();
    
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
    
    const deliveredHTML = courier.delivered_packages && courier.delivered_packages.length > 0 
        ? `<div class="delivered-list">Delivered: ${courier.delivered_packages.join(', ')}</div>`
        : '';
    
    div.innerHTML = `
        <div class="courier-header">
            <span class="courier-name">${courier.id.toUpperCase()}</span>
            <span class="courier-status ${courier.status}">${statusText}</span>
        </div>
        ${detailsHTML}
        <div class="courier-stats">
            <span>Total Delivered: ${courier.total_delivered || 0}</span>
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
            document.getElementById(`courier-${courier.id}`).classList.remove('updating');
        }, 1000);
    } else {
        renderCouriers();
    }
}

// Render all packages
function renderPackages() {
    elements.ordersList.innerHTML = '';
    
    // Sort packages by creation time (newest first)
    const sortedPackages = Object.values(appState.packages)
        .sort((a, b) => (b.created_at || 0) - (a.created_at || 0));
    
    sortedPackages.forEach(pkg => {
        const packageElement = createPackageElement(pkg);
        elements.ordersList.appendChild(packageElement);
    });
}

// Create package DOM element
function createPackageElement(pkg) {
    const div = document.createElement('div');
    div.className = `order-item ${pkg.status}`;
    div.id = `package-${pkg.id}`;
    
    const statusText = pkg.status.replace('_', ' ').toUpperCase();
    
    let detailsHTML = '<div class="order-details">';
    if (pkg.zone) {
        detailsHTML += `Zone: ${pkg.zone}<br>`;
    }
    if (pkg.courier) {
        detailsHTML += `Courier: ${pkg.courier}<br>`;
        if (pkg.status === 'in_transit') {
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
            <span class="order-id">Order #${pkg.id}</span>
            <span class="order-status ${pkg.status}">${statusText}</span>
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
            document.getElementById(`package-${pkg.id}`).classList.remove('updating');
        }, 1000);
    } else {
        renderPackages();
    }
}

// Update zone display
function updateZoneDisplay(zone) {
    const zoneElement = document.querySelector(`[data-zone="${zone.zone}"]`);
    if (zoneElement) {
        zoneElement.querySelector('.zone-waiting').textContent = zone.waiting_packages || 0;
        zoneElement.querySelector('.zone-active').textContent = zone.active_deliveries || 0;
        zoneElement.querySelector('.zone-delivered').textContent = zone.total_delivered || 0;
    }
}

// Update statistics
function updateStats() {
    const packages = Object.values(appState.packages);
    
    const stats = {
        total: packages.length,
        pending: packages.filter(p => p.status === 'ordered').length,
        inTransit: packages.filter(p => ['assigned', 'picking_up', 'in_transit'].includes(p.status)).length,
        delivered: packages.filter(p => p.status === 'delivered').length
    };
    
    elements.totalOrders.textContent = stats.total;
    elements.pendingOrders.textContent = stats.pending;
    elements.inTransitOrders.textContent = stats.inTransit;
    elements.deliveredOrders.textContent = stats.delivered;
}

// Format ETA for display
function formatETA(eta) {
    if (!eta) return '';
    const seconds = Math.floor(eta / 1000);
    const minutes = Math.floor(seconds / 60);
    const remainingSeconds = seconds % 60;
    return `${minutes}m ${remainingSeconds}s`;
}

// Start the application when DOM is loaded
document.addEventListener('DOMContentLoaded', init);