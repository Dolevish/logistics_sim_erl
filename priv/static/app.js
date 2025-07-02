// Logistics Simulator Dashboard - Enhanced WebSocket Connection

// WebSocket connection
let ws = null;
let reconnectInterval = null;
let isConnected = false;
let reconnectAttempts = 0;
const MAX_RECONNECT_ATTEMPTS = 10;

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
    
    setInterval(() => {
        if (!isConnected) {
            console.log('Auto-reconnecting due to disconnect...');
            connectWebSocket();
        }
    }, 30000);
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

// Connect to WebSocket server with enhanced reconnection
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
            
            setTimeout(() => {
                ws.send(JSON.stringify({
                    type: 'request_full_state'
                }));
            }, 100);
        };
        
        ws.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data);
                handleWebSocketMessage(data);
            } catch (error) {
                console.error('Error parsing WebSocket message:', error);
                console.log('Raw message:', event.data);
            }
        };
        
        ws.onerror = (error) => {
            console.error('WebSocket error:', error);
            isConnected = false;
            updateConnectionStatus(false);
        };
        
        ws.onclose = (event) => {
            clearTimeout(connectionTimeout);
            console.log(`WebSocket disconnected (code: ${event.code}, reason: ${event.reason})`);
            isConnected = false;
            updateConnectionStatus(false);
            
            if (reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
                const backoffDelay = Math.min(1000 * Math.pow(2, reconnectAttempts), 30000);
                console.log(`Attempting to reconnect in ${backoffDelay}ms...`);
                
                clearInterval(reconnectInterval);
                reconnectInterval = setTimeout(() => {
                    reconnectAttempts++;
                    connectWebSocket();
                }, backoffDelay);
            } else {
                console.log('Max reconnection attempts reached. Manual refresh required.');
                elements.connectionText.textContent = 'Connection failed - Please refresh';
            }
        };
        
    } catch (error) {
        console.error('Failed to create WebSocket:', error);
        isConnected = false;
        updateConnectionStatus(false);
    }
}

// Handle incoming WebSocket messages with better error handling
function handleWebSocketMessage(data) {
    try {
        console.log('Received:', data.type, data);
        
        switch (data.type) {
            case 'state_update':
                handleStateUpdate(data.update_type, data.data);
                break;
            case 'heartbeat':
                if (ws && ws.readyState === WebSocket.OPEN) {
                    ws.send(JSON.stringify({type: 'pong'}));
                }
                break;
            case 'pong':
                console.log('Received pong from server');
                break;
            default:
                console.warn('Unknown message type:', data.type);
        }
    } catch (error) {
        console.error('Error handling WebSocket message:', error);
    }
}

// Handle state updates with better error handling
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
                            (existingCourier.total_delivered || (data.delivered_packages || existingCourier.delivered_packages || []).length)
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
                    appState.zones[data.zone] = data;
                    updateZoneDisplay(data);
                }
                break;
                
            case 'full_state':
                console.log('Received full state:', data);
                
                if (data.couriers && Array.isArray(data.couriers)) {
                    appState.couriers = {};
                    data.couriers.forEach(courier => {
                        appState.couriers[courier.id] = courier;
                    });
                    renderCouriers();
                }
                
                if (data.packages && Array.isArray(data.packages)) {
                    appState.packages = {};
                    data.packages.forEach(pkg => {
                        appState.packages[pkg.id] = pkg;
                    });
                    renderPackages();
                }
                
                if (data.zones && Array.isArray(data.zones)) {
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

// Send command to server with retry logic
function sendCommand(action) {
    if (ws && ws.readyState === WebSocket.OPEN) {
        try {
            ws.send(JSON.stringify({
                type: 'command',
                action: action
            }));
        } catch (error) {
            console.error('Error sending command:', error);
        }
    } else {
        console.warn('WebSocket not connected, cannot send command:', action);
        connectWebSocket();
    }
}

// Update connection status indicator with more states
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
    try {
        elements.couriersList.innerHTML = '';
        Object.values(appState.couriers).forEach(courier => {
            try {
                const courierElement = createCourierElement(courier);
                elements.couriersList.appendChild(courierElement);
            } catch (error) {
                console.error('Error rendering courier:', courier, error);
            }
        });
    } catch (error) {
        console.error('Error in renderCouriers:', error);
    }
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
    try {
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
    } catch (error) {
        console.error('Error updating courier display:', error);
    }
}

// Render all packages
function renderPackages() {
    try {
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
    } catch (error) {
        console.error('Error in renderPackages:', error);
    }
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
    try {
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
    } catch (error) {
        console.error('Error updating package display:', error);
    }
}

// Update zone display
function updateZoneDisplay(zone) {
    try {
        const zoneElement = document.querySelector(`[data-zone="${zone.zone}"]`);
        if (zoneElement) {
            const waitingEl = zoneElement.querySelector('.zone-waiting');
            const activeEl = zoneElement.querySelector('.zone-active');
            const deliveredEl = zoneElement.querySelector('.zone-delivered');
            // הערה חדשה: בחירת האלמנט החדש שהוספנו
            const totalEl = zoneElement.querySelector('.zone-total');
            
            if (waitingEl) waitingEl.textContent = zone.waiting_packages || 0;
            if (activeEl) activeEl.textContent = zone.active_deliveries || 0;
            if (deliveredEl) deliveredEl.textContent = zone.total_delivered || 0;
            // הערה חדשה: עדכון התוכן של האלמנט החדש
            if (totalEl) totalEl.textContent = zone.total_orders || 0;
        }
    } catch (error) {
        console.error('Error updating zone display:', error);
    }
}

// Update statistics
function updateStats() {
    try {
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
    } catch (error) {
        console.error('Error updating stats:', error);
    }
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
