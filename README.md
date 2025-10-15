# Erlang Logistics Simulator

## Overview

This project is a real-time logistics and delivery simulation built entirely in Erlang/OTP. It demonstrates a robust, concurrent system managing a fleet of couriers, package deliveries, and geographical zones. The simulation is visualized and controlled through a dynamic web-based dashboard that communicates with the Erlang backend via WebSockets.

The system is designed with a fault-tolerant architecture using OTP principles, featuring supervisors, `gen_statem` for finite state machines, and `gen_server` for various server processes.

## Features

  * **Real-time Web Dashboard:** A web interface built with HTML, CSS, and JavaScript provides a live view of the simulation, including courier statuses, order tracking, and a real-time map visualization.
  * **Dynamic Simulation Control:** The simulation can be started, stopped, paused, and resumed directly from the web dashboard.
  * **Configurable Parameters:** Users can configure key simulation parameters before starting, such as the number of couriers, the rate of order generation, and the size of the map to be used.
  * **Map & Navigation:**
      * Loads map data (homes, businesses, roads) from JSON files.
      * Constructs a graph representation of the map for efficient routing.
      * Implements Dijkstra's algorithm for optimal pathfinding between locations.
      * A `location_tracker` module simulates courier movement along calculated routes in real-time.
  * **OTP-Based Architecture:** The core logic is built using OTP behaviors for scalability and fault tolerance:
      * **`gen_statem`:** Used to model the complex states of the `control_center`, `courier`s, `package`s, and `zone_manager`s.
      * **Supervisors:** A hierarchical supervision tree (`logistics_sim_sup` and `simulation_supervisor`) ensures that all processes are monitored and restarted if they fail.
  * **Zone Management:** The map is divided into distinct zones (`north`, `center`, `south`), each managed by its own `zone_manager` process responsible for dispatching orders within its area.
  * **Centralized State Management:** A `logistics_state_collector` acts as a central event bus, aggregating state changes from all components and broadcasting them to the web interface.
  * **Efficient Resource Handling:** A `courier_pool` manages the queue of available couriers, ensuring that waiting delivery zones are assigned a courier as soon as one becomes available.

## Architecture

The application is structured around a supervision tree managed by `rebar3`.

  * **`logistics_sim_app` & `logistics_sim_sup`:** The main application and its top-level supervisor, which starts all core infrastructure services.
  * **`control_center`:** The brain of the simulation. It's a finite state machine that manages the overall simulation lifecycle (idle, initializing, running, paused, etc.) and dynamically starts/stops the simulation components.
  * **`simulation_supervisor`:** A dynamic supervisor started by the `control_center` to manage all processes that are specific to a single simulation run (couriers, zone managers, etc.).
  * **Erlang Modules:**
      * **`courier`:** An FSM representing a single courier, managing its state (idle, picking\_up, delivering) and movement.
      * **`package`:** An FSM for a single package, tracking its status from `ordered` to `delivered`.
      * **`zone_manager`:** An FSM for each geographical zone, responsible for handling new packages and assigning them to available couriers from the pool.
      * **`courier_pool`:** A `gen_server` that manages a FIFO queue of available couriers and a queue of zones waiting for a courier.
      * **`random_order_generator`:** A `gen_server` that periodically creates new package orders in random zones.
      * **`map_server` & `map_loader`:** Handle loading map data from JSON files, building the graph, and providing an API for pathfinding and location queries.
      * **`location_tracker`:** A `gen_server` that simulates the real-time movement of couriers along their routes, calculating their position at each tick and broadcasting updates.
  * **Web Interface:**
      * **`logistics_web_server` & `logistics_ws_handler`:** A Cowboy-based web server that serves the static frontend files (`index.html`, `app.js`, `style.css`) and handles the WebSocket connection for real-time communication.
      * The frontend (`app.js`) connects to the WebSocket, sends user commands, and receives state updates to render the live dashboard and map.

## How to Run

1.  **Build the application:**

    ```bash
    rebar3 compile
    ```

2.  **Start the Erlang shell:**

    ```bash
    rebar3 shell
    ```

3.  **Open your web browser** and navigate to:
    [http://localhost:8080](https://www.google.com/search?q=http://localhost:8080)

## Configuration

The simulation can be configured through the web interface before starting. The following parameters are available:

  * **Number of Couriers:** The total number of couriers to be included in the simulation.
  * **Order Generation Interval:** The time in seconds between the creation of new random orders.
  * **Map Size:** Choose between pre-defined maps with 100, 200, or 1043 homes.

## License

This project is licensed under the Apache License 2.0. See the [LICENSE.md](LICENSE.md) file for details.
