# Haskell Web Server and Client Simulation
A Haskell project simulating a multi-threaded web server and client interactions with concurrency, handling 100 HTTP-like request-response pairs, and logging them with timestamps.  Feel free to copy and paste this into your GitHub repository description! Let me know if there's anything else you'd like to adjust.

This repository contains a project for simulating a multi-threaded web server and client interactions using Haskell. The project involves concurrent computation and simulates HTTP-like requests and responses between a single web server and multiple client threads.

## Features

- Simulates one web server thread and ten client threads.
- Clients generate random HTTP-like requests at intervals and send them to the server.
- The server maintains a queue to process requests in the order they are received.
- Responses are timestamped and logged in a `requests.log` file.
- Uses Haskell's concurrency libraries, such as `forkIO` and `MVar`.
- Includes Haddock-style comments and modular organization for easy comprehension.

## Requirements

- GHC (The Glasgow Haskell Compiler)
- Stack (Haskell build tool)
- Suggested libraries:
  - `Control.Concurrent` (for concurrency tools like `forkIO` and `MVar`)
  - `Data.Time` (for timestamps)
  - `System.Random` (for generating random intervals and content)

## Project Structure

- **`src/`**: Contains Haskell source code, organized into modules.
  - `Request.hs`: Defines the `Request` data type to model HTTP-like requests.
  - `Response.hs`: Defines the `Response` data type to model HTTP-like responses.
  - `RequestQueue.hs`: Implements the queue for handling client requests.
  - `Main.hs`: The main entry point of the application. Spawns threads and coordinates client-server interaction.
- **`tests/`**: Contains test cases for different components.
- **`requests.log`**: Log file that records requests and responses with timestamps.
- **`README.md`**: Documentation for the repository.
- **`report.pdf`**: A one-page report describing the project, challenges faced, and design decisions.

## How to Run the Project

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/haskell-web-server-client-simulation.git
   cd haskell-web-server-client-simulation
   ```

2. Build the project using Stack:
   ```bash
   stack build
   ```

3. Run the project:
   ```bash
   stack exec haskell-web-server-client-simulation
   ```

4. Check the `requests.log` file for the recorded requests and responses.

## Design Decisions

- **Request and Response Types**: Designed to include timestamps for tracking interactions.
- **MVars**: Chosen to synchronize threads and ensure safe access to the request queue.
- **Queue Implementation**: Ensures first-come-first-served processing for requests.

## Extensions

To go beyond the basic requirements, the project includes:
- Enhanced logging with additional metadata (e.g., client ID, request type).
- Configurable number of clients and server threads via command-line arguments.
- Improved error handling and graceful shutdown.

## Challenges

Some challenges faced during development include:
- Managing concurrency and ensuring thread safety.
- Simulating random intervals and ensuring proper synchronization.
- Designing a clean and extensible architecture.

## Contributing
As this project is part of a university assignment, it is not open for external contributions.

## License
All rights reserved. This project is proprietary and is not licensed for use, reproduction, or distribution.
