/**
 * REST API interfaces (Controllers) for the hexagonal architecture.
 *
 * <p>These controllers serve as primary adapters (driving adapters) that receive HTTP requests and
 * delegate to application use cases. They should:
 *
 * <ul>
 *   <li>Only depend on Application Layer (Use Cases via Ports)
 *   <li>Handle HTTP concerns (validation, serialization, status codes)
 *   <li>Transform DTOs to/from domain objects
 *   <li>Never contain business logic
 * </ul>
 */
package com.rulex.interfaces.rest;
