#!/usr/bin/env python3
"""
Simple test to validate IoT demo concepts
This tests the basic logic without relying on FluentAI runtime
"""

# Simulate sensor readings
class SensorReading:
    def __init__(self, id, timestamp, value, metadata):
        self.id = id
        self.timestamp = timestamp
        self.value = value
        self.metadata = metadata
    
    def __repr__(self):
        return f"SensorReading({self.id}, {self.timestamp}, {self.value}, {self.metadata})"

# Test data
test_readings = [
    SensorReading("temp-001", 1000, 25.5, {"type": "temperature"}),
    SensorReading("temp-002", 1100, 45.0, {"type": "temperature"}),  # Anomaly
    SensorReading("temp-003", 1200, 22.0, {"type": "temperature"}),
    SensorReading("pres-001", 1300, 1020.0, {"type": "pressure"}),
    SensorReading("temp-004", 1400, 55.0, {"type": "temperature"}),  # Anomaly
]

# Pipeline functions
def enrich_with_metadata(reading):
    """Add location metadata based on sensor ID"""
    location = "Building A" if reading.id.startswith("temp-") else "Building B"
    enriched = SensorReading(
        reading.id,
        reading.timestamp,
        reading.value,
        {**reading.metadata, "location": location, "processed_at": "now"}
    )
    return enriched

def detect_anomalies(reading):
    """Detect if reading is anomalous based on type and value"""
    if reading.metadata.get("type") == "temperature":
        return reading.value > 40.0 or reading.value < -10.0
    elif reading.metadata.get("type") == "pressure":
        return reading.value > 1100.0 or reading.value < 900.0
    return False

def log_anomaly(reading):
    """Log anomaly (in real impl would be side effect)"""
    print(f"ANOMALY: Sensor {reading.id} at {reading.timestamp} - Value: {reading.value}")
    return reading

# Test implementations
def process_v1(readings):
    """Naive implementation - separate passes"""
    enriched = [enrich_with_metadata(r) for r in readings]
    anomalies = [r for r in enriched if detect_anomalies(r)]
    logged = [log_anomaly(r) for r in anomalies]
    return logged

def process_v3(readings):
    """Optimized - single pass"""
    result = []
    for reading in readings:
        enriched = enrich_with_metadata(reading)
        if detect_anomalies(enriched):
            logged = log_anomaly(enriched)
            result.append(logged)
    return result

# Run tests
print("=== Testing IoT Pipeline Logic ===\n")

print(f"Test data: {len(test_readings)} readings")
print(f"Expected anomalies: 2 (temp > 40°C)\n")

print("--- V1: Naive Implementation ---")
result_v1 = process_v1(test_readings)
print(f"Found {len(result_v1)} anomalies\n")

print("--- V3: Optimized Implementation ---")
result_v3 = process_v3(test_readings)
print(f"Found {len(result_v3)} anomalies\n")

# Verify results are the same
assert len(result_v1) == len(result_v3), "Results should be equal"
assert all(r1.id == r2.id for r1, r2 in zip(result_v1, result_v3)), "Same anomalies detected"

print("✓ Both implementations produce identical results")
print("✓ Optimization maintains correctness")
print("\nThe IoT demo logic is sound!")