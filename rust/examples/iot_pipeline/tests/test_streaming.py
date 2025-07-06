#!/usr/bin/env python3
"""
Test streaming concepts from IoT demo
Simulates channels and async processing
"""

import time
import random
from collections import deque
from threading import Thread, Lock

class Channel:
    """Simple channel implementation"""
    def __init__(self):
        self.queue = deque()
        self.lock = Lock()
        self.closed = False
    
    def send(self, value):
        with self.lock:
            if not self.closed:
                self.queue.append(value)
    
    def receive(self):
        with self.lock:
            if self.queue:
                return self.queue.popleft()
            elif self.closed:
                return None
        return None  # No data yet
    
    def close(self):
        with self.lock:
            self.closed = True

def stream_from_list(items):
    """Convert list to stream (channel)"""
    ch = Channel()
    def producer():
        for item in items:
            ch.send(item)
            time.sleep(0.001)  # Simulate async
        ch.close()
    Thread(target=producer).start()
    return ch

def stream_map(f, in_stream):
    """Map function over stream"""
    out_stream = Channel()
    def processor():
        while True:
            val = in_stream.receive()
            if val is None:
                time.sleep(0.001)
                if in_stream.closed:
                    break
            else:
                out_stream.send(f(val))
        out_stream.close()
    Thread(target=processor).start()
    return out_stream

def stream_filter(pred, in_stream):
    """Filter stream by predicate"""
    out_stream = Channel()
    def processor():
        while True:
            val = in_stream.receive()
            if val is None:
                time.sleep(0.001)
                if in_stream.closed:
                    break
            else:
                if pred(val):
                    out_stream.send(val)
        out_stream.close()
    Thread(target=processor).start()
    return out_stream

def stream_collect(stream):
    """Collect stream into list"""
    result = []
    while True:
        val = stream.receive()
        if val is None:
            time.sleep(0.001)
            if stream.closed:
                break
        else:
            result.append(val)
    return result

# Test sensor data generator
def generate_sensor_stream(count=100):
    """Generate random sensor readings"""
    sensors = ["temp-001", "temp-002", "temp-003", "pres-001"]
    readings = []
    for i in range(count):
        sensor_id = random.choice(sensors)
        # Normal values with 5% anomaly chance
        if random.random() < 0.05:
            value = random.uniform(45, 60) if "temp" in sensor_id else random.uniform(1150, 1200)
        else:
            value = random.uniform(18, 26) if "temp" in sensor_id else random.uniform(1000, 1030)
        
        readings.append({
            "id": sensor_id,
            "timestamp": i * 100,
            "value": value,
            "type": "temperature" if "temp" in sensor_id else "pressure"
        })
    return readings

# Pipeline implementation
def process_stream_v4(sensor_stream):
    """Stream-based processing"""
    # Enrich
    enriched = stream_map(
        lambda r: {**r, "location": "Building A", "processed": True},
        sensor_stream
    )
    
    # Filter anomalies
    anomalies = stream_filter(
        lambda r: (r["value"] > 40 if r["type"] == "temperature" else r["value"] > 1100),
        enriched
    )
    
    # Log (map with side effect)
    logged = stream_map(
        lambda r: (print(f"ANOMALY: {r['id']} = {r['value']:.1f}"), r)[1],
        anomalies
    )
    
    return logged

# Run test
print("=== Testing Stream Processing ===\n")

# Generate test data
test_data = generate_sensor_stream(50)
print(f"Generated {len(test_data)} sensor readings")

# Process with streaming
print("\nProcessing with streams...")
sensor_stream = stream_from_list(test_data)
result_stream = process_stream_v4(sensor_stream)
results = stream_collect(result_stream)

print(f"\nProcessed {len(test_data)} readings")
print(f"Found {len(results)} anomalies")
print(f"Detection rate: {len(results)/len(test_data)*100:.1f}%")

# Verify streaming maintains order
if results:
    print("\nFirst anomaly:", results[0]['id'], "at", results[0]['timestamp'])
    print("Last anomaly:", results[-1]['id'], "at", results[-1]['timestamp'])
    assert all(results[i]['timestamp'] <= results[i+1]['timestamp'] 
               for i in range(len(results)-1)), "Stream maintains order"
    print("✓ Stream processing maintains temporal order")

print("\n✓ Stream-based processing works correctly!")