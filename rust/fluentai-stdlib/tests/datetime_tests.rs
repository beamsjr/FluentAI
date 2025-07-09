//! Tests for datetime stdlib functions

use fluentai_stdlib::init_stdlib;
use fluentai_stdlib::value::Value;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn test_current_time() {
    let stdlib = init_stdlib();
    let current_time = stdlib.get("current-time").unwrap();

    let before = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    let result = current_time.call(&[]).unwrap();

    let after = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs();

    match result {
        Value::Integer(timestamp) => {
            let ts = timestamp as u64;
            assert!(ts >= before && ts <= after);
        }
        _ => panic!("Expected integer timestamp"),
    }
}

#[test]
fn test_current_millis() {
    let stdlib = init_stdlib();
    let current_millis = stdlib.get("current-millis").unwrap();

    let before = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    let result = current_millis.call(&[]).unwrap();

    let after = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    match result {
        Value::Integer(millis) => {
            let ms = millis as u128;
            assert!(ms >= before && ms <= after);
        }
        _ => panic!("Expected integer milliseconds"),
    }
}

#[test]
fn test_time_to_string() {
    let stdlib = init_stdlib();
    let time_to_string = stdlib.get("time->string").unwrap();

    // Test with a known timestamp (2024-01-01 00:00:00 UTC)
    let timestamp = Value::Integer(1704067200);
    let result = time_to_string.call(&[timestamp]).unwrap();

    match result {
        Value::String(s) => {
            // The exact format depends on the system, but it should contain "2024"
            assert!(s.contains("2024"));
        }
        _ => panic!("Expected string"),
    }
}

#[test]
fn test_time_components() {
    let stdlib = init_stdlib();

    // Test with a known timestamp (2024-06-15 14:10:45 UTC)
    // 1718460645 seconds since Unix epoch
    let timestamp = Value::Integer(1718460645);

    // Test time-year
    let time_year = stdlib.get("time-year").unwrap();
    let year = time_year.call(&[timestamp.clone()]).unwrap();
    match year {
        Value::Integer(y) => assert_eq!(y, 2024),
        _ => panic!("Expected integer year"),
    }

    // Test time-month
    let time_month = stdlib.get("time-month").unwrap();
    let month = time_month.call(&[timestamp.clone()]).unwrap();
    match month {
        Value::Integer(m) => assert_eq!(m, 6),
        _ => panic!("Expected integer month"),
    }

    // Test time-day
    let time_day = stdlib.get("time-day").unwrap();
    let day = time_day.call(&[timestamp.clone()]).unwrap();
    match day {
        Value::Integer(d) => assert_eq!(d, 15),
        _ => panic!("Expected integer day"),
    }

    // Test time-hour
    let time_hour = stdlib.get("time-hour").unwrap();
    let hour = time_hour.call(&[timestamp.clone()]).unwrap();
    match hour {
        Value::Integer(h) => assert_eq!(h, 14),
        _ => panic!("Expected integer hour"),
    }

    // Test time-minute
    let time_minute = stdlib.get("time-minute").unwrap();
    let minute = time_minute.call(&[timestamp.clone()]).unwrap();
    match minute {
        Value::Integer(m) => assert_eq!(m, 10),
        _ => panic!("Expected integer minute"),
    }

    // Test time-second
    let time_second = stdlib.get("time-second").unwrap();
    let second = time_second.call(&[timestamp.clone()]).unwrap();
    match second {
        Value::Integer(s) => assert_eq!(s, 45),
        _ => panic!("Expected integer second"),
    }
}

#[test]
fn test_make_time() {
    let stdlib = init_stdlib();
    let make_time = stdlib.get("make-time").unwrap();

    // Create a specific date/time: 2024-06-15 14:30:45
    let result = make_time
        .call(&[
            Value::Integer(2024), // year
            Value::Integer(6),    // month
            Value::Integer(15),   // day
            Value::Integer(14),   // hour
            Value::Integer(30),   // minute
            Value::Integer(45),   // second
        ])
        .unwrap();

    match result {
        Value::Integer(timestamp) => {
            // Verify the timestamp represents the correct date
            let time_year = stdlib.get("time-year").unwrap();
            let year = time_year.call(&[Value::Integer(timestamp)]).unwrap();
            assert_eq!(year, Value::Integer(2024));

            let time_month = stdlib.get("time-month").unwrap();
            let month = time_month.call(&[Value::Integer(timestamp)]).unwrap();
            assert_eq!(month, Value::Integer(6));

            let time_day = stdlib.get("time-day").unwrap();
            let day = time_day.call(&[Value::Integer(timestamp)]).unwrap();
            assert_eq!(day, Value::Integer(15));
        }
        _ => panic!("Expected integer timestamp"),
    }
}

#[test]
fn test_time_comparison() {
    let stdlib = init_stdlib();
    let time_before = stdlib.get("time-before?").unwrap();
    let time_after = stdlib.get("time-after?").unwrap();

    let t1 = Value::Integer(1704067200); // 2024-01-01 00:00:00
    let t2 = Value::Integer(1704153600); // 2024-01-02 00:00:00

    // Test time-before?
    assert_eq!(
        time_before.call(&[t1.clone(), t2.clone()]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        time_before.call(&[t2.clone(), t1.clone()]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        time_before.call(&[t1.clone(), t1.clone()]).unwrap(),
        Value::Boolean(false)
    );

    // Test time-after?
    assert_eq!(
        time_after.call(&[t2.clone(), t1.clone()]).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        time_after.call(&[t1.clone(), t2.clone()]).unwrap(),
        Value::Boolean(false)
    );
    assert_eq!(
        time_after.call(&[t1.clone(), t1.clone()]).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_time_difference() {
    let stdlib = init_stdlib();
    let time_diff = stdlib.get("time-diff").unwrap();

    let t1 = Value::Integer(1704067200); // 2024-01-01 00:00:00
    let t2 = Value::Integer(1704153600); // 2024-01-02 00:00:00

    // t2 - t1 should be 86400 seconds (1 day)
    let diff = time_diff.call(&[t2.clone(), t1.clone()]).unwrap();
    assert_eq!(diff, Value::Integer(86400));

    // t1 - t2 should be -86400 seconds
    let diff = time_diff.call(&[t1.clone(), t2.clone()]).unwrap();
    assert_eq!(diff, Value::Integer(-86400));

    // Same times should give 0
    let diff = time_diff.call(&[t1.clone(), t1.clone()]).unwrap();
    assert_eq!(diff, Value::Integer(0));
}

#[test]
fn test_add_seconds() {
    let stdlib = init_stdlib();
    let add_seconds = stdlib.get("add-seconds").unwrap();

    let base_time = Value::Integer(1704067200); // 2024-01-01 00:00:00

    // Add 1 hour (3600 seconds)
    let result = add_seconds
        .call(&[base_time.clone(), Value::Integer(3600)])
        .unwrap();
    match result {
        Value::Integer(new_time) => {
            assert_eq!(new_time, 1704070800); // 2024-01-01 01:00:00
        }
        _ => panic!("Expected integer timestamp"),
    }

    // Subtract 1 hour (negative seconds)
    let result = add_seconds
        .call(&[base_time.clone(), Value::Integer(-3600)])
        .unwrap();
    match result {
        Value::Integer(new_time) => {
            assert_eq!(new_time, 1704063600); // 2023-12-31 23:00:00
        }
        _ => panic!("Expected integer timestamp"),
    }
}

#[test]
fn test_weekday() {
    let stdlib = init_stdlib();
    let time_weekday = stdlib.get("time-weekday").unwrap();

    // Test known dates
    // 2024-01-01 is a Monday (1)
    let monday = Value::Integer(1704067200);
    let result = time_weekday.call(&[monday]).unwrap();
    assert_eq!(result, Value::Integer(1));

    // 2024-01-07 is a Sunday (0)
    let sunday = Value::Integer(1704585600);
    let result = time_weekday.call(&[sunday]).unwrap();
    assert_eq!(result, Value::Integer(0));
}

#[test]
fn test_invalid_date() {
    let stdlib = init_stdlib();
    let make_time = stdlib.get("make-time").unwrap();

    // Test invalid month
    assert!(make_time
        .call(&[
            Value::Integer(2024),
            Value::Integer(13), // Invalid month
            Value::Integer(1),
            Value::Integer(0),
            Value::Integer(0),
            Value::Integer(0),
        ])
        .is_err());

    // Test invalid day
    assert!(make_time
        .call(&[
            Value::Integer(2024),
            Value::Integer(2),
            Value::Integer(30), // February doesn't have 30 days
            Value::Integer(0),
            Value::Integer(0),
            Value::Integer(0),
        ])
        .is_err());

    // Test invalid hour
    assert!(make_time
        .call(&[
            Value::Integer(2024),
            Value::Integer(1),
            Value::Integer(1),
            Value::Integer(25), // Invalid hour
            Value::Integer(0),
            Value::Integer(0),
        ])
        .is_err());
}
