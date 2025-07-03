//! Date and time operations

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use claudelang_core::ast::EffectType;
use anyhow::{anyhow, Result};
use chrono::{DateTime, Local, Utc, NaiveDate, Datelike, Timelike};

/// Register all datetime functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Current time functions (effectful - Time effect)
        StdlibFunction::effectful(
            "datetime:now",
            datetime_now,
            0,
            Some(0),
            vec![EffectType::Time],
            "Get current local datetime"
        ),
        StdlibFunction::effectful(
            "datetime:utcnow",
            datetime_utcnow,
            0,
            Some(0),
            vec![EffectType::Time],
            "Get current UTC datetime"
        ),
        StdlibFunction::effectful(
            "datetime:today",
            datetime_today,
            0,
            Some(0),
            vec![EffectType::Time],
            "Get current date"
        ),
        
        // Creation functions (pure)
        StdlibFunction::pure("datetime:create", datetime_create, 3, Some(6), "Create datetime from components"),
        StdlibFunction::pure("date:create", date_create, 3, Some(3), "Create date from year, month, day"),
        
        // Parsing functions (pure)
        StdlibFunction::pure("datetime:parse", datetime_parse, 2, Some(2), "Parse datetime from string with format"),
        StdlibFunction::pure("datetime:parse-iso", datetime_parse_iso, 1, Some(1), "Parse ISO 8601 datetime"),
        
        // Formatting functions (pure)
        StdlibFunction::pure("datetime:format", datetime_format, 2, Some(2), "Format datetime to string"),
        StdlibFunction::pure("datetime:iso-format", datetime_iso_format, 1, Some(1), "Format datetime as ISO 8601"),
        
        // Unix timestamp functions
        StdlibFunction::effectful(
            "current-time",
            current_time,
            0,
            Some(0),
            vec![EffectType::Time],
            "Get current Unix timestamp in seconds"
        ),
        StdlibFunction::effectful(
            "current-millis",
            current_millis,
            0,
            Some(0),
            vec![EffectType::Time],
            "Get current Unix timestamp in milliseconds"
        ),
        StdlibFunction::pure("time->string", time_to_string, 1, Some(1), "Convert Unix timestamp to string"),
        StdlibFunction::pure("time-year", time_year, 1, Some(1), "Get year from Unix timestamp"),
        StdlibFunction::pure("time-month", time_month, 1, Some(1), "Get month from Unix timestamp"),
        StdlibFunction::pure("time-day", time_day, 1, Some(1), "Get day from Unix timestamp"),
        StdlibFunction::pure("time-hour", time_hour, 1, Some(1), "Get hour from Unix timestamp"),
        StdlibFunction::pure("time-minute", time_minute, 1, Some(1), "Get minute from Unix timestamp"),
        StdlibFunction::pure("time-second", time_second, 1, Some(1), "Get second from Unix timestamp"),
        StdlibFunction::pure("time-weekday", time_weekday, 1, Some(1), "Get weekday from Unix timestamp (0=Sunday)"),
        StdlibFunction::pure("make-time", make_time, 6, Some(6), "Create Unix timestamp from components"),
        StdlibFunction::pure("time-before?", time_before, 2, Some(2), "Check if first time is before second"),
        StdlibFunction::pure("time-after?", time_after, 2, Some(2), "Check if first time is after second"),
        StdlibFunction::pure("time-diff", time_diff, 2, Some(2), "Get difference between two times in seconds"),
        StdlibFunction::pure("add-seconds", add_seconds, 2, Some(2), "Add seconds to a Unix timestamp"),
        
        // Component extraction (pure)
        StdlibFunction::pure("datetime:year", datetime_year, 1, Some(1), "Get year from datetime"),
        StdlibFunction::pure("datetime:month", datetime_month, 1, Some(1), "Get month from datetime"),
        StdlibFunction::pure("datetime:day", datetime_day, 1, Some(1), "Get day from datetime"),
        StdlibFunction::pure("datetime:hour", datetime_hour, 1, Some(1), "Get hour from datetime"),
        StdlibFunction::pure("datetime:minute", datetime_minute, 1, Some(1), "Get minute from datetime"),
        StdlibFunction::pure("datetime:second", datetime_second, 1, Some(1), "Get second from datetime"),
        StdlibFunction::pure("datetime:weekday", datetime_weekday, 1, Some(1), "Get weekday (0=Sunday, 6=Saturday)"),
        
        // Arithmetic (pure)
        StdlibFunction::pure("datetime:add-days", datetime_add_days, 2, Some(2), "Add days to datetime"),
        StdlibFunction::pure("datetime:add-hours", datetime_add_hours, 2, Some(2), "Add hours to datetime"),
        StdlibFunction::pure("datetime:add-minutes", datetime_add_minutes, 2, Some(2), "Add minutes to datetime"),
        StdlibFunction::pure("datetime:add-seconds", datetime_add_seconds, 2, Some(2), "Add seconds to datetime"),
        
        // Comparison (pure)
        StdlibFunction::pure("datetime:before?", datetime_before, 2, Some(2), "Check if first datetime is before second"),
        StdlibFunction::pure("datetime:after?", datetime_after, 2, Some(2), "Check if first datetime is after second"),
        StdlibFunction::pure("datetime:equal?", datetime_equal, 2, Some(2), "Check if datetimes are equal"),
        
        // Differences (pure)
        StdlibFunction::pure("datetime:diff-seconds", datetime_diff_seconds, 2, Some(2), "Difference in seconds"),
        StdlibFunction::pure("datetime:diff-days", datetime_diff_days, 2, Some(2), "Difference in days"),
        
        // Unix timestamps (pure)
        StdlibFunction::pure("datetime:to-timestamp", datetime_to_timestamp, 1, Some(1), "Convert to Unix timestamp"),
        StdlibFunction::pure("datetime:from-timestamp", datetime_from_timestamp, 1, Some(1), "Create from Unix timestamp"),
    ]);
}

// Datetime representation as a Map with fields
fn create_datetime_value(dt: DateTime<Utc>) -> Value {
    let mut map = rustc_hash::FxHashMap::default();
    map.insert("year".to_string(), Value::Int(dt.year() as i64));
    map.insert("month".to_string(), Value::Int(dt.month() as i64));
    map.insert("day".to_string(), Value::Int(dt.day() as i64));
    map.insert("hour".to_string(), Value::Int(dt.hour() as i64));
    map.insert("minute".to_string(), Value::Int(dt.minute() as i64));
    map.insert("second".to_string(), Value::Int(dt.second() as i64));
    map.insert("timestamp".to_string(), Value::Int(dt.timestamp()));
    map.insert("_type".to_string(), Value::String("datetime".to_string()));
    Value::Map(map)
}

fn extract_datetime(value: &Value) -> Result<DateTime<Utc>> {
    match value {
        Value::Map(map) => {
            if let Some(Value::String(t)) = map.get("_type") {
                if t != "datetime" {
                    return Err(anyhow!("expected datetime object"));
                }
            } else {
                return Err(anyhow!("expected datetime object"));
            }
            
            let timestamp = match map.get("timestamp") {
                Some(Value::Int(ts)) => *ts,
                _ => return Err(anyhow!("invalid datetime object")),
            };
            
            Ok(DateTime::from_timestamp(timestamp, 0).unwrap())
        }
        _ => Err(anyhow!("expected datetime object")),
    }
}

// Current time functions

fn datetime_now(_args: &[Value]) -> Result<Value> {
    let now = Local::now();
    Ok(create_datetime_value(now.with_timezone(&Utc)))
}

fn datetime_utcnow(_args: &[Value]) -> Result<Value> {
    Ok(create_datetime_value(Utc::now()))
}

fn datetime_today(_args: &[Value]) -> Result<Value> {
    let today = Local::now().date_naive();
    let mut map = rustc_hash::FxHashMap::default();
    map.insert("year".to_string(), Value::Int(today.year() as i64));
    map.insert("month".to_string(), Value::Int(today.month() as i64));
    map.insert("day".to_string(), Value::Int(today.day() as i64));
    map.insert("_type".to_string(), Value::String("date".to_string()));
    Ok(Value::Map(map))
}

// Creation functions

fn datetime_create(args: &[Value]) -> Result<Value> {
    let year = match &args[0] {
        Value::Int(y) => *y as i32,
        _ => return Err(anyhow!("datetime:create: expected integer year")),
    };
    
    let month = match &args[1] {
        Value::Int(m) => *m as u32,
        _ => return Err(anyhow!("datetime:create: expected integer month")),
    };
    
    let day = match &args[2] {
        Value::Int(d) => *d as u32,
        _ => return Err(anyhow!("datetime:create: expected integer day")),
    };
    
    let hour = if args.len() > 3 {
        match &args[3] {
            Value::Int(h) => *h as u32,
            _ => return Err(anyhow!("datetime:create: expected integer hour")),
        }
    } else {
        0
    };
    
    let minute = if args.len() > 4 {
        match &args[4] {
            Value::Int(m) => *m as u32,
            _ => return Err(anyhow!("datetime:create: expected integer minute")),
        }
    } else {
        0
    };
    
    let second = if args.len() > 5 {
        match &args[5] {
            Value::Int(s) => *s as u32,
            _ => return Err(anyhow!("datetime:create: expected integer second")),
        }
    } else {
        0
    };
    
    let date = NaiveDate::from_ymd_opt(year, month, day)
        .ok_or_else(|| anyhow!("datetime:create: invalid date"))?;
    
    let datetime = date.and_hms_opt(hour, minute, second)
        .ok_or_else(|| anyhow!("datetime:create: invalid time"))?;
    
    Ok(create_datetime_value(DateTime::from_naive_utc_and_offset(datetime, Utc)))
}

fn date_create(args: &[Value]) -> Result<Value> {
    let year = match &args[0] {
        Value::Int(y) => *y as i32,
        _ => return Err(anyhow!("date:create: expected integer year")),
    };
    
    let month = match &args[1] {
        Value::Int(m) => *m as u32,
        _ => return Err(anyhow!("date:create: expected integer month")),
    };
    
    let day = match &args[2] {
        Value::Int(d) => *d as u32,
        _ => return Err(anyhow!("date:create: expected integer day")),
    };
    
    let _date = NaiveDate::from_ymd_opt(year, month, day)
        .ok_or_else(|| anyhow!("date:create: invalid date"))?;
    
    let mut map = rustc_hash::FxHashMap::default();
    map.insert("year".to_string(), Value::Int(year as i64));
    map.insert("month".to_string(), Value::Int(month as i64));
    map.insert("day".to_string(), Value::Int(day as i64));
    map.insert("_type".to_string(), Value::String("date".to_string()));
    Ok(Value::Map(map))
}

// Unix timestamp functions

fn current_time(_args: &[Value]) -> Result<Value> {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|e| anyhow!("Failed to get current time: {}", e))?;
    Ok(Value::Int(now.as_secs() as i64))
}

fn current_millis(_args: &[Value]) -> Result<Value> {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|e| anyhow!("Failed to get current time: {}", e))?;
    Ok(Value::Int(now.as_millis() as i64))
}

fn time_to_string(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time->string: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::String(dt.to_rfc3339()))
}

fn time_year(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-year: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::Int(dt.year() as i64))
}

fn time_month(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-month: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::Int(dt.month() as i64))
}

fn time_day(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-day: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::Int(dt.day() as i64))
}

fn time_hour(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-hour: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::Int(dt.hour() as i64))
}

fn time_minute(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-minute: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::Int(dt.minute() as i64))
}

fn time_second(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-second: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    Ok(Value::Int(dt.second() as i64))
}

fn time_weekday(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-weekday: expected integer timestamp")),
    };
    
    let dt = DateTime::<Utc>::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("Invalid timestamp"))?;
    
    // Convert chrono weekday (Mon=0) to standard (Sun=0)
    let weekday = dt.weekday().num_days_from_sunday();
    Ok(Value::Int(weekday as i64))
}

fn make_time(args: &[Value]) -> Result<Value> {
    let year = match &args[0] {
        Value::Int(y) => *y as i32,
        _ => return Err(anyhow!("make-time: expected integer year")),
    };
    
    let month = match &args[1] {
        Value::Int(m) => *m as u32,
        _ => return Err(anyhow!("make-time: expected integer month")),
    };
    
    let day = match &args[2] {
        Value::Int(d) => *d as u32,
        _ => return Err(anyhow!("make-time: expected integer day")),
    };
    
    let hour = match &args[3] {
        Value::Int(h) => *h as u32,
        _ => return Err(anyhow!("make-time: expected integer hour")),
    };
    
    let minute = match &args[4] {
        Value::Int(m) => *m as u32,
        _ => return Err(anyhow!("make-time: expected integer minute")),
    };
    
    let second = match &args[5] {
        Value::Int(s) => *s as u32,
        _ => return Err(anyhow!("make-time: expected integer second")),
    };
    
    // Validate ranges
    if month < 1 || month > 12 {
        return Err(anyhow!("make-time: invalid month {}", month));
    }
    if day < 1 || day > 31 {
        return Err(anyhow!("make-time: invalid day {}", day));
    }
    if hour > 23 {
        return Err(anyhow!("make-time: invalid hour {}", hour));
    }
    if minute > 59 {
        return Err(anyhow!("make-time: invalid minute {}", minute));
    }
    if second > 59 {
        return Err(anyhow!("make-time: invalid second {}", second));
    }
    
    let date = NaiveDate::from_ymd_opt(year, month, day)
        .ok_or_else(|| anyhow!("make-time: invalid date"))?;
    let time = date.and_hms_opt(hour, minute, second)
        .ok_or_else(|| anyhow!("make-time: invalid time"))?;
    let dt = DateTime::<Utc>::from_naive_utc_and_offset(time, Utc);
    
    Ok(Value::Int(dt.timestamp()))
}

fn time_before(args: &[Value]) -> Result<Value> {
    let t1 = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-before?: expected integer timestamp")),
    };
    
    let t2 = match &args[1] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-before?: expected integer timestamp")),
    };
    
    Ok(Value::Bool(t1 < t2))
}

fn time_after(args: &[Value]) -> Result<Value> {
    let t1 = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-after?: expected integer timestamp")),
    };
    
    let t2 = match &args[1] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-after?: expected integer timestamp")),
    };
    
    Ok(Value::Bool(t1 > t2))
}

fn time_diff(args: &[Value]) -> Result<Value> {
    let t1 = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-diff: expected integer timestamp")),
    };
    
    let t2 = match &args[1] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("time-diff: expected integer timestamp")),
    };
    
    Ok(Value::Int(t1 - t2))
}

fn add_seconds(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(t) => *t,
        _ => return Err(anyhow!("add-seconds: expected integer timestamp")),
    };
    
    let seconds = match &args[1] {
        Value::Int(s) => *s,
        _ => return Err(anyhow!("add-seconds: expected integer seconds")),
    };
    
    Ok(Value::Int(timestamp + seconds))
}

// Parsing functions

fn datetime_parse(_args: &[Value]) -> Result<Value> {
    // This would require a proper date parsing library
    Err(anyhow!("datetime:parse: not yet implemented"))
}

fn datetime_parse_iso(args: &[Value]) -> Result<Value> {
    let iso_str = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("datetime:parse-iso: expected string")),
    };
    
    let dt = DateTime::parse_from_rfc3339(iso_str)
        .map_err(|e| anyhow!("datetime:parse-iso: {}", e))?;
    
    Ok(create_datetime_value(dt.with_timezone(&Utc)))
}

// Formatting functions

fn datetime_format(_args: &[Value]) -> Result<Value> {
    // This would require format string parsing
    Err(anyhow!("datetime:format: not yet implemented"))
}

fn datetime_iso_format(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    Ok(Value::String(dt.to_rfc3339()))
}

// Component extraction

fn datetime_year(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(map) => {
            match map.get("year") {
                Some(Value::Int(y)) => Ok(Value::Int(*y)),
                _ => Err(anyhow!("datetime:year: invalid datetime object")),
            }
        }
        _ => Err(anyhow!("datetime:year: expected datetime object")),
    }
}

fn datetime_month(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(map) => {
            match map.get("month") {
                Some(Value::Int(m)) => Ok(Value::Int(*m)),
                _ => Err(anyhow!("datetime:month: invalid datetime object")),
            }
        }
        _ => Err(anyhow!("datetime:month: expected datetime object")),
    }
}

fn datetime_day(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(map) => {
            match map.get("day") {
                Some(Value::Int(d)) => Ok(Value::Int(*d)),
                _ => Err(anyhow!("datetime:day: invalid datetime object")),
            }
        }
        _ => Err(anyhow!("datetime:day: expected datetime object")),
    }
}

fn datetime_hour(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(map) => {
            match map.get("hour") {
                Some(Value::Int(h)) => Ok(Value::Int(*h)),
                _ => Err(anyhow!("datetime:hour: invalid datetime object")),
            }
        }
        _ => Err(anyhow!("datetime:hour: expected datetime object")),
    }
}

fn datetime_minute(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(map) => {
            match map.get("minute") {
                Some(Value::Int(m)) => Ok(Value::Int(*m)),
                _ => Err(anyhow!("datetime:minute: invalid datetime object")),
            }
        }
        _ => Err(anyhow!("datetime:minute: expected datetime object")),
    }
}

fn datetime_second(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Map(map) => {
            match map.get("second") {
                Some(Value::Int(s)) => Ok(Value::Int(*s)),
                _ => Err(anyhow!("datetime:second: invalid datetime object")),
            }
        }
        _ => Err(anyhow!("datetime:second: expected datetime object")),
    }
}

fn datetime_weekday(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    // Convert chrono weekday (Mon=0) to standard (Sun=0)
    let weekday = (dt.weekday().num_days_from_sunday()) as i64;
    Ok(Value::Int(weekday))
}

// Arithmetic

fn datetime_add_days(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    let days = match &args[1] {
        Value::Int(d) => *d,
        _ => return Err(anyhow!("datetime:add-days: expected integer days")),
    };
    
    let new_dt = dt + chrono::Duration::days(days);
    Ok(create_datetime_value(new_dt))
}

fn datetime_add_hours(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    let hours = match &args[1] {
        Value::Int(h) => *h,
        _ => return Err(anyhow!("datetime:add-hours: expected integer hours")),
    };
    
    let new_dt = dt + chrono::Duration::hours(hours);
    Ok(create_datetime_value(new_dt))
}

fn datetime_add_minutes(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    let minutes = match &args[1] {
        Value::Int(m) => *m,
        _ => return Err(anyhow!("datetime:add-minutes: expected integer minutes")),
    };
    
    let new_dt = dt + chrono::Duration::minutes(minutes);
    Ok(create_datetime_value(new_dt))
}

fn datetime_add_seconds(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    let seconds = match &args[1] {
        Value::Int(s) => *s,
        _ => return Err(anyhow!("datetime:add-seconds: expected integer seconds")),
    };
    
    let new_dt = dt + chrono::Duration::seconds(seconds);
    Ok(create_datetime_value(new_dt))
}

// Comparison

fn datetime_before(args: &[Value]) -> Result<Value> {
    let dt1 = extract_datetime(&args[0])?;
    let dt2 = extract_datetime(&args[1])?;
    Ok(Value::Bool(dt1 < dt2))
}

fn datetime_after(args: &[Value]) -> Result<Value> {
    let dt1 = extract_datetime(&args[0])?;
    let dt2 = extract_datetime(&args[1])?;
    Ok(Value::Bool(dt1 > dt2))
}

fn datetime_equal(args: &[Value]) -> Result<Value> {
    let dt1 = extract_datetime(&args[0])?;
    let dt2 = extract_datetime(&args[1])?;
    Ok(Value::Bool(dt1 == dt2))
}

// Differences

fn datetime_diff_seconds(args: &[Value]) -> Result<Value> {
    let dt1 = extract_datetime(&args[0])?;
    let dt2 = extract_datetime(&args[1])?;
    let diff = dt2.signed_duration_since(dt1);
    Ok(Value::Int(diff.num_seconds()))
}

fn datetime_diff_days(args: &[Value]) -> Result<Value> {
    let dt1 = extract_datetime(&args[0])?;
    let dt2 = extract_datetime(&args[1])?;
    let diff = dt2.signed_duration_since(dt1);
    Ok(Value::Int(diff.num_days()))
}

// Unix timestamps

fn datetime_to_timestamp(args: &[Value]) -> Result<Value> {
    let dt = extract_datetime(&args[0])?;
    Ok(Value::Int(dt.timestamp()))
}

fn datetime_from_timestamp(args: &[Value]) -> Result<Value> {
    let timestamp = match &args[0] {
        Value::Int(ts) => *ts,
        _ => return Err(anyhow!("datetime:from-timestamp: expected integer timestamp")),
    };
    
    let dt = DateTime::from_timestamp(timestamp, 0)
        .ok_or_else(|| anyhow!("datetime:from-timestamp: invalid timestamp"))?;
    
    Ok(create_datetime_value(dt))
}