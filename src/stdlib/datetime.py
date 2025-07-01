"""
ClaudeLang Standard Library - Date and Time Functions

Date and time manipulation functions for ClaudeLang.
"""

from datetime import datetime, timedelta, timezone
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_datetime_functions():
    """Register date and time functions"""
    
    # Current time operations (these use TIME effect)
    PRIMITIVES.register(
        "datetime:now",
        Function(
            name="datetime:now",
            arity=0,
            effects={EffectType.TIME},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.TIME}
            )
        ),
        lambda: datetime.now()
    )
    
    PRIMITIVES.register(
        "datetime:utcnow",
        Function(
            name="datetime:utcnow",
            arity=0,
            effects={EffectType.TIME},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.TIME}
            )
        ),
        lambda: datetime.now(timezone.utc)
    )
    
    PRIMITIVES.register(
        "datetime:today",
        Function(
            name="datetime:today",
            arity=0,
            effects={EffectType.TIME},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Date")
                ],
                effects={EffectType.TIME}
            )
        ),
        lambda: datetime.today().date()
    )
    
    # Date/time creation (pure)
    PRIMITIVES.register(
        "datetime:create",
        Function(
            name="datetime:create",
            arity=6,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),  # year
                    TypeAnnotation("Int"),  # month
                    TypeAnnotation("Int"),  # day
                    TypeAnnotation("Int"),  # hour
                    TypeAnnotation("Int"),  # minute
                    TypeAnnotation("Int"),  # second
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda year, month, day, hour, minute, second: datetime(year, month, day, hour, minute, second)
    )
    
    PRIMITIVES.register(
        "date:create",
        Function(
            name="date:create",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),  # year
                    TypeAnnotation("Int"),  # month
                    TypeAnnotation("Int"),  # day
                    TypeAnnotation("Date")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda year, month, day: datetime(year, month, day).date()
    )
    
    # Parsing
    PRIMITIVES.register(
        "datetime:parse",
        Function(
            name="datetime:parse",
            arity=2,
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),  # format
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda date_str, fmt: _safe_datetime_parse(date_str, fmt)
    )
    
    PRIMITIVES.register(
        "datetime:parse-iso",
        Function(
            name="datetime:parse-iso",
            arity=1,
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda date_str: _safe_iso_parse(date_str)
    )
    
    # Formatting
    PRIMITIVES.register(
        "datetime:format",
        Function(
            name="datetime:format",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("String"),  # format
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt, fmt: dt.strftime(fmt) if isinstance(dt, datetime) else str(dt)
    )
    
    PRIMITIVES.register(
        "datetime:iso-format",
        Function(
            name="datetime:iso-format",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.isoformat() if isinstance(dt, datetime) else str(dt)
    )
    
    # Components extraction
    PRIMITIVES.register(
        "datetime:year",
        Function(
            name="datetime:year",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.year if hasattr(dt, 'year') else 0
    )
    
    PRIMITIVES.register(
        "datetime:month",
        Function(
            name="datetime:month",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.month if hasattr(dt, 'month') else 0
    )
    
    PRIMITIVES.register(
        "datetime:day",
        Function(
            name="datetime:day",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.day if hasattr(dt, 'day') else 0
    )
    
    PRIMITIVES.register(
        "datetime:hour",
        Function(
            name="datetime:hour",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.hour if hasattr(dt, 'hour') else 0
    )
    
    PRIMITIVES.register(
        "datetime:minute",
        Function(
            name="datetime:minute",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.minute if hasattr(dt, 'minute') else 0
    )
    
    PRIMITIVES.register(
        "datetime:second",
        Function(
            name="datetime:second",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.second if hasattr(dt, 'second') else 0
    )
    
    PRIMITIVES.register(
        "datetime:weekday",
        Function(
            name="datetime:weekday",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")  # 0=Monday, 6=Sunday
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.weekday() if hasattr(dt, 'weekday') else 0
    )
    
    # Date arithmetic
    PRIMITIVES.register(
        "datetime:add-days",
        Function(
            name="datetime:add-days",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt, days: dt + timedelta(days=days) if isinstance(dt, datetime) else dt
    )
    
    PRIMITIVES.register(
        "datetime:add-hours",
        Function(
            name="datetime:add-hours",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt, hours: dt + timedelta(hours=hours) if isinstance(dt, datetime) else dt
    )
    
    PRIMITIVES.register(
        "datetime:add-minutes",
        Function(
            name="datetime:add-minutes",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt, minutes: dt + timedelta(minutes=minutes) if isinstance(dt, datetime) else dt
    )
    
    PRIMITIVES.register(
        "datetime:add-seconds",
        Function(
            name="datetime:add-seconds",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt, seconds: dt + timedelta(seconds=seconds) if isinstance(dt, datetime) else dt
    )
    
    # Date comparison
    PRIMITIVES.register(
        "datetime:before?",
        Function(
            name="datetime:before?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt1, dt2: dt1 < dt2 if isinstance(dt1, datetime) and isinstance(dt2, datetime) else False
    )
    
    PRIMITIVES.register(
        "datetime:after?",
        Function(
            name="datetime:after?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt1, dt2: dt1 > dt2 if isinstance(dt1, datetime) and isinstance(dt2, datetime) else False
    )
    
    PRIMITIVES.register(
        "datetime:equal?",
        Function(
            name="datetime:equal?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt1, dt2: dt1 == dt2 if isinstance(dt1, datetime) and isinstance(dt2, datetime) else False
    )
    
    # Duration/difference
    PRIMITIVES.register(
        "datetime:diff-seconds",
        Function(
            name="datetime:diff-seconds",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt1, dt2: (dt1 - dt2).total_seconds() if isinstance(dt1, datetime) and isinstance(dt2, datetime) else 0.0
    )
    
    PRIMITIVES.register(
        "datetime:diff-days",
        Function(
            name="datetime:diff-days",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt1, dt2: (dt1 - dt2).days if isinstance(dt1, datetime) and isinstance(dt2, datetime) else 0
    )
    
    # Unix timestamp conversion
    PRIMITIVES.register(
        "datetime:to-timestamp",
        Function(
            name="datetime:to-timestamp",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("DateTime"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda dt: dt.timestamp() if isinstance(dt, datetime) else 0.0
    )
    
    PRIMITIVES.register(
        "datetime:from-timestamp",
        Function(
            name="datetime:from-timestamp",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float"),
                    TypeAnnotation("DateTime")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda ts: datetime.fromtimestamp(ts)
    )


def _safe_datetime_parse(date_str, fmt):
    """Safely parse datetime with format"""
    try:
        return datetime.strptime(date_str, fmt)
    except Exception as e:
        return {"error": f"Failed to parse datetime: {str(e)}"}


def _safe_iso_parse(date_str):
    """Safely parse ISO format datetime"""
    try:
        return datetime.fromisoformat(date_str)
    except Exception as e:
        return {"error": f"Failed to parse ISO datetime: {str(e)}"}


# Initialize datetime functions when module is imported
register_datetime_functions()