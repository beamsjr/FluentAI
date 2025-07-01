# Effect system for ClaudeLang

from .handlers import (
    EffectHandler, EffectContext, EffectRequest, EffectResult,
    IOHandler, StateHandler, ErrorHandler, TimeHandler, RandomHandler, NetworkHandler,
    create_default_handler, create_test_handler
)
from .primitives import register_effect_primitives, set_effect_context, get_effect_context

# Register all effect primitives when module is imported
register_effect_primitives()

__all__ = [
    'EffectHandler', 'EffectContext', 'EffectRequest', 'EffectResult',
    'IOHandler', 'StateHandler', 'ErrorHandler', 'TimeHandler', 'RandomHandler', 'NetworkHandler',
    'create_default_handler', 'create_test_handler',
    'register_effect_primitives', 'set_effect_context', 'get_effect_context'
]