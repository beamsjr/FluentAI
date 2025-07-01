"""
LSP Server capabilities for ClaudeLang
"""

def get_server_capabilities():
    """Return server capabilities for LSP"""
    return {
        'textDocumentSync': {
            'openClose': True,
            'change': 1,  # Full document sync
            'save': {
                'includeText': True
            }
        },
        'completionProvider': {
            'triggerCharacters': ['(', ' ', ':'],
            'resolveProvider': False
        },
        'hoverProvider': True,
        'definitionProvider': True,
        'referencesProvider': True,
        'documentSymbolProvider': True,
        'workspaceSymbolProvider': False,
        'codeActionProvider': False,
        'codeLensProvider': False,
        'documentFormattingProvider': True,
        'documentRangeFormattingProvider': False,
        'documentOnTypeFormattingProvider': False,
        'renameProvider': False,
        'documentLinkProvider': False,
        'colorProvider': False,
        'foldingRangeProvider': False,
        'declarationProvider': False,
        'implementationProvider': False,
        'typeDefinitionProvider': False,
        'callHierarchyProvider': False,
        'selectionRangeProvider': False,
        'semanticTokensProvider': False,
        'moniker': False,
        'linkedEditingRangeProvider': False,
        'inlayHintProvider': False,
        'inlineValueProvider': False,
        'diagnosticProvider': {
            'interFileDependencies': False,
            'workspaceDiagnostics': False
        }
    }