import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    // Get the LSP server path from configuration
    const config = vscode.workspace.getConfiguration('claudelang');
    let serverPath = config.get<string>('lspServerPath', 'claudelang-lsp');
    
    // If it's a relative path, resolve it relative to the workspace
    if (!path.isAbsolute(serverPath)) {
        const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
        if (workspaceRoot) {
            serverPath = path.join(workspaceRoot, 'tools', serverPath);
        }
    }
    
    // Server options
    const serverOptions: ServerOptions = {
        command: serverPath,
        args: [],
        options: {
            cwd: vscode.workspace.workspaceFolders?.[0]?.uri.fsPath
        }
    };
    
    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'claudelang' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.cl')
        }
    };
    
    // Create the language client
    client = new LanguageClient(
        'claudelang',
        'ClaudeLang Language Server',
        serverOptions,
        clientOptions
    );
    
    // Start the client
    client.start();
    
    // Register commands
    const formatCommand = vscode.commands.registerCommand('claudelang.format', () => {
        const editor = vscode.window.activeTextEditor;
        if (editor) {
            vscode.commands.executeCommand('editor.action.formatDocument');
        }
    });
    
    context.subscriptions.push(formatCommand);
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}