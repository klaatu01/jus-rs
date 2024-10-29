use jus_rs::parser::ParseError;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _: tower_lsp::lsp_types::InitializeParams,
    ) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "JUS LSP initialized!")
            .await;
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        self.client
            .log_message(MessageType::INFO, "JUS LSP shutting down!")
            .await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.process_document(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().next() {
            self.process_document(params.text_document.uri, change.text)
                .await;
        }
    }
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self) -> Diagnostic;
}

impl IntoDiagnostic for ParseError {
    fn into_diagnostic(self) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position {
                    line: (self.line - 1) as u32,
                    character: (self.column - 1) as u32,
                },
                end: Position {
                    line: (self.line - 1) as u32,
                    character: (self.column + self.width - 1) as u32,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: self.to_string(),
            ..Default::default()
        }
    }
}

impl Backend {
    async fn process_document(&self, uri: Url, text: String) {
        let parser = jus_rs::parser::parse_ast(&text);
        self.client
            .log_message(MessageType::INFO, "JUS LSP: Parsing document...")
            .await;
        match parser {
            Ok(_) => {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
                self.client
                    .log_message(MessageType::INFO, "JUS LSP: Document parsed successfully!")
                    .await;
            }
            Err(err) => {
                let diagnostics = err.into_iter().map(|e| e.into_diagnostic()).collect();
                self.client
                    .publish_diagnostics(uri.clone(), diagnostics, None)
                    .await;
            }
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
