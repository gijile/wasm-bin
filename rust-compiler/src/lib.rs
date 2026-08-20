// Removed unused OS-dependent imports for bare-metal target compatibility


/// Simple AST
#[derive(Debug)]
enum Expr {
    Number(i32),
    Variable(String),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
struct Param {
    name: String,
}

#[derive(Debug)]
struct Function {
    name: String,
    params: Vec<Param>,
    body: Expr,
}

/// Tokenizer
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Fn,
    Arrow,
    Ident(String),
    Number(i32),
    Plus,
    Minus,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Colon,
    Comma,
    ArrowType,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
            continue;
        }

        if c.is_digit(10) {
            let mut num_str = String::new();
            while let Some(&nc) = chars.peek() {
                if nc.is_digit(10) {
                    num_str.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            tokens.push(Token::Number(num_str.parse().unwrap()));
            continue;
        }

        if c.is_alphabetic() || c == '_' {
            let mut ident = String::new();
            while let Some(&nc) = chars.peek() {
                if nc.is_alphanumeric() || nc == '_' {
                    ident.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            if ident == "fn" {
                tokens.push(Token::Fn);
            } else {
                tokens.push(Token::Ident(ident));
            }
            continue;
        }

        match c {
            '(' => { tokens.push(Token::LParen); chars.next(); }
            ')' => { tokens.push(Token::RParen); chars.next(); }
            '{' => { tokens.push(Token::LBrace); chars.next(); }
            '}' => { tokens.push(Token::RBrace); chars.next(); }
            ':' => { tokens.push(Token::Colon); chars.next(); }
            ',' => { tokens.push(Token::Comma); chars.next(); }
            '+' => { tokens.push(Token::Plus); chars.next(); }
            '-' => {
                chars.next();
                if let Some(&'>') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Minus);
                }
            }
            _ => { chars.next(); } // skip unknown characters
        }
    }
    tokens
}

/// Simple Parser
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let t = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(t)
        } else {
            None
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), String> {
        match self.next() {
            Some(t) if t == token => Ok(()),
            other => Err(format!("Expected {:?}, got {:?}", token, other)),
        }
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        self.expect(Token::Fn)?;

        let name = match self.next() {
            Some(Token::Ident(name)) => name,
            other => return Err(format!("Expected function name, got {:?}", other)),
        };

        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        while let Some(t) = self.peek() {
            if *t == Token::RParen {
                break;
            }
            let p_name = match self.next() {
                Some(Token::Ident(name)) => name,
                other => return Err(format!("Expected parameter name, got {:?}", other)),
            };
            self.expect(Token::Colon)?;
            // Expect type (e.g. i32)
            self.expect(Token::Ident("i32".to_string()))?;
            params.push(Param { name: p_name });

            if let Some(Token::Comma) = self.peek() {
                self.next();
            }
        }
        self.expect(Token::RParen)?;

        // Return type is optional or expected `-> i32`
        if let Some(Token::Arrow) = self.peek() {
            self.next();
            self.expect(Token::Ident("i32".to_string()))?;
        }

        self.expect(Token::LBrace)?;
        let body = self.parse_expr()?;
        self.expect(Token::RBrace)?;

        Ok(Function { name, params, body })
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let mut term = self.parse_primary()?;
        while let Some(t) = self.peek() {
            match t {
                Token::Plus => {
                    self.next();
                    let right = self.parse_primary()?;
                    term = Expr::Add(Box::new(term), Box::new(right));
                }
                Token::Minus => {
                    self.next();
                    let right = self.parse_primary()?;
                    term = Expr::Sub(Box::new(term), Box::new(right));
                }
                _ => break,
            }
        }
        Ok(term)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.next() {
            Some(Token::Number(val)) => Ok(Expr::Number(val)),
            Some(Token::Ident(name)) => Ok(Expr::Variable(name)),
            Some(Token::LParen) => {
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            other => Err(format!("Expected primary expression, got {:?}", other)),
        }
    }
}

/// WebAssembly Binary Encoder Helper
fn encode_leb128(mut value: u32) -> Vec<u8> {
    let mut bytes = Vec::new();
    loop {
        let mut byte = (value & 0x7F) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        bytes.push(byte);
        if value == 0 {
            break;
        }
    }
    bytes
}

fn encode_sleb128(mut value: i32) -> Vec<u8> {
    let mut bytes = Vec::new();
    loop {
        let mut byte = (value & 0x7F) as u8;
        value >>= 7;
        let more = !((value == 0 && (byte & 0x40) == 0) || (value == -1 && (byte & 0x40) != 0));
        if more {
            byte |= 0x80;
        }
        bytes.push(byte);
        if !more {
            break;
        }
    }
    bytes
}

fn compile_expr(expr: &Expr, params: &[Param], code: &mut Vec<u8>) -> Result<(), String> {
    match expr {
        Expr::Number(val) => {
            code.push(0x41); // i32.const
            code.extend(encode_sleb128(*val));
        }
        Expr::Variable(name) => {
            let idx = params.iter().position(|p| p.name == *name)
                .ok_or_else(|| format!("Undefined variable: {}", name))?;
            code.push(0x20); // local.get
            code.extend(encode_leb128(idx as u32));
        }
        Expr::Add(left, right) => {
            compile_expr(left, params, code)?;
            compile_expr(right, params, code)?;
            code.push(0x6A); // i32.add
        }
        Expr::Sub(left, right) => {
            compile_expr(left, params, code)?;
            compile_expr(right, params, code)?;
            code.push(0x6B); // i32.sub
        }
    }
    Ok(())
}

fn generate_wasm_bytes(func: &Function) -> Result<Vec<u8>, String> {
    // 1. WASM Header
    let mut wasm = vec![
        0x00, 0x61, 0x73, 0x6d, // Magic number: "\0asm"
        0x01, 0x00, 0x00, 0x00, // Version: 1
    ];

    // 2. Type Section (ID = 1)
    let mut type_payload = Vec::new();
    type_payload.push(1); // One signature definition
    type_payload.push(0x60); // Type descriptor for function (form = 0x60)
    
    type_payload.extend(encode_leb128(func.params.len() as u32));
    for _ in 0..func.params.len() {
        type_payload.push(0x7F); // i32
    }
    
    type_payload.push(1);
    type_payload.push(0x7F); // i32

    wasm.push(1); // Type Section ID
    wasm.extend(encode_leb128(type_payload.len() as u32));
    wasm.extend(type_payload);

    // 3. Function Section (ID = 3)
    let mut func_payload = Vec::new();
    func_payload.push(1); // One function index
    func_payload.push(0); // References Type Signature Index 0

    wasm.push(3); // Function Section ID
    wasm.extend(encode_leb128(func_payload.len() as u32));
    wasm.extend(func_payload);

    // 4. Export Section (ID = 7)
    let mut export_payload = Vec::new();
    export_payload.push(1); // Number of exports
    
    let name_bytes = func.name.as_bytes();
    export_payload.extend(encode_leb128(name_bytes.len() as u32));
    export_payload.extend(name_bytes);
    export_payload.push(0x00); // Export kind = Function
    export_payload.push(0x00); // Function Index = 0

    wasm.push(7); // Export Section ID
    wasm.extend(encode_leb128(export_payload.len() as u32));
    wasm.extend(export_payload);

    // 5. Code Section (ID = 10)
    let mut func_body = Vec::new();
    func_body.push(0); // Number of local variable declarations
    
    compile_expr(&func.body, &func.params, &mut func_body)?;
    func_body.push(0x0B); // end instruction

    let mut code_payload = Vec::new();
    code_payload.push(1); // One function body
    code_payload.extend(encode_leb128(func_body.len() as u32));
    code_payload.extend(func_body);

    wasm.push(10); // Code Section ID
    wasm.extend(encode_leb128(code_payload.len() as u32));
    wasm.extend(code_payload);

    Ok(wasm)
}

#[no_mangle]
pub extern "C" fn alloc(size: usize) -> *mut u8 {
    let mut buf = Vec::with_capacity(size);
    let ptr = buf.as_mut_ptr();
    std::mem::forget(buf);
    ptr
}

#[no_mangle]
pub extern "C" fn dealloc(ptr: *mut u8, size: usize) {
    unsafe {
        let _ = Vec::from_raw_parts(ptr, 0, size);
    }
}

#[no_mangle]
pub extern "C" fn compile_wasm(code_ptr: *const u8, code_len: usize) -> *mut u8 {
    let code_bytes = unsafe { std::slice::from_raw_parts(code_ptr, code_len) };
    let code_str = match std::str::from_utf8(code_bytes) {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let tokens = tokenize(code_str);
    let mut parser = Parser::new(tokens);
    let func = match parser.parse_function() {
        Ok(f) => f,
        Err(_) => return std::ptr::null_mut(),
    };

    let wasm_bytes = match generate_wasm_bytes(&func) {
        Ok(bytes) => bytes,
        Err(_) => return std::ptr::null_mut(),
    };

    // Return the bytes. Write the length in the first 4 bytes of output
    let mut output = Vec::with_capacity(4 + wasm_bytes.len());
    output.extend_from_slice(&(wasm_bytes.len() as u32).to_le_bytes());
    output.extend_from_slice(&wasm_bytes);
    
    let ptr = output.as_mut_ptr();
    std::mem::forget(output);
    ptr
}
