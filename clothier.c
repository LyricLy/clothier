/*                    Copyright 2021 LyricLy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#define PCRE2_CODE_UNIT_WIDTH 8

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <pcre2.h>
#include <string.h>
#include <libgen.h>
#include <locale.h>
#include <errno.h>

void *safe_malloc(size_t n) {
    void *p = malloc(n);
    if (p == NULL) {
        perror("Allocation error");
        exit(1);
    }
    return p;
}
#define malloc safe_malloc

bool warn = false;
bool recurse_warn = false;
bool repl = false;
typedef int idx_t;

#define DEF_RAW_VEC(T)                                                         \
    typedef struct _raw_vec##T {                                               \
        idx_t capacity;                                                        \
        T *data;                                                               \
    } _raw_vec##T;                                                             \
                                                                               \
    _raw_vec##T _init_raw_vec##T(idx_t capacity) {                             \
        _raw_vec##T v;                                                         \
        v.capacity = capacity;                                                 \
        v.data = malloc(capacity * sizeof (T));                                \
        return v;                                                              \
    }                                                                          \
                                                                               \
    void _destroy_raw_vec##T(_raw_vec##T *vec) {                               \
        free(vec->data);                                                       \
        /* don't free `vec`; it isn't malloc'd */                              \
    }                                                                          \
                                                                               \
    void _resize_raw_vec##T(_raw_vec##T *vec, idx_t new_capacity) {            \
        vec->capacity = new_capacity;                                          \
        vec->data = realloc(vec->data, new_capacity * sizeof (T));             \
    }                                                                          \
                                                                               \
    void _reserve_raw_vec##T(_raw_vec##T *vec, idx_t size) {                   \
        if (vec->capacity < size) {                                            \
            idx_t cap = vec->capacity ? vec->capacity*2 : 8;                   \
            while (cap < size) {                                               \
                cap *= 2;                                                      \
            }                                                                  \
            _resize_raw_vec##T(vec, cap);                                      \
        }                                                                      \
    }                                                                          \


#define RawVec(T) _raw_vec##T
#define raw_vec_init(T, c) _init_raw_vec##T(c)
#define raw_vec_destroy(T, v) _destroy_raw_vec##T(v)
#define raw_vec_resize(T, v, c) _resize_raw_vec##T(v, c)
#define raw_vec_reserve(T, v, c) _reserve_raw_vec##T(v, c)

#define DEF_VEC(T)                                                             \
    DEF_RAW_VEC(T)                                                             \
    typedef struct _vec##T {                                                   \
        RawVec(T) buf;                                                         \
        idx_t length;                                                          \
    } *_vector##T;                                                             \
                                                                               \
    _vector##T _init_vector##T(idx_t capacity) {                               \
        _vector##T v = malloc(sizeof *v);                                      \
        v->length = 0;                                                         \
        v->buf = raw_vec_init(T, capacity);                                    \
        return v;                                                              \
    }                                                                          \
                                                                               \
    void _destroy_vector##T(_vector##T vec) {                                  \
        raw_vec_destroy(T, &vec->buf);                                         \
        free(vec);                                                             \
    }                                                                          \
                                                                               \
    void _resize_vector##T(_vector##T vec, idx_t new_capacity) {               \
        raw_vec_resize(T, &vec->buf, new_capacity);                            \
        vec->length = (vec->length<new_capacity) ? vec->length : new_capacity; \
    }                                                                          \
                                                                               \
    T *_append_vector##T(_vector##T vec, T val) {                              \
        raw_vec_reserve(T, &vec->buf, vec->length+1);                          \
        vec->buf.data[vec->length] = val;                                      \
        return &vec->buf.data[vec->length++];                                  \
    }                                                                          \
                                                                               \
    void _extend_vector##T(_vector##T vec, T *ex, idx_t c) {                   \
        raw_vec_reserve(T, &vec->buf, vec->length + c);                        \
        memcpy(&vec->buf.data[vec->length], ex, c * sizeof (T));               \
        vec->length += c;                                                      \
    }                                                                          \
                                                                               \
    bool _eq_vector##T(_vector##T first, _vector##T second) {                  \
        if (first->length != second->length) return false;                     \
        return !strncmp((char *) first->buf.data,                              \
                        (char *) second->buf.data,                             \
                        first->length);                                        \
    }                                                                          \
                                                                               \
    _vector##T _copy_vector##T(_vector##T vec) {                               \
        _vector##T v = malloc(sizeof *v);                                      \
        v->length = vec->length;                                               \
        v->buf = raw_vec_init(T, vec->buf.capacity);                           \
        memcpy(v->buf.data, vec->buf.data, vec->length);                       \
        return v;                                                              \
    }                                                                          \

#define Vec(T) _vector##T
#define vec_init(T, c) _init_vector##T(c)
#define vec_destroy(T, v) _destroy_vector##T(v)
#define vec_resize(T, v, c) _resize_vector##T(v, c)
#define vec_append(T, v, val) _append_vector##T(v, val)
#define vec_extend(T, v, s, c) _extend_vector##T(v, s, c)
#define vec_eq(T, x, y) _eq_vector##T(x, y)
#define vec_copy(T, x) _copy_vector##T(x)

#define DEF_DEQUE(T)                                                           \
    DEF_RAW_VEC(T)                                                             \
    typedef struct _deq##T {                                                   \
        RawVec(T) buf;                                                         \
        /* first is the index to the first element, end is the index to the    \  yes I know these are commented out
           element after the last element. The difference between the indices  \
           is the length of the deque; when they are equal, it is empty. */    \
        idx_t first;                                                           \
        idx_t end;                                                             \
    } *_deque##T;                                                              \
                                                                               \
    _deque##T _init_deque##T() {                                               \
        _DEQUE##T v = malloc(sizeof *v);                                       \
        v->buf = raw_vec_init(T, 8);                                           \
        v->first = 0;                                                          \
        v->end = 0;                                                            \
    }                                                                          \
                                                                               \
    void _destroy_deque##T(_deque##T deq) {                                    \
        raw_vec_destroy(T, &vec->buf);                                         \
        free(vec);                                                             \
    }                                                                          \
                                                                               \
    idx_t _length_deque##T(_deque##T deq) {                                    \
        return (deq->end-deq->first) & (deq->buf.capacity-1);                  \
    }                                                                          \
                                                                               \
    void _grow_deque##T(_deque##T deq) {                                       \
        if (_length_deque##T(deq) + 1 == deq->buf.capacity) {                  \
            idx_t old_capacity = deq->buf.capacity;                            \
            idx_t new_capacity = old_capacity*2;                               \
            raw_vec_resize(T, &deq->buf, new_capacity);                        \
            if (deq->first <= deq->end) {                                      \
                /* all good; nop */                                            \
            } else if (deq->end < old_capacity - deq->first) {                 \
                /* the end should move */                                      \
                memcpy(&deq->buf.data[old_capacity],                           \
                       &deq->buf.data[0],                                      \
                       deq->end);                                              \
                deq->end += old_capacity;                                      \
            } else {                                                           \
                /* the start should move */                                    \
                idx_t new_first = new_capacity - (old_capacity - deq->first);  \
                memcpy(&deq->buf.data[new_first],                              \
                       &deq->buf.data[deq->first],                             \
                       old_capacity - deq->first);                             \
                deq->first = new_first;                                        \
            }                                                                  \
        }                                                                      \
    }                                                                          \
                                                                               \
    void _append_deque##T(_deque##T deq, T val) {                              \
        _grow_deque##T(deq);                                                   \
        vec->buf.data[deq->end] = val;                                         \
        return &vec->buf.data[deq->end = (deq->end+1) & (deq->capacity-1)];    \
    }                                                                          \
                                                                               \
    void _prepend_deque##T(_deque##T deq, T val) {                             \
        _grow_deque##T(deq);                                                   \
        vec->buf.data[deq->first-1] = val;                                     \
        return &vec->buf.data[deq->end = (deq->end-1) & (deq->capacity-1)];    \
    }                                                                          \
                                                                               \

#define Deque(T) _deque##T
#define deque_init(T) _init_deque##T()
#define deque_destroy(T, d) _destroy_deque##T(d)
#define deque_grow(T, d) _grow_deque##T(d)
#define deque_clear(T, d) _clear_deque##T(d)
#define deque_append(T, d, val) _append_deque##T(d, val)
#define deque_prepend(T, d, val) _prepend_deque##T(d, val)

typedef union {
    struct {
        bool prepend: 1;
        bool append: 1;
        bool global: 1;
    
        bool ascii: 1;
        bool ignorecase: 1;
        bool multiline: 1;
        bool dotall: 1;
        bool verbose: 1;
    };
    uint8_t v;
} Flags;

#define ADD_FLAGS (((Flags) { .prepend = true, .append = true }).v)
#define GLOBAL_FLAG (((Flags) { .global = true }).v)
#define REGEX_FLAGS (((Flags) { .ascii = true, .ignorecase = true, .multiline = true, .dotall = true, .verbose = true }).v)

typedef struct Token {
    enum TokenType {
        LPAREN,
        RPAREN,
        LBRACKET,
        RBRACKET,
        LBRACE,
        RBRACE,
        COMMA,
        PLUS,
        EQUALS,
        DEQUALS,
        IDENT,
        STRING,
        REGEX,
        FLAGS,
        INT,
        LINE_SEP,
        INVALID,
    } type;

    idx_t line;
    idx_t start_column;
    idx_t end_column;

    union {
        int val;
        char *string;
        Flags flags;
    };
} Token;
DEF_VEC(Token);

typedef char *str;
DEF_VEC(str)
DEF_VEC(char)

typedef struct LexState {
    FILE *program;
    char *filename;
    idx_t line;
    idx_t column;
    Vec(str) lines;
    Vec(char) current_line;
} LexState;

LexState *lex_new(FILE *program, char *filename) {
    LexState *s = malloc(sizeof *s);
    s->line = 0;
    s->column = -1;
    s->program = program;
    s->filename = filename;
    s->lines = vec_init(str, 0);
    s->current_line = vec_init(char, 0);
    return s;
}

void lex_destroy(LexState *s) {
    fclose(s->program);
    vec_destroy(str, s->lines);
    free(s);
}

char lex_peek(LexState *s) {
    int c = fgetc(s->program);
    ungetc(c, s->program);
    return c != EOF ? c : 0;
}

char lex_consume(LexState *s) {
    int _next = fgetc(s->program);
    char next = _next != EOF ? _next : 0;
    if (next == '\n' || next == '\0') {
        vec_append(char, s->current_line, '\n');  // these strings are newline-terminated
        char *line = s->current_line->buf.data;
        line = realloc(line, s->current_line->length);
        vec_append(str, s->lines, line);
        s->current_line = vec_init(char, 0);
        s->line++;
        s->column = -1;
    } else {
        vec_append(char, s->current_line, next);
        s->column++;
    }
    return next;
}

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define warning_string (isatty(fileno(stderr)) ? "\033[0;1;91mwarning\033[0m" : "warning")

Token build_token(LexState *s, enum TokenType type) {
    Token t;
    t.type = type;
    t.line = s->line;
    t.start_column = s->column;
    t.end_column = s->column;
    return t;
}

void push_token(LexState *s, Vec(Token) tokens, enum TokenType type) {
    vec_append(Token, tokens, build_token(s, type));
}

void read_until_ender(LexState *s, Vec(Token) tokens, enum TokenType type, char ender) {
    Token t = build_token(s, type);
    Vec(char) string = vec_init(char, 16);
    while (true) {
        char n = lex_peek(s);
        if (n == ender) {
            lex_consume(s);
            break;
        }
        if (n == '\n' || n == 0) {
            if (warn) {
                eprintf("%s:%d:%d: %s: expected %c (got EOL)\n", s->filename, s->line+1, s->column+1, warning_string, ender);
            }
            t.type = INVALID;
            break;
        }
        vec_append(char, string, lex_consume(s));
    }
    vec_append(char, string, 0);
    t.string = string->buf.data;
    free(string);
    t.end_column = s->column;
    vec_append(Token, tokens, t);
}

Vec(Token) lex_line(LexState *s) {
    int level = 0;

    Vec(Token) tokens = vec_init(Token, 0);
    while (true) {
        char c = lex_consume(s);
        if (!c) return tokens;
        switch (c) {
          case '(': level++; push_token(s, tokens, LPAREN); break;
          case ')': level--; push_token(s, tokens, RPAREN); break;
          case '[': level++; push_token(s, tokens, LBRACKET); break;
          case ']': level--; push_token(s, tokens, RBRACKET); break;
          case '\n':
              if (level <= 0) {
                  push_token(s, tokens, LINE_SEP);
                  return tokens;
              }
              break;
          case '{': push_token(s, tokens, LBRACE); break;
          case '}': push_token(s, tokens, RBRACE); break;
          case ',': push_token(s, tokens, COMMA); break;
          case '+': push_token(s, tokens, PLUS); break;
          case '=':
              if (lex_peek(s) == '=') {
                  Token t = build_token(s, DEQUALS);
                  lex_consume(s);
                  t.end_column++;
                  vec_append(Token, tokens, t);
              } else {
                  push_token(s, tokens, EQUALS);
              }
              break;
          case '"': case '\'': read_until_ender(s, tokens,  STRING, c); break;
          case '/': read_until_ender(s, tokens,  REGEX, '/'); break;
          case '-': {
              Token t = build_token(s, FLAGS);
              t.flags = (Flags) { .append = false, .prepend = false, .global = false };
              while (true) {
                  char n = lex_peek(s);
                  if (!((n >= 'a' && n <= 'z') || (n >= 'A' && n <= 'Z'))) break;
                  switch (n) {
                    case 'a': t.flags.append = true; break;
                    case 'p': t.flags.prepend = true; break;
                    case 'g': t.flags.global = true; break;

                    case 'A': t.flags.ascii = true; break;
                    case 'I': t.flags.ignorecase = true; break;
                    case 'M': t.flags.multiline = true; break;
                    case 'S': t.flags.dotall = true; break;
                    case 'X': t.flags.verbose = true; break;
                  }
                  lex_consume(s);
              }
              t.end_column = s->column;
              vec_append(Token, tokens, t);
              break;
          }
          case '0' ... '9': {
              Token t = build_token(s, INT);
              int num = c - '0';
              while (true) {
                  char next = lex_peek(s);
                  if (next < '0' || next > '9') break;
                  num = num * 10 + next - '0';
                  lex_consume(s);
              }
              t.val = num;
              t.end_column = s->column;
              vec_append(Token, tokens, t);
              break;
          }
          case ' ': case '\t': case '\r': break;
          case '#':
              while (true) {
                  char n = lex_peek(s);
                  if (n == '\n' || n == 0) break;
                  lex_consume(s);
              }
              break;
          default: {
              Token t = build_token(s, IDENT);
              Vec(char) string = vec_init(char, 16);
              vec_append(char, string, c);
              while (true) {
                  char n = lex_peek(s);
                  if (( n == '\n'
                     || n == ' '
                     || n == '\t'
                     || n == '\r'
                     || n == 0
                     || n == '('
                     || n == ')'
                     || n == '['
                     || n == ']'
                     || n == '{'
                     || n == '}'
                     || n == ','
                     || n == '+'
                     || n == '='
                     || n == '"'
                     || n == '\''
                     || n == '/'
                     || n == '-'
                     || n == '#'
                     )) {
                      break;
                  }
                  vec_append(char, string, lex_consume(s));
              }
              vec_append(char, string, 0);
              char *name = string->buf.data;
              free(string);
              t.string = name;
              t.end_column = s->column;
              vec_append(Token, tokens, t);
              break;
          }
        }
    }

    return tokens;
}


void print_tokens(Vec(Token) tokens) {
    printf("[");
    for (int i = 0; i < tokens->length; i++) {
        Token token = tokens->buf.data[i];
        switch (token.type) {
          case LPAREN: printf("LPAREN"); break;
          case RPAREN: printf("RPAREN"); break;
          case LBRACKET: printf("LBRACKET"); break;
          case RBRACKET: printf("RBRACKET"); break;
          case LBRACE: printf("LBRACE"); break;
          case RBRACE: printf("RBRACE"); break;
          case COMMA: printf("COMMA"); break;
          case PLUS: printf("PLUS"); break;
          case EQUALS: printf("EQUALS"); break;
          case DEQUALS: printf("DEQUALS"); break;
          case INT: printf("INT(%d)", token.val); break;
          case LINE_SEP: printf("_"); break;
          case IDENT: printf("IDENT(\"%s\")", token.string); break;
          case STRING: printf("STRING(\"%s\")", token.string); break;
          case REGEX: printf("REGEX(\"%s\")", token.string); break;
          case FLAGS: printf(
              "FLAGS(append = %s, prepend = %s, global = %s)",
              token.flags.append ? "true" : "false",
              token.flags.prepend ? "true" : "false",
              token.flags.global ? "true" : "false"
          ); break;
          case INVALID: printf("?"); break;
        }
        printf(",");
    }
    printf("]\n");
}

typedef struct _vecCommand *_vectorCommand;

typedef struct ColourChange {
    idx_t idx;
    bool reset;
    uint8_t colour;
} ColourChange;
DEF_VEC(ColourChange)

typedef struct TailorStr {
    Vec(char) data;
    //Vec(ColourChange) colours;
} TailorStr;
DEF_VEC(TailorStr)

TailorStr str_new() {
    TailorStr r;
    r.data = vec_init(char, 0);
    // TODO: make from_nt and from_size parse colour from the string they're passed
    //r.colours = vec_init(ColourChange, 0);
    return r;
}

TailorStr str_from_nt(char *s) {
    TailorStr r = str_new();
    vec_extend(char, r.data, s, strlen(s));
    return r;
}

TailorStr str_from_size(char *s, idx_t size) {
    TailorStr r = str_new();
    vec_extend(char, r.data, s, size);
    return r;
}

void str_destroy(TailorStr s) {
    vec_destroy(char, s.data);
}

typedef enum SymbolType {
    COND,
    FAB,
    PROC,
    TYPE,
} SymbolType;

typedef struct Symbol Symbol;

typedef struct CondExpr {
    enum { COND_REGEX, COND_NOT, COND_EQ, COND_LOGIC } type;

    union {
        struct {
            Symbol *fabric;
            Flags flags;
            pcre2_code *regex;
        } regex;
        struct {
            Symbol *condition;
        } not;
        struct {
            Symbol *fab1;
            Symbol *fab2;
        } eq;
        struct {
            enum CondOp { AND, OR, XOR } op;
            Symbol *cond1;
            Symbol *cond2;
        } logic;
    };
} CondExpr;

typedef struct _vecSymbol_p *_vectorSymbol_p;
typedef struct Symbol {
    char *name;
    enum { FREE, BOUND, CONST } defined;
    bool is_read;
    SymbolType type;
    Token defined_by;
    union {
        struct { bool cond_update; union { bool cond_value; CondExpr cond_expr; }; };
        TailorStr fab_data;
        struct { Vec(Symbol_p) proc_args; Vec(Command) proc_code; };
        Vec(TailorStr) type_strs;
    };
} Symbol;
typedef Symbol *Symbol_p;
DEF_VEC(Symbol)
DEF_VEC(Symbol_p)

typedef struct ParseState {
    LexState *lexer;
    Vec(Token) token_buf;
    idx_t token_idx;
    char *filename;
    bool last_was_sep;
    bool empty;

    Vec(Symbol) fabrics;
    Vec(Symbol) conditions;
    Vec(Symbol) procedures;
    Vec(Symbol) types;
} ParseState;

typedef struct ActiveParseState {
    ParseState *s;
    Token cmd;
    bool ignore;
} ActiveParseState;

ParseState *parse_new(LexState *lexer, char *filename) {
    ParseState *p = malloc(sizeof *p);
    p->lexer = lexer;
    p->token_buf = vec_init(Token, 0);
    p->token_idx = 0;
    p->filename = filename;
    p->last_was_sep = false;
    p->empty = false;

    p->fabrics = vec_init(Symbol, 128);
    p->conditions = vec_init(Symbol, 128);
    p->procedures = vec_init(Symbol, 128);
    p->types = vec_init(Symbol, 128);
    return p;
}

Token parse_peek(ParseState *p) {
    if (p->token_idx >= p->token_buf->length) {
        // lex another line
        vec_destroy(Token, p->token_buf);
        p->token_idx = 0;
        p->token_buf = lex_line(p->lexer);
        if (p->token_buf->length == 0) {
            // all done; just return a dummy
            p->empty = true;
            return (Token) { .type = LINE_SEP };
        }
    }
    return p->token_buf->buf.data[p->token_idx];
}

Token parse_consume(ParseState *p) {
    Token r = parse_peek(p);
    p->last_was_sep = r.type == LINE_SEP;
    p->token_idx++;
    return r;
}

void parse_destroy(ParseState *p) {
    // this neglects some strings which I'm assuming will be freed later; if there's a memory leak, check Token.string first, since this doesn't free them
    vec_destroy(Token, p->token_buf);
    lex_destroy(p->lexer);
    free(p->fabrics);
    free(p->conditions);
    free(p->procedures);
    free(p->types);
    free(p);
}

void show_token(ParseState *p, Token start, Token start_hl, Token end_hl) {
    // don't ask
    if (start.type == LINE_SEP) start.line--;
    if (start_hl.type == LINE_SEP) start_hl.line--;
    if (end_hl.type == LINE_SEP) end_hl.line--;

    bool tty = isatty(fileno(stderr));
    Vec(str) lines = p->lexer->lines;
    for (idx_t line = start.line; line < end_hl.line+1; line++) {
        eprintf("%4d | ", line+1);
        char *line_str = lines->buf.data[line];
        for (idx_t column = 0; line_str[column] != '\n'; column++) {
            if (tty && line == start_hl.line && column == start_hl.start_column) {
                eprintf("\033[0;1;91m");
            }
            fputc(line_str[column], stderr);
            if (tty && line == end_hl.line && (column == end_hl.end_column)) {
                eprintf("\033[0m");
            }
        }
        if (tty) eprintf("\033[0m");
        eprintf("\n");
    }

    if (start_hl.line == end_hl.line) {
        eprintf("     | ");
        int i = 0;
        for (; i < start_hl.start_column; i++) {
            if (lines->buf.data[start_hl.line][i] == '\t') {
                eprintf("%c", '\t');
            } else {
                eprintf("%c", ' ');
            }
        }
        for (; i <= end_hl.end_column; i++) eprintf("%c", '~');
        eprintf("\n");
    }
}

#define warning(P, S, SH, E, F, ...) do { \
    eprintf("%s:%d:%d: %s: " F "\n", P->filename, (SH).line+1, (SH).type==LINE_SEP ? 1 : (SH).start_column+1, warning_string, ##__VA_ARGS__); \
    show_token(P, S, SH, E); \
} while (0)

typedef struct TypeExpr {
    enum { LITERAL, VARIABLE } kind;
    union {
        Vec(TailorStr) values;
        Symbol *name;
    };
} TypeExpr;
DEF_VEC(TypeExpr)

typedef struct VariationAssoc {
    Symbol *source;
    Symbol *target;
} VariationAssoc;
DEF_VEC(VariationAssoc)

typedef struct Command {
    enum CommandType {
        Gather,
        Sell,
        Stop,
        End,
        Notch,
        SeeInt,
        SeeNotch,
        Hem,
        Cond,
        If,
        While,
        Bleach,
        DyeInt,
        DyeFabric,
        Embroider,
        CopyStraight,
        CopyRegex,
        TypeDec,
        Replace,
        Alter,
        Procedure,
        Call,
        Variation,
    } type;

    union {
        Symbol *sym;
        idx_t line;
        struct {
            Symbol *name;
            CondExpr expr;
            bool update;
        } cond;
        struct {
            Symbol *cond;
            Vec(Command) code;
        } block_cond;
        struct {
            Symbol *fab;
            int colour;
        } dye_int;
        struct {
            Symbol *fab;
            Symbol *colour;
        } dye_fabric;
        struct {
            Symbol *target;
            Flags flags;
            TailorStr source;
        } embroider;
        struct {
            Symbol *target;
            Symbol *source;
        } copy_straight;
        struct {
            Symbol *target;
            Flags flags;
            pcre2_code *regex;
            Symbol *source;
        } copy_regex;
        struct {
            Symbol *name;
            Vec(TypeExpr) contents;
        } type_dec;
        struct {
            Symbol *data;
            Symbol *type1;
            Symbol *type2;
            Flags flags;
            bool trivial;
        } replace;
        struct {
            Symbol *data;
            Flags flags;
            pcre2_code *regex;
            TailorStr replacement;
        } alter;
        struct {
            Symbol *name;
            Vec(Symbol_p) args;
            Vec(Command) code;
        } proc;
        struct {
            Symbol *name;
            Vec(Symbol_p) args;
        } call;
        struct {
            Vec(Command) code;
            Vec(VariationAssoc) assocs;
            bool all_const;
        } variation;
    };
} Command;
DEF_VEC(Command)

#define BASE_COST 4

int conversion_cost(unsigned char x, unsigned char y) {
    if (x == y) {
        return 0;
    }
    // check for case insensitive match
    if (x >= 'A' && x <= 'Z') {
        x += 'a' - 'A';
    }
    if (y >= 'A' && y <= 'Z') {
        y += 'a' - 'A';
    }
    if (x == y) {
        return BASE_COST / 2;
    }
    // okay, they're unequal. our program doesn't know anything about Unicode, but I'm assuming the data is UTF-8, so we do a silly trick here.
    // if one of the characters is outside the ASCII range, make conversion cost less to avoid overweighting the difference between two (multi-byte) Unicode codepoints.
    if (x > 0x7F || y > 0x7F) {
        return BASE_COST / 4;
    }
    // both characters are ASCII and unequal, so return the "normal" cost.
    return BASE_COST;
}

int string_distance(char *x, char *y) {
    // Calculate the Damerau-Levenshtein distance of two strings. Direct port of pseudocode from Wikipedia.

    int xl = strlen(x);
    int yl = strlen(y);

    int da[256] = {0};

    int d[xl+2][yl+2];
    #define index_d(X, Y) d[X+1][Y+1]

    int max_dist = xl + yl;
    index_d(-1, -1) = max_dist;
    for (int i = 0; i <= xl; i++) {
        index_d(i, -1) = max_dist;
        index_d(i, 0) = i;
    }
    for (int i = 0; i <= yl; i++) {
        index_d(-1, i) = max_dist;
        index_d(0, i) = i;
    }

    for (int i = 1; i <= xl; i++) {
        int db = 0;
        for (int j = 1; j <= yl; j++) {
            int k = da[(unsigned char) y[j-1]];
            int l = db;
            int cost = conversion_cost(x[i-1], y[j-1]);
            if (cost == 0) db = j;
            int best = index_d(i-1, j-1) + cost;
            #define try(X) if ((X) < best) best = (X)
            try(index_d(i, j-1) + BASE_COST);
            try(index_d(i-1, j) + BASE_COST);
            try(index_d(k-1, l-1) + ((i-k-1) + 1 + (j-l-1))*BASE_COST);
            index_d(i, j) = best;
        }
        da[(unsigned char) x[i-1]] = i;
    }

    return index_d(xl, yl);
}

void show_similar_name(ParseState *s, Vec(Symbol) group, char *name) {
    Symbol *best_name = NULL;
    int best_match = -1;
    for (idx_t i = 0; i < group->length; i++) {
        Symbol *s = &group->buf.data[i];
        int match = string_distance(name, s->name);
        // we want a *similar* name, not an equal one. if there's an equal name in here, it's not the one we want
        if (match && (best_match == -1 || match < best_match)) {
            best_name = s;
            best_match = match;
        }
    }
    if (best_match != -1 && best_match <= (int)strlen(name)/3 * BASE_COST) {
        eprintf("help: a symbol '%s' with a similar name exists, first defined here:\n", best_name->name);
        show_token(s, best_name->defined_by, best_name->defined_by, best_name->defined_by);
    }
}

Vec(Symbol) appropriate_group(ParseState *s, SymbolType type) {
    switch (type) {
        case COND: return s->conditions;
        case FAB: return s->fabrics;
        case PROC: return s->procedures;
        case TYPE: return s->types;
    }
}

bool get_symbol(ParseState *s, SymbolType type, Token token, Symbol **target, bool edits, bool reads) {
    Vec(Symbol) group = appropriate_group(s, type);

    for (idx_t i = 0; i < group->length; i++) {
        if (!strcmp(group->buf.data[i].name, token.string)) {
            *target = &group->buf.data[i];
            if (edits) (*target)->defined = FREE; 
            if (reads) (*target)->is_read = true;
            return true;
        }
    }
    Symbol n;
    n.name = token.string;
    n.defined = FREE;
    n.type = type;
    n.is_read = reads;
    n.defined_by = token;
    *target = vec_append(Symbol, group, n);
    return false;
}

Symbol *_get_symbol_get(ParseState *s, SymbolType type, Token ident, bool edits) {
    Symbol *p;
    if (!get_symbol(s, type, ident, &p, edits, true) && warn) {
        warning(s, ident, ident, ident, "symbol '%s' used before definition", ident.string);
        show_similar_name(s, appropriate_group(s, type), ident.string);
    }
    return p;
}
#define get_symbol_read(...) _get_symbol_get(__VA_ARGS__, false)
#define get_symbol_get(...) _get_symbol_get(__VA_ARGS__, true)

Symbol *_get_symbol_set(ParseState *s, SymbolType type, Token token, bool reads) {
    Symbol *p;
    get_symbol(s, type, token, &p, true, reads);
    return p;
}
#define get_symbol_set(...) _get_symbol_set(__VA_ARGS__, true)
#define get_symbol_assign(...) _get_symbol_set(__VA_ARGS__, false)

bool get_symbol_set_const(ParseState *s, SymbolType type, Token token, Symbol **p) {
    if (!get_symbol(s, type, token, p, true, false)) {
        (*p)->defined = CONST;
        return true;
    }
    return false;
}

Command build_command(enum CommandType type) {
    Command c;
    c.type = type;
    return c;
}
#define add_command(e) do *command = e; while (0)

bool _expect(ActiveParseState *as, enum TokenType type, char *expects_string, Token t) {
    if (t.type != type) {
        if (warn) {
            warning(as->s, as->cmd, t, t, "expected %s (command will be ignored)", expects_string);
        }
        as->ignore = true;
        return false;
    }
    return true;
}

#define expect(type, string, token) _expect(as, type, string, token)

void ignore_line_sep(ParseState *s) {
    while (parse_peek(s).type == LINE_SEP) parse_consume(s);
}

Vec(Command) parse(ParseState *s, Token *bloody_mary);

bool parse_block_cond(ActiveParseState *as, enum CommandType type, Command *command) {
    Token start, cond;
    if (parse_peek(as->s).type == LPAREN) parse_consume(as->s);
    if (expect(IDENT, "a condition", cond = parse_consume(as->s))) {
        if (parse_peek(as->s).type == RPAREN) parse_consume(as->s);
        ignore_line_sep(as->s);
        if (expect(LBRACE, "a block", start = parse_consume(as->s))) {
            Command c = build_command(type);
            c.block_cond.cond = get_symbol_read(as->s, COND, cond);
            c.block_cond.code = parse(as->s, &start);
            add_command(c);
            return true;
        }
    }
    return false;
}

void narrow_flags(ActiveParseState *as, Token t, uint8_t ok_flags) {
    if (warn && t.flags.v & ~ok_flags) {
        warning(as->s, as->cmd, t, t, "unsupported flags for this command will be ignored");
    }
}

bool parse_optional_flags(ActiveParseState *as, Flags *f, uint8_t ok_flags) {
    if (parse_peek(as->s).type == FLAGS) {
        Token t = parse_consume(as->s);
        narrow_flags(as, t, ok_flags);
        *f = t.flags;
        return true;
    } else {
        *f = (Flags) {};
        return false;
    }
}

bool parse_update(ParseState *s) {
    if (parse_peek(s).type == IDENT && !strcmp(parse_peek(s).string, "update")) {
        parse_consume(s);
        return true;
    }
    return false;
}

void add_cond_logic(ActiveParseState *as, Command *command, enum CondOp op, Token name, Token name1) {
    parse_consume(as->s);
    Token name2;
    if (expect(IDENT, "a condition", name2 = parse_consume(as->s))) {
        Command l = build_command(Cond);
        l.cond.name = get_symbol_set(as->s, COND, name);
        l.cond.expr.type = COND_LOGIC;
        l.cond.expr.logic.cond1 = get_symbol_read(as->s, COND, name1);
        l.cond.expr.logic.cond2 = get_symbol_read(as->s, COND, name2);
        l.cond.expr.logic.op = op;
        l.cond.update = parse_update(as->s);
        add_command(l);
    }
}

Token out_ender;
Vec(Token) parse_comma_sep(ActiveParseState *as, enum TokenType type, char *elem_string, enum TokenType ender, char *ender_string) {
    Vec(Token) tokens = vec_init(Token, 16);
    while (true) {
        if ((out_ender = parse_peek(as->s)).type == ender) break;
        Token elem = parse_consume(as->s);
        char v[32];
        snprintf(v, sizeof v, "%s or %s", elem_string, ender_string); 
        if (!expect(type, v, elem)) return NULL;
        vec_append(Token, tokens, elem);

        if ((out_ender = parse_peek(as->s)).type == ender) break;
        Token comma = parse_consume(as->s);
        snprintf(v, sizeof v, "',' or %s", ender_string); 
        if (!expect(COMMA, v, comma)) return NULL;
    }
    parse_consume(as->s);
    return tokens;
}

bool parse_type_expr(ActiveParseState *as, TypeExpr *te) {
    if (parse_peek(as->s).type == IDENT) {
        te->kind = VARIABLE;
        te->name = get_symbol_read(as->s, TYPE, parse_consume(as->s));
        return true;
    } else if (expect(LBRACKET, "a type or type literal", parse_consume(as->s))) {
        Vec(Token) strings = parse_comma_sep(as, STRING, "a string", RBRACKET, "']'");
        if (strings == NULL) return false;
        te->kind = LITERAL;
        te->values = vec_init(TailorStr, strings->length+1);
        for (int i = 0; i < strings->length; i++) {
            vec_append(TailorStr, te->values, str_from_nt(strings->buf.data[i].string));
        }
        vec_destroy(Token, strings);
        return true;
    }
    return false;
}

pcre2_code *compile_regex(ActiveParseState *as, Token t, Flags flags) {
    int error;
    PCRE2_SIZE error_offset;
    pcre2_code *p = pcre2_compile(
          (unsigned char *) t.string
        , PCRE2_ZERO_TERMINATED
        , PCRE2_ALT_BSUX
        | PCRE2_NEVER_BACKSLASH_C
        | (flags.ignorecase ? PCRE2_CASELESS : 0)
        | (flags.dotall ? PCRE2_DOTALL : 0)
        | (flags.verbose ? PCRE2_EXTENDED : 0)
        | (flags.multiline ? PCRE2_MULTILINE : 0)
        | (flags.ascii ? PCRE2_UTF | PCRE2_UCP : PCRE2_NEVER_UTF | PCRE2_NEVER_UCP)
        , &error
        , &error_offset
        , NULL
    );

    if (p == NULL) {
        if (warn) {
            unsigned char buffer[120];
            pcre2_get_error_message(error, buffer, 120);
            warning(as->s, as->cmd, t, t, "regex is invalid at position %zu: %s (command will be ignored)", error_offset, buffer);
        }
        as->ignore = true;
        return NULL;
    }

    int jit_error = pcre2_jit_compile(p, PCRE2_JIT_COMPLETE);
    if (warn && jit_error) {
        uint32_t jit_enabled;
        pcre2_config(PCRE2_CONFIG_JIT, &jit_enabled);
        if (jit_enabled) {
            unsigned char buffer[120];
            pcre2_get_error_message(jit_error, buffer, 120);
            warning(as->s, as->cmd, t, t, "JIT compilation failed: %s", buffer);
            eprintf("note: the command will not be ignored, but its performance may be affected\n");
        }
    }

    return p;
}

typedef struct ParseResult {
    enum { SUCCESS, EMPTY, HALT } verdict;
    Command command;
} ParseResult;

Vec(Command) parse_file(FILE *fp, char *filename, bool w, ParseState **s);

#define is_known(X) (repl || X->defined == CONST)

ParseResult parse_command(ParseState *s, Token *bloody_mary) {
    Command _command;
    Command *command = &_command;

    ActiveParseState _as;
    ActiveParseState *as = &_as;
    as->s = s;

    while (true) {
        if (s->empty) {
            if (bloody_mary != NULL) {
                warning(s, *bloody_mary, *bloody_mary, *bloody_mary, "this bracket is unmatched");
            }
            return (ParseResult) { .verdict = EMPTY };
        }
        as->cmd = parse_consume(s);
        as->ignore = false;

        if (as->cmd.type == LINE_SEP) {
            continue;
        } else if (bloody_mary != NULL && as->cmd.type == RBRACE) {
            parse_consume(s);
            return (ParseResult) { .verdict = HALT };
        } else if (!expect(IDENT, "a command name", as->cmd)) {
        } else if (!strcmp(as->cmd.string, "gather")) {
            Command c = build_command(Gather);
            Token t = as->cmd;
            t.string = "materials";
            c.sym = get_symbol_set(s, FAB, t);
            add_command(c);
        } else if (!strcmp(as->cmd.string, "sell")) {
            Command c = build_command(Sell);
            Token t;
            t.line = as->cmd.line;
            t.start_column = as->cmd.start_column;
            t.end_column = as->cmd.end_column;
            t.string = "garment";
            c.sym = get_symbol_read(s, FAB, t);
            add_command(c);
        } else if (!strcmp(as->cmd.string, "stop")) {
            add_command(build_command(Stop));
        } else if (!strcmp(as->cmd.string, "end")) {
            add_command(build_command(End));
        // TODO: implement notch and see
        } else if (!strcmp(as->cmd.string, "notch")) {
            Token name = parse_consume(s);
            if (expect(IDENT, "a name", name)) {
                // ...
            }
        } else if (!strcmp(as->cmd.string, "see")) {
            Token arg = parse_consume(s);
            if (arg.type == INT) {
            } else if (expect(IDENT, "an integer or notch", arg)) {
                // ...
            }
        } else if (!strcmp(as->cmd.string, "hem")) {
            Token fabric = parse_consume(s);
            if (expect(IDENT, "a fabric", fabric)) {
                Command b = build_command(Hem);
                b.sym = get_symbol_get(s, FAB, fabric);
                add_command(b);
            }                                                
        } else if (!strcmp(as->cmd.string, "condition")) {
            Token name, name1, name2, flags, regex;
            if (expect(IDENT, "a condition name", name = parse_consume(s))
             && expect(EQUALS, "'='", parse_consume(s))
             && expect(IDENT, "a fabric, a condition, or 'not'", name1 = parse_consume(s))
            ) {
                Token peek = parse_peek(s);
                if (!strcmp(name1.string, "not")) {
                    if (expect(IDENT, "a condition", name2 = parse_consume(s))) {
                        Command n = build_command(Cond);
                        n.cond.name = get_symbol_set(s, COND, name);
                        n.cond.expr.type = COND_NOT;
                        n.cond.expr.not.condition = get_symbol_read(s, COND, name2);
                        n.cond.update = parse_update(s);
                        add_command(n);
                    }
                } else if (peek.type == DEQUALS) {
                    parse_consume(s);
                    if (expect(IDENT, "a fabric", name2 = parse_consume(s))) {
                        Command e = build_command(Cond);
                        e.cond.name = get_symbol_set(s, COND, name);
                        e.cond.expr.type = COND_EQ;
                        e.cond.expr.eq.fab1 = get_symbol_read(s, FAB, name1);
                        e.cond.expr.eq.fab2 = get_symbol_read(s, FAB, name2);
                        e.cond.update = parse_update(s);
                        add_command(e);
                    }
                } else if (peek.type == IDENT && !strcmp(peek.string, "and")) {
                    add_cond_logic(as, command, AND, name, name1);
                } else if (peek.type == IDENT && !strcmp(peek.string, "or")) {
                    add_cond_logic(as, command, OR, name, name1);
                } else if (peek.type == IDENT && !strcmp(peek.string, "xor")) {
                    add_cond_logic(as, command, XOR, name, name1);
                } else if (expect(FLAGS, "flags, '==', 'and', 'or', or 'xor'", flags = parse_consume(s))
                        && expect(REGEX, "a regex", regex = parse_consume(s))
                ) {
                    narrow_flags(as, flags, REGEX_FLAGS);
                    Command r = build_command(Cond);
                    r.cond.name = get_symbol_set(s, COND, name);
                    r.cond.expr.type = COND_REGEX;
                    r.cond.expr.regex.fabric = get_symbol_read(s, FAB, name1);
                    r.cond.expr.regex.flags = flags.flags;
                    r.cond.expr.regex.regex = compile_regex(as, regex, flags.flags);
                    r.cond.update = parse_update(s);
                    if (!as->ignore) add_command(r);
                }
            }
        } else if (!strcmp(as->cmd.string, "if")) {
            if (parse_block_cond(as, If, command)) {
                // we don't want to clear away or warn about any tokens after }
                return (ParseResult) { .verdict = SUCCESS, .command = *command };
            }
        } else if (!strcmp(as->cmd.string, "while")) {
            if (parse_block_cond(as, While, command)) return (ParseResult) { .verdict = SUCCESS, .command = *command };
        } else if (!strcmp(as->cmd.string, "bleach")) {
            Token fabric = parse_consume(s);
            if (expect(IDENT, "a fabric", fabric)) {
                Command b = build_command(Bleach);
                b.sym = get_symbol_get(s, FAB, fabric);
                add_command(b);
            }
        } else if (!strcmp(as->cmd.string, "dye")) {
            Token fabric = parse_consume(s);
            if (expect(IDENT, "a fabric", fabric)) {
                Token arg = parse_consume(s);
                if (arg.type == INT) {
                    Command di = build_command(DyeInt);
                    di.dye_int.fab = get_symbol_get(s, FAB, fabric);
                    di.dye_int.colour = arg.val;
                    add_command(di);
                } else if (expect(IDENT, "an integer or fabric", arg)) {
                    Command df = build_command(DyeFabric);
                    df.dye_fabric.fab = get_symbol_get(s, FAB, fabric);
                    df.dye_fabric.colour = get_symbol_read(s, FAB, arg);
                    add_command(df);
                }
            }
        } else if (!strcmp(as->cmd.string, "embroider")) {
            Token name = parse_consume(s);
            if (expect(IDENT, "a fabric", name)) {
                Flags flags;
                bool parsed_flags = parse_optional_flags(as, &flags, ADD_FLAGS);
                Token string = parse_consume(s);
                if (expect(STRING, parsed_flags ? "a string" : "flags or a string", string)) {
                    Command e = build_command(Embroider);
                    if (!flags.append && !flags.prepend) {
                        if (get_symbol_set_const(s, FAB, name, &e.embroider.target)) {
                            e.embroider.target->fab_data = str_from_nt(string.string);
                        }
                    } else {
                        e.embroider.target = get_symbol_set(s, FAB, name);
                    }
                    e.embroider.flags = flags;
                    e.embroider.source = str_from_nt(string.string);
                    add_command(e);
                }
            }
        } else if (!strcmp(as->cmd.string, "copy")) {
            Token source = parse_consume(s);
            Token target;
            if (expect(IDENT, "a fabric", source)) {
                Flags flags;
                if (parse_optional_flags(as, &flags, ADD_FLAGS)) {
                    Token regex;
                    if (expect(REGEX, "a regex", regex = parse_consume(s))
                     && expect(IDENT, "a fabric", target = parse_consume(s))  
                    ) {
                        Command r = build_command(CopyRegex);
                        r.copy_regex.source = get_symbol_read(s, FAB, source);
                        r.copy_regex.flags = flags;
                        r.copy_regex.regex = compile_regex(as, regex, flags);
                        r.copy_regex.target = _get_symbol_set(s, FAB, target, flags.append || flags.prepend);
                        if (!as->ignore) add_command(r);
                    }
                } else if (expect(IDENT, "flags or a fabric", target = parse_consume(s))) {
                    Command st = build_command(CopyStraight);
                    st.copy_straight.source = get_symbol_read(s, FAB, source);
                    st.copy_straight.target = get_symbol_assign(s, FAB, target);
                    add_command(st);
                }
            }
        } else if (!strcmp(as->cmd.string, "type")) {
            Token name = parse_consume(s);
            TypeExpr te;
            if (expect(IDENT, "a type name", name)
             && expect(EQUALS, "'='", parse_consume(s))
             && parse_type_expr(as, &te)
            ) {
                Vec(TypeExpr) ts = vec_init(TypeExpr, 1);
                vec_append(TypeExpr, ts, te);
                bool good = true;
                while (parse_peek(s).type == PLUS) {
                    parse_consume(s);
                    good = parse_type_expr(as, &te);
                    if (good) vec_append(TypeExpr, ts, te);
                    else break;
                }
                if (good) {
                    Command c = build_command(TypeDec);
                    if (ts->length == 1 && ts->buf.data[0].kind == LITERAL) {
                        if (get_symbol_set_const(s, TYPE, name, &c.type_dec.name)) {
                            c.type_dec.name->type_strs = ts->buf.data[0].values;
                        }
                    } else {
                        c.type_dec.name = get_symbol_assign(s, TYPE, name);
                    }
                    c.type_dec.contents = ts;
                    add_command(c);
                }
            }
        } else if (!strcmp(as->cmd.string, "replace")) {
            Token fabric = parse_consume(s);
            if (expect(IDENT, "a fabric", fabric)) {
                Flags flags;
                bool parsed_flags = parse_optional_flags(as, &flags, ADD_FLAGS | GLOBAL_FLAG);
                Token t1, t2;
                if (expect(IDENT, parsed_flags ? "a type" : "flags or a type", t1 = parse_consume(s))
                 && expect(IDENT, "a type", t2 = parse_consume(s))
                ) {
                    Command c = build_command(Replace);
                    c.replace.data = get_symbol_get(s, FAB, fabric);
                    c.replace.flags = flags;
                    c.replace.type1 = get_symbol_read(s, TYPE, t1);
                    c.replace.type2 = get_symbol_read(s, TYPE, t2);
                    c.replace.trivial = false;

                    if (warn && is_known(c.replace.type1)) {
                        Vec(TailorStr) x = c.replace.type1->type_strs;
                        for (idx_t i = 0; i < x->length; i++) {
                            if (x->buf.data[i].data->length == 0) {
                                warning(s, as->cmd, t1, t1, "constant argument to replace contains an empty string");
                            }
                        }
                    }
                    if (is_known(c.replace.type1) && is_known(c.replace.type2)) {
                        Vec(TailorStr) x = c.replace.type1->type_strs;
                        Vec(TailorStr) y = c.replace.type2->type_strs;

                        if (warn && x->length != y->length) {
                            warning(s, as->cmd, t1, t2, "constant arguments to replace have different sizes");
                        }

                        if (!flags.prepend && !flags.append) {
                            c.replace.trivial = true;
                            for (idx_t i = 0; i < x->length && i < y->length; i++) {
                                if (x->buf.data[i].data->length != y->buf.data[i].data->length) {
                                    c.replace.trivial = false;
                                    break;
                                }
                            }
                        }
                    }
                    add_command(c);
                }
            }
        } else if (!strcmp(as->cmd.string, "alter")) {
            Token fabric, flags, regex, replace;
            if (expect(IDENT, "a fabric", fabric = parse_consume(s))
             && expect(FLAGS, "flags", flags = parse_consume(s))
             && expect(REGEX, "a regex", regex = parse_consume(s))
             && expect(STRING, "a string", replace = parse_consume(s))
            ) {
                Command c = build_command(Alter);
                c.alter.data = get_symbol_get(s, FAB, fabric);
                c.alter.flags = flags.flags;
                c.alter.regex = compile_regex(as, regex, flags.flags);
                c.alter.replacement = str_from_nt(replace.string);
                if (!as->ignore) add_command(c);
            }
        } else if (!strcmp(as->cmd.string, "procedure")) {
            Token name, start;
            Vec(Token) args;
            if (expect(IDENT, "a procedure name", name = parse_consume(s))
             && expect(LPAREN, "'('", parse_consume(s))
             && (args = parse_comma_sep(as, IDENT, "an argument name", RPAREN, "')'")) != NULL
             && expect(LBRACE, "a block", start = parse_consume(s))
            ) {
                Command c = build_command(Procedure);
                if (!get_symbol_set_const(s, PROC, name, &c.proc.name) && warn) {
                    warning(s, as->cmd, name, name, "redefinition of procedure '%s'", name.string);
                }
                Vec(Symbol_p) symbols = vec_init(Symbol_p, args->length+1);
                for (int i = 0; i < args->length; i++) {
                    vec_append(Symbol_p, symbols, get_symbol_set(s, FAB, args->buf.data[i]));
                }
                vec_destroy(Token, args);
                c.proc.args = symbols;
                c.proc.name->proc_args = symbols;
                c.proc.code = parse(s, &start);
                c.proc.name->proc_code = c.proc.code;
                add_command(c);
            }
        } else if (!strcmp(as->cmd.string, "do")) {
            Token name = parse_consume(s);
            if (expect(IDENT, "a procedure", name)
             && expect(LPAREN, "'('", parse_consume(s))
            ) {
                Vec(Token) args = parse_comma_sep(as, IDENT, "a fabric", RPAREN, "')'");
                if (args != NULL) {
                    Symbol *proc = get_symbol_read(s, PROC, name);
                    Vec(Symbol_p) symbols = vec_init(Symbol_p, args->length+1);
                    Vec(Token) additional = vec_init(Token, 0);
                    for (int i = 0; i < args->length; i++) {
                        vec_append(Symbol_p, symbols, get_symbol_get(s, FAB, args->buf.data[i]));
                        if (warn && is_known(proc) && i >= proc->proc_args->length) {
                            vec_append(Token, additional, args->buf.data[i]);
                        }
                    }
                    if (warn) {
                        if (additional->length) {
                            warning(s, as->cmd, additional->buf.data[0], additional->buf.data[additional->length-1], "extraneous arguments to '%s' will be ignored", proc->name);
                        } else if (is_known(proc) && args->length < proc->proc_args->length) {
                            warning(s, as->cmd, out_ender, out_ender,
                                    "insufficient arguments to '%s' (%d expected, got %d)", proc->name, proc->proc_args->length, args->length);
                        }
                    }
                    vec_destroy(Token, args);
                    Command c = build_command(Call);
                    c.call.name = proc;
                    c.call.args = symbols;
                    add_command(c);
                }
            }
        } else if (!strcmp(as->cmd.string, "variation")) {
            Token filename = parse_consume(s);
            if (expect(IDENT, "a file path", filename)) {
                FILE *fp = fopen(filename.string, "r");
                if (fp == NULL) {
                    if (warn) warning(s, as->cmd, filename, filename, "failed to read file: %s (command will be ignored)", strerror(errno));
                    as->ignore = true;
                } else {
                    Command c = build_command(Variation);
                    ParseState *inner;
                    c.variation.code = parse_file(fp, filename.string, recurse_warn, &inner);
                    c.variation.assocs = vec_init(VariationAssoc, 0);

                    // copy procedures
                    char *base_name = basename(filename.string);
                    strrchr(base_name, '.')[1] = '\0';
                    idx_t base_len = strlen(base_name);

                    bool all_const = true;
                    for (idx_t i = 0; i < inner->procedures->length; i++) {
                        Symbol *source = &inner->procedures->buf.data[i];
                        idx_t source_len = strlen(source->name);

                        char *name = malloc(base_len+source_len+1);
                        memcpy(name, base_name, base_len);
                        memcpy(name+base_len, source->name, source_len);
                        name[base_len+source_len] = '\0';

                        Token cmd = filename;
                        cmd.string = name;
                        Symbol *target;
                        if (is_known(source)) {
                            if (!get_symbol_set_const(s, PROC, cmd, &target) && warn) {
                                warning(s, as->cmd, filename, filename, "file contains member '%s', which shadows an existing name '%s'", source->name, name);
                            }
                            target->is_read = true;  // we don't need errors for unused import members (unless *all* members are unused; TODO)
                            target->proc_args = source->proc_args;  // this might cause bugs?
                            target->proc_code = source->proc_code;
                        } else {
                            all_const = false;
                            target = get_symbol_set(s, PROC, cmd);
                        }

                        vec_append(VariationAssoc, c.variation.assocs, ((VariationAssoc) { .source = source, .target = target }));
                    }
                    c.variation.all_const = all_const;

                    parse_destroy(inner);
                    free(base_name);
                    add_command(c);
                }
            }
        } else {
            as->ignore = true;
            if (warn) {
                warning(s, as->cmd, as->cmd, as->cmd, "unknown command name '%s' (command will be ignored)", as->cmd.string);
            }
        }

        Token next = parse_peek(s);
        if (!s->last_was_sep && next.type != LINE_SEP && next.type != RBRACE) {
            parse_consume(s);
            Token start = next;
            Token end = next;
            while (true) {
                Token n = parse_consume(s);
                if (n.type == LINE_SEP || n.type == RBRACE) break;
                end = n;
            }
            if (!as->ignore && warn) {
                warning(s, as->cmd, start, end, "extra tokens after end of command ignored");
            }
        }

        if (!as->ignore) break;
    }

    return (ParseResult) { .verdict = SUCCESS, .command = *command };
}

void check_unused(ParseState *s, Vec(Symbol) group) {
    for (idx_t i = 0; i < group->length; i++) {
        Symbol *p = &group->buf.data[i];
        if (!p->is_read) {
            warning(s, p->defined_by, p->defined_by, p->defined_by, "symbol '%s' never used (first defined here)", p->name);
            show_similar_name(s, group, p->name);
        }
    }
}

Vec(Command) parse(ParseState *s, Token *bloody_mary) {
    ParseResult c;
    Vec(Command) commands = vec_init(Command, 0);
    while ((c = parse_command(s, bloody_mary)).verdict == SUCCESS) {
        vec_append(Command, commands, c.command);
    }
    return commands;
}

Vec(Command) parse_file(FILE *fp, char *filename, bool w, ParseState **s_out) {
    bool old_warn = warn;
    warn = w;

    LexState *ls = lex_new(fp, filename);
    ParseState *s = parse_new(ls, filename);
    *s_out = s;
    Vec(Command) commands = parse(s, NULL);

    if (warn) {
        check_unused(s, s->fabrics);
        // procedures may be intended to be exported
        //check_unused(s, s->procedures);
        check_unused(s, s->conditions);
        check_unused(s, s->types);
    }

    warn = old_warn;
    return commands;
}


void acquire(Symbol *sym, Vec(Symbol_p) frame) {
    if (sym->defined == FREE) {
        sym->defined = BOUND;
        vec_append(Symbol_p, frame, sym);
        switch (sym->type) {
          case COND:
            sym->cond_update = false;
            sym->cond_value = false;
            break;
          case FAB:
            sym->fab_data = str_new();
            break;
          case PROC:
            sym->proc_args = vec_init(Symbol_p, 0);
            sym->proc_code = vec_init(Command, 0);
            break;
          case TYPE:
            sym->type_strs = vec_init(TailorStr, 0);
            break;
        }
    }
}

bool eval_cond(Symbol *s, Vec(Symbol_p) frame) {
    CondExpr e = s->cond_expr;
    switch (e.type) {
      case COND_REGEX: {
        acquire(e.regex.fabric, frame);
        // consider keeping a match data object for longer if it's a performance concern
        pcre2_match_data *md = pcre2_match_data_create(1, NULL);
        int r = pcre2_match(
            e.regex.regex,
            (unsigned char *) e.regex.fabric->fab_data.data->buf.data,
            e.regex.fabric->fab_data.data->length,
            0,
            0,
            md,
            NULL
        );
        pcre2_match_data_free(md);
        // this might bite
        return r >= 0;
      }
      case COND_NOT:
        acquire(e.not.condition, frame);
        return !eval_cond(e.not.condition, frame);
      case COND_EQ:
        acquire(e.eq.fab1, frame);
        acquire(e.eq.fab2, frame);
        return vec_eq(char, e.eq.fab1->fab_data.data, e.eq.fab2->fab_data.data);
      case COND_LOGIC:
        acquire(e.logic.cond1, frame);
        acquire(e.logic.cond2, frame);
        bool x = eval_cond(e.logic.cond1, frame);
        bool y = eval_cond(e.logic.cond2, frame);
        switch (e.logic.op) {
            case AND: return x && y;
            case OR: return x || y;
            case XOR: return x != y;
        }
    }
}

bool cond_value(Symbol *s, Vec(Symbol_p) frame) {
    if (!s->cond_update) return s->cond_value;
    return eval_cond(s, frame);
}

void extend_alter_replacement(Vec(char) out, Command cmd, size_t *ovector, pcre2_code *regex, Vec(char) source) {
    Vec(char) rep = cmd.alter.replacement.data;
    for (idx_t i = 0; i < rep->length; i++) {
        char c = rep->buf.data[i];
        if (c == '\\') {
            idx_t group_num;

            char nc = rep->buf.data[i+1];
            if (nc == '\\') {
                i += 1;
                vec_append(char, out, '\\');
                continue;
            } else if (nc == '<') {
                idx_t start = i+2;

                idx_t n;
                unsigned int amount_read;
                if (sscanf(&rep->buf.data[start], "%d%n", &n, &amount_read) && rep->buf.data[start+amount_read] == '>') {
                    group_num = n;
                    i = start+amount_read;
                } else {
                    char *p = strchr(&rep->buf.data[start], '>');
                    char c = *p;
                    *p = '\0';
                    int r = pcre2_substring_number_from_name(regex, (unsigned char *) &rep->buf.data[start]);
                    *p = c;
                    if (r < 0) {
                        vec_append(char, out, '\\');
                        continue;
                    }
                    group_num = r;
                    i = p - rep->buf.data;
                }
            } else {
                idx_t start = i+1;
                idx_t n;
                unsigned int amount_read;
                if (sscanf(&rep->buf.data[start], "%d%n", &n, &amount_read)) {
                    group_num = n;
                    i += amount_read;
                } else {
                    vec_append(char, out, '\\');
                    continue;
                }
            }

            vec_extend(char, out, &source->buf.data[ovector[group_num*2]], ovector[group_num*2+1]-ovector[group_num*2]);
        } else {
            vec_append(char, out, c);
        }
    }
}

typedef enum InterpretResult {
    OK,
    PROC_EXIT,
    PATTERN_EXIT,
} InterpretResult;

InterpretResult interpret_new_frame(Vec(Command) code);

InterpretResult interpret(Vec(Command) code, Vec(Symbol_p) frame);

InterpretResult interpret_command(Command cmd, Vec(Symbol_p) frame) {
    switch (cmd.type) {
      case Gather:
        acquire(cmd.sym, frame);
        char *in = readline("");
        vec_resize(char, cmd.sym->fab_data.data, 0);
        if (in) {
            if (*in) add_history(in);
            vec_extend(char, cmd.sym->fab_data.data, in, strlen(in));
            free(in);
        }
        break;
      case Sell:
        acquire(cmd.sym, frame);
        fwrite(cmd.sym->fab_data.data->buf.data, 1, cmd.sym->fab_data.data->length, stdout);
        break;
      case Stop:
        return PATTERN_EXIT;
      case End:
        return PROC_EXIT;
      case Notch:
        // TODO
        break;
      case SeeInt:
        // TODO
        break;
      case SeeNotch:
        // TODO
        break;
      case Hem: {
        Symbol *s = cmd.sym;
        acquire(s, frame);
        Vec(char) out = vec_init(char, 0);
        char *data = s->fab_data.data->buf.data;
        for (idx_t i = 0; i < s->fab_data.data->length; i++) {
            if (data[i] == '\\') {
                if (data[i+1] == 'n') {
                    vec_append(char, out, '\n');
                    i += 1;
                    continue;
                } else if (data[i+1] == '\\') {
                    vec_append(char, out, '\\');
                    i += 1;
                    continue;
                } else if (data[i+1] == 'u') {
                    uint16_t n = 0;
                    for (idx_t j = i+2; j < i+6; j++) {
                        n *= 16;
                        char c = data[j];
                        if (c >= '0' && c <= '9') {
                            n += c - '0';
                        } else if (c >= 'A' && c <= 'F') {
                            n += c - 'A' + 10;
                        } else if (c >= 'a' && c <= 'f') {
                            n += c - 'a' + 10;
                        } else {
                            goto fail;
                        }
                    }
                    char x[4];
                    idx_t l;
                    if (n <= 0x7F) {
                        x[0] = n;
                        l = 1;
                    } else if (n <= 0x7FF) {
                        x[0] = 0xC0 | (n >> 6);
                        x[1] = 0x80 | (n & 0x3F);
                        l = 2;
                    } else if (n >= 0xD800 && n <= 0xDFFF) {
                        l = -1;
                    } else {
                        x[0] = 0xE0 | (n >> 12);
                        x[1] = 0x80 | ((n >> 6) & 0x3F);
                        x[2] = 0x80 | (n & 0x3F);
                        l = 3;
                    }
                    if (l > 0) {
                        vec_extend(char, out, x, l);
                        i += 5;
                        continue;
                    }
                }
            }
            fail:
            vec_append(char, out, data[i]);
        }
        vec_destroy(char, s->fab_data.data);
        s->fab_data.data = out;
        break;
      }
      case Cond: {
        Symbol *name = cmd.cond.name;
        acquire(name, frame);
        name->cond_update = cmd.cond.update;
        name->cond_expr = cmd.cond.expr;
        if (!cmd.cond.update) {
            name->cond_value = eval_cond(name, frame);
        }
        break;
      }
      case If:
        if (cond_value(cmd.block_cond.cond, frame)) {
            int r = interpret(cmd.block_cond.code, frame);
            if (r != OK) return r;
        }
        break;
      case While:
        while (cond_value(cmd.block_cond.cond, frame)) {
            int r = interpret(cmd.block_cond.code, frame);
            if (r != OK) return r;
        }
        break;
      case Bleach:
        // ...
        break;
      case DyeInt:
        // ...
        break;
      case DyeFabric:
        // ...
        break;
      case Embroider: {
        Symbol *target = cmd.embroider.target;
        if (is_known(target)) break;
        acquire(target, frame);

        Flags flags = cmd.embroider.flags;
        TailorStr source = cmd.embroider.source;

        Vec(char) target_data = target->fab_data.data;
        idx_t target_len = target_data->length;
        char *source_data = source.data->buf.data;
        idx_t source_len = source.data->length;

        if (flags.prepend) {
            idx_t new_len = target_len + source_len*(flags.prepend + flags.append);
            TailorStr new = str_new();
            raw_vec_reserve(char, &new.data->buf, new_len);
            vec_extend(char, new.data, source_data, source_len);
            vec_extend(char, new.data, target_data->buf.data, target_len);
            if (flags.append) {
                vec_extend(char, new.data, source_data, source_len);
            }
            target->fab_data = new;
        } else {
            if (!flags.append) {
                vec_resize(char, target->fab_data.data, 0);
            }
            vec_extend(char, target_data, source_data, source_len);
        }
        break;
      }
      case CopyStraight: {
        Symbol *target = cmd.copy_straight.target;
        Symbol *source = cmd.copy_straight.source;
        acquire(target, frame);
        acquire(source, frame);
        vec_destroy(char, target->fab_data.data);
        target->fab_data.data = vec_copy(char, source->fab_data.data);
        break;
      }
      case CopyRegex: {
        Symbol *target = cmd.copy_regex.target;
        Symbol *source = cmd.copy_regex.source;
        Flags flags = cmd.copy_regex.flags;
        acquire(target, frame);
        acquire(source, frame);

        Vec(char) out_vec;
        if (!flags.append || flags.prepend) {
            out_vec = vec_init(char, 0);
        } else {
            out_vec = target->fab_data.data;
        }

        pcre2_match_data *md = pcre2_match_data_create(1, NULL);
        idx_t offset = 0;
        while (true) {
            int m = pcre2_match(
                cmd.copy_regex.regex,
                (unsigned char *) source->fab_data.data->buf.data,
                source->fab_data.data->length,
                offset,
                0,
                md,
                NULL
            );
            if (m < 0) break;

            PCRE2_SIZE *v = pcre2_get_ovector_pointer(md);
            vec_extend(char, out_vec, &source->fab_data.data->buf.data[v[0]], v[1] - v[0]);
            offset = v[1];
        }
        pcre2_match_data_free(md);

        if (flags.prepend) {
            if (flags.append) {
                Vec(char) new_vec = vec_init(char, 0);
                vec_extend(char, new_vec, out_vec->buf.data, out_vec->length);
                vec_extend(char, new_vec, target->fab_data.data->buf.data, target->fab_data.data->length);
                vec_extend(char, new_vec, out_vec->buf.data, out_vec->length);
                vec_destroy(char, out_vec);
                out_vec = new_vec;
            } else {
                vec_extend(char, out_vec, target->fab_data.data->buf.data, target->fab_data.data->length);
            }
        }

        if (out_vec != target->fab_data.data) {
            vec_destroy(char, target->fab_data.data);
            target->fab_data.data = out_vec;
        }
        break;
      }
      case TypeDec: {
        Symbol *data = cmd.type_dec.name;
        if (is_known(data)) break;
        acquire(data, frame);
        Vec(TypeExpr) contents = cmd.type_dec.contents;

        Vec(TailorStr) strings = vec_init(TailorStr, 1);
        for (idx_t i = 0; i < contents->length; i++) {
            Vec(TailorStr) next_strings;
            if (contents->buf.data[i].kind == VARIABLE) {
                next_strings = contents->buf.data[i].name->type_strs;
            } else {
                next_strings = contents->buf.data[i].values;
            }
            vec_extend(TailorStr, strings, next_strings->buf.data, next_strings->length);
        }

        data->type_strs = strings;
        break;
      }
      case Replace: {
        Symbol *string = cmd.replace.data;
        Flags flags = cmd.replace.flags;
        Symbol *type1 = cmd.replace.type1;
        Symbol *type2 = cmd.replace.type2;
        acquire(string, frame);
        acquire(type1, frame);
        acquire(type2, frame);

        bool trivial = is_known(type1) && is_known(type2) && cmd.replace.trivial;
        if (!trivial && !flags.prepend && !flags.append) {
            trivial = true;
            for (idx_t i = 0; i < type1->type_strs->length && i < type2->type_strs->length; i++) {
                if (type1->type_strs->buf.data[i].data->length != type2->type_strs->buf.data[i].data->length) {
                    trivial = false;
                }
            }
        }

        if (trivial) {
            // The simplest and most common case. We don't have to worry about lengths, so just look for strings and replace them in-place.
            for (idx_t i = 0; i < string->fab_data.data->length; i++) {
                for (idx_t j = 0; j < type1->type_strs->length && j < type2->type_strs->length; j++) {
                    // no fancy string finding here
                    char *t = &string->fab_data.data->buf.data[i];
                    Vec(char) candidate = type1->type_strs->buf.data[j].data;
                    Vec(char) replacement = type2->type_strs->buf.data[j].data;
                    if (!strncmp(t, candidate->buf.data, candidate->length)) {
                        memcpy(t, replacement->buf.data, replacement->length);
                        break;
                    }
                }
            }
        } else {
            // Doing anything more complicated than this is hard to think about and probably not worth my time, so we'll just build a new string from scratch.
            Vec(char) s = vec_init(char, string->fab_data.data->length);
            for (idx_t i = 0; i < string->fab_data.data->length; i++) {
                char *c = &string->fab_data.data->buf.data[i];
                for (idx_t j = 0; j < type1->type_strs->length && j < type2->type_strs->length; j++) {
                    Vec(char) candidate = type1->type_strs->buf.data[j].data;
                    Vec(char) replacement = type2->type_strs->buf.data[j].data;
                    if (!strncmp(c, candidate->buf.data, candidate->length)) {
                        #define push_replacement vec_extend(char, s, replacement->buf.data, replacement->length)
                        if (!flags.append && !flags.prepend) {
                            push_replacement;
                        } else {
                            if (flags.prepend) push_replacement;
                            vec_append(char, s, *c);
                            if (flags.append) push_replacement;
                        }
                        i += candidate->length - 1;
                        goto cont;
                    }
                }
                vec_append(char, s, *c);
                cont:;
            }
            vec_destroy(char, string->fab_data.data);
            string->fab_data.data = s;
        }
        break;
      }
      case Alter: {
        Symbol *data = cmd.alter.data;
        acquire(data, frame);
        Vec(char) out = vec_init(char, 0);
        pcre2_match_data *md = pcre2_match_data_create_from_pattern(cmd.alter.regex, NULL);
        size_t offset = 0;
        do {
            int r = pcre2_match(
                cmd.alter.regex,
                (unsigned char *) data->fab_data.data->buf.data,
                data->fab_data.data->length,
                offset,
                0,
                md,
                NULL
            );
            if (r < 0) break;
            size_t *ovector = pcre2_get_ovector_pointer(md);
            vec_extend(char, out, &data->fab_data.data->buf.data[offset], ovector[0]-offset);
            if (!cmd.alter.flags.prepend && !cmd.alter.flags.append) {
                extend_alter_replacement(out, cmd, ovector, cmd.alter.regex, data->fab_data.data);
            } else {
                if (cmd.alter.flags.prepend) extend_alter_replacement(out, cmd, ovector, cmd.alter.regex, data->fab_data.data);
                vec_extend(char, out, &data->fab_data.data->buf.data[ovector[0]], ovector[1]-ovector[0]);
                if (cmd.alter.flags.append) extend_alter_replacement(out, cmd, ovector, cmd.alter.regex, data->fab_data.data);
            }
            offset = ovector[1];
        } while (cmd.alter.flags.global);
        vec_extend(char, out, &data->fab_data.data->buf.data[offset], data->fab_data.data->length-offset);
        vec_destroy(char, data->fab_data.data);
        data->fab_data.data = out;
        pcre2_match_data_free(md);
        break;
      }
      case Procedure: {
        Symbol *name = cmd.proc.name;
        if (is_known(name)) break;
        acquire(name, frame);
        name->proc_args = cmd.proc.args;
        name->proc_code = cmd.proc.code;
        break;
      }
      case Call: {
        Symbol *func = cmd.call.name;
        acquire(func, frame);
        // call the symbols from the procedure's perspective "procedure arguments", and from the caller's perspective "call arguments"

        // copy call arguments to corresponding procedure arguments
        for (idx_t i = 0; i < func->proc_args->length; i++) {  // ignore additional arguments
            Symbol *arg = func->proc_args->buf.data[i];
            if (arg->defined != FREE) continue;
            acquire(arg, frame);
            if (i < cmd.proc.args->length) {
                arg->fab_data.data = vec_copy(char, cmd.proc.args->buf.data[i]->fab_data.data);
            } else {
                // fill in missing arguments with empty strings
                arg->fab_data = str_new();
            }
        }

        // call the function
        InterpretResult r = interpret_new_frame(func->proc_code);
        if (r == PATTERN_EXIT) return r;

        // copy back modified fabrics
        for (idx_t i = 0; i < func->proc_args->length && i < cmd.proc.args->length; i++) {
            cmd.proc.args->buf.data[i]->fab_data.data = vec_copy(char, func->proc_args->buf.data[i]->fab_data.data);
        }
        break;
      }
      case Variation:
        interpret_new_frame(cmd.variation.code);
        if (cmd.variation.all_const) break;
        for (idx_t i = 0; i < cmd.variation.assocs->length; i++) {
            VariationAssoc assoc = cmd.variation.assocs->buf.data[i];
            if (is_known(assoc.target)) continue;
            assoc.target->proc_args = assoc.source->proc_args;
            assoc.target->proc_code = assoc.source->proc_code;
        }
        break;
    }
    return OK;
}

InterpretResult interpret(Vec(Command) code, Vec(Symbol_p) frame) {
    for (idx_t idx = 0; idx < code->length; idx++) {
        InterpretResult r = interpret_command(code->buf.data[idx], frame);
        if (r != OK) return r;
    }
    return OK;
}

InterpretResult interpret_new_frame(Vec(Command) code) {
    Vec(Symbol_p) frame = vec_init(Symbol_p, 16);
    InterpretResult r = interpret(code, frame);
    for (idx_t i = 0; i < frame->length; i++) {
        Symbol *sym = frame->buf.data[i];
        sym->defined = FREE;
        if (sym->type == FAB) free(sym->fab_data.data);
        else if (sym->type == TYPE) vec_destroy(TailorStr, sym->type_strs);
    }
    if (r == PROC_EXIT) return OK;
    return r;
}

int main(int argc, char *argv[]) {
    bool interpret = true;
    bool warn_opt = false;

    int opt;
    while ((opt = getopt(argc, argv, "wcr")) != -1) {
        switch (opt) {
            case 'w': warn_opt = true; break;
            case 'c': interpret = false; warn_opt = true; break;
            case 'r': recurse_warn = true; break;
            case '?': eprintf("usage: %s [OPTIONS]... PROGRAM\n", argv[0]); return 1;
        }
    }

    FILE *fp;
    char *filename;
    if (optind >= argc) {
        filename = "<stdin>";
        fp = stdin;
        //if (isatty(fileno(stdin))) repl = true;
    } else {
        filename = argv[optind];
        fp = fopen(filename, "r");
        if (fp == NULL) {
            perror("clothier: failed to read file");
            return 1;
        }
    }

    if (repl) {
        warn = true;
        Vec(Symbol_p) frame = vec_init(Symbol_p, 16);
        LexState *ls = lex_new(fp, filename);
        ParseState *s = parse_new(ls, filename);
        while (true) {
            printf(">>> ");
            ParseResult cmd = parse_command(s, NULL);
            if (cmd.verdict != SUCCESS) break;
            interpret_command(cmd.command, frame);
        }
    } else {
        ParseState *ps;
        Vec(Command) ast = parse_file(fp, filename, warn_opt, &ps);
        parse_destroy(ps);

        if (interpret) {
            interpret_new_frame(ast);
        }
    }
}
