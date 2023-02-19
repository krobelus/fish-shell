#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::parse_constants::{
    ParseErrorList, ParseKeyword, ParseTokenType, ParseTreeFlags, SourceRange, StatementDecoration,
    PARSE_FLAG_ACCEPT_INCOMPLETE_TOKENS, PARSE_FLAG_CONTINUE_AFTER_ERROR,
    PARSE_FLAG_INCLUDE_COMMENTS,
};
use crate::tokenizer::{
    TokFlags, TokenType, TOK_ACCEPT_UNFINISHED, TOK_CONTINUE_AFTER_ERROR, TOK_SHOW_COMMENTS,
};
use crate::wchar::{wstr, WString, L};
use crate::wchar_ffi::{WCharFromFFI, WCharToFFI};
use crate::wutil::format::printf::sprintf;
use autocxx::extern_rust_type;
use cxx::{type_id, ExternType};
use cxx::{CxxWString, SharedPtr, UniquePtr};
use std::any::Any;
use std::pin::Pin;

/**
 * This defines the fish abstract syntax tree.
 * The fish ast is a tree data structure. The nodes of the tree
 * are divided into three categories:
 *
 * - leaf nodes refer to a range of source, and have no child nodes.
 * - branch nodes have ONLY child nodes, and no other fields.
 * - list nodes contain a list of some other node type (branch or leaf).
 *
 * Most clients will be interested in visiting the nodes of an ast.
 * See NodeVisitation below.
 */

pub enum Category {
    branch,
    leaf,
    list,
}

/*
* A FieldVisitor is something which can visit the fields of an ast node.
* This is used during ast construction.
*
* To trigger field visitation, use the accept() function:
*    MyFieldVisitor v;
*    node->accept(v);
*
* Example FieldVisitor:
*
* struct MyFieldVisitor {
*
*    /// will_visit (did_visit) is called before (after) a node's fields are visited.
*    void will_visit_fields_of(node_t &node);
*    void did_visit_fields_of(node_t &node);
*
*    /// These are invoked with the concrete type of each node,
*    /// so they may be overloaded to distinguish node types.
*    /// Example:
*    void will_visit_fields_of(job_t &job);
*
*    /// The visitor needs to be prepared for the following four field types.
*    /// Naturally the visitor may overload visit_field to carve this
*    /// arbitrarily finely.
*
*    /// A field may be a "direct embedding" of a node.
*    /// That is, an ast node may have another node as a member.
*    template <typename Node>
*    void visit_node_field(Node &node);

*    /// A field may be a list_t of (pointers to) some other node type.
*    template <type_t List, typename Node>
*    void visit_list_field(list_t<List, Node> &list);
*
*    /// A field may be a unique_ptr to another node.
*    /// Every such pointer must be non-null after construction.
*    template <typename Node>
*    void visit_pointer_field(std::unique_ptr<Node> &ptr);
*
*    /// A field may be optional, meaning it may or may not exist.
*    template <typename Node>
*    void visit_optional_field(optional_t<NodeT> &opt);
*
*    /// A field may be a union pointer, meaning it points to one of
*    /// a fixed set of node types. A union pointer is never null
*    /// after construction.
*    template <typename... Nodes>
*    void visit_union_field(union_ptr_t<Nodes...> &union_ptr);
* };
*/

pub trait NodeVisitor {
    fn visit(&mut self, node: *const dyn Nodeish) {}
}

struct ExampleVisitor {}
impl ExampleVisitor {
    fn visit(&mut self, node: *const dyn Nodeish) {}
}

pub trait Acceptor {
    fn accept(&self, visitor: &mut dyn NodeVisitor, reversed: bool);
}

impl<T: Acceptor> Acceptor for Option<T> {
    fn accept(&self, visitor: &mut dyn NodeVisitor, reversed: bool) {
        match self {
            Some(node) => node.accept(visitor, reversed),
            None => (),
        }
    }
}

pub trait Nodeish: Acceptor {
    /// The parent node, or null if this is root.
    fn parent(&self) -> Option<*const dyn Nodeish>;

    /// The type of this node.
    fn typ(&self) -> Type;

    /// The category of this node.
    fn category(&self) -> Category;

    /// Cast to a concrete node type, aborting on failure.
    /// Example usage:
    /// TODO
    ///   if (node->type == type_t::job_list) node->as_job_list()->...
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    /// \return a helpful string description of this node.
    fn describe(&self) -> WString;

    /// \return the source range for this node, or none if unsourced.
    /// This may return none if the parse was incomplete or had an error.
    fn try_source_range(&self) -> Option<SourceRange>;

    /// \return the source range for this node, or an empty range {0, 0} if unsourced.
    fn source_range(&self) -> SourceRange {
        self.try_source_range().unwrap_or(SourceRange::new(0, 0))
    }

    /// \return the source code for this node, or none if unsourced.
    fn try_source<'a>(&self, orig: &'a wstr) -> Option<&'a wstr> {
        self.try_source_range()
            .map(|r| &orig[r.start as usize..r.end() as usize])
    }

    /// \return the source code for this node, or an empty string if unsourced.
    fn source<'a>(&self, orig: &'a wstr) -> &'a wstr {
        self.try_source(orig).unwrap_or_default()
    }
}

macro_rules! ast_type_to_string_impl {
    (
        $input:ident,
        (
            $name:ident, $snake_case_name:ident : $base:tt,
            $category:ident $( : $list_element:ident )?,
            $(($field:ident: $t:ty)),*
            $(,)?)
    ) => {
        if $input == Type::$snake_case_name {
            return L!(stringify!($snake_case_name));
        }
    };
}

macro_rules! nodes {
    (
        $($nodes:tt);*
        $(;)?
    ) => {
        pub fn ast_type_to_string(t: Type) -> &'static wstr {
            $(
                ast_type_to_string_impl!(t, $nodes);
            )*
            panic!();
        }

        $(
            node!($nodes);
        )*
    };
}

pub trait Leafish {
    fn range(&self) -> SourceRange;
}

macro_rules! leafish_unsourced {
    (leaf) => {
        bool
    };
    ($category:ident) => {
        ()
    };
}
macro_rules! leafish_range {
    (leaf) => {
        SourceRange
    };
    ($category:ident) => {
        ()
    };
}
macro_rules! leafish_impl {
    ($name:ident, leaf) => {
        impl Leafish for $name {
            fn range(&self) -> SourceRange {
                self.range
            }
        }
    };
    ($name:ident, $category:ident) => {};
}

pub trait Tokenish: Nodeish {
    fn token_type(&self) -> ParseTokenType;
    // TODO make it a free function
    fn allows_token(&self, token_type: ParseTokenType) -> bool;
}

macro_rules! tokenish_data {
    ((Tokenish($($allowed:expr),*))) => {
        ParseTokenType
    };
    ($base:tt) => {
        ()
    };
}

macro_rules! tokenish_impl {
    ($name:ident, (Tokenish($($allowed:expr),*))) => {
        impl Tokenish for $name {
            fn token_type(&self) -> ParseTokenType {
                self.parse_token_type
            }
            fn allows_token(&self, token_type: ParseTokenType) -> bool {
                [$($allowed),*].contains(&token_type)
            }
        }
    };
    ($name:ident, $base:tt) => {};
}

pub trait Keywordish: Nodeish {
    fn keyword(&self) -> ParseKeyword;
    // TODO
    fn allows_keyword(&self, kw: ParseKeyword) -> bool;
}

macro_rules! keywordish_data {
    ((Keywordish($($allowed:expr),*))) => {
        ParseKeyword
    };
    ($base:tt) => {
        ()
    };
}
macro_rules! keywordish_impl {
    ($name:ident, (Keywordish($($allowed:expr),*))) => {
        impl Keywordish for $name {
            fn keyword(&self) -> ParseKeyword {
                self.keyword
            }
            fn allows_keyword(&self, kw: ParseKeyword) -> bool {
                [$($allowed),*].contains(&kw)
            }
        }
    };
    ($name:ident, $base:tt) => {};
}

macro_rules! list_data {
    ($list_element:ident) => {
        Vec<$list_element>
    };
    () => {
        ()
    };
}

macro_rules! accept_impl {
    (
        $self:ident,
        $visitor:ident,
        $reversed:ident,
        ( $list_element:ident ),
    ) => {
        $self.visit
        if $reversed {
            for child in $self.children.iter().rev() {
                $child.accept($visitor, $reversed);
            }
        } else {
            for child in &$self.children {
                $child.accept($visitor, $reversed);
            }
        }
    };
    (
        $self:ident,
        $visitor:ident,
        $reversed:ident,
        $($field:ident)*,
        $list:tt,
    ) => {
        // TODO reverse
        $(
            $self.$field.accept($visitor, $reversed);
        )*
    };
}

macro_rules! node {
    ((
        $name:ident, $snake_case_name:ident : $base:tt,
        $category:ident $( : $list_element:ident )?,
        $(($field:ident: $t:ty)),*
        $(,)?
    )) => {
        pub struct $name {
            parent: Option<*const dyn Nodeish>,

            $(
                pub $field: $t,
            )*
            children: list_data!($($list_element)*),

            unsourced: leafish_unsourced!($category),
            range: leafish_range!($category),
            parse_token_type: tokenish_data!($base),
            keyword: keywordish_data!($base),
        }
        leafish_impl!($name, $category);
        tokenish_impl!($name, $base);
        keywordish_impl!($name, $base);
        impl Acceptor for $name {
            fn accept(&self, visitor: &mut dyn NodeVisitor, reversed: bool) {
                visitor.visit(self);
                accept_impl!(
                    self,
                    visitor,
                    reversed,
                    $($field)*,
                    ( $($list_element)* ),
                );
            }
        }
        impl Nodeish for $name {
            fn parent(&self) -> Option<*const dyn Nodeish> {
                self.parent
            }
            fn typ(&self) -> Type {
                Type::$snake_case_name
            }
            fn category(&self) -> Category {
                Category::$category
            }
            fn as_any(&self) -> &dyn Any {
                self
            }
            fn as_any_mut(&mut self) -> &mut dyn Any {
                self
            }
            fn describe(&self) -> WString {
                let mut res = ast_type_to_string(self.typ()).to_owned();
                if let Some(n) = self.as_any().downcast_ref::<&dyn Tokenish>() {
                    let token_type: &'static wstr = n.token_type().into();
                    res += &sprintf!(L!(" '%ls'"), token_type)[..];
                } else if let Some(n) = self.as_any().downcast_ref::<&dyn Keywordish>() {
                    let keyword: &'static wstr = n.keyword().into();
                    res += &sprintf!(L!(" '%ls'"), keyword)[..];
                }
                res
            }
            fn try_source_range(&self) -> Option<SourceRange> {
                todo!()
            }
        }
    }
}

pub enum ArgumentOrRedirection {
    Argument(ArgumentNode),
    Redirection(RedirectionNode),
}

impl Acceptor for ArgumentOrRedirection {
    fn accept(&self, visitor: &mut dyn NodeVisitor, reversed: bool) {
        match self {
            ArgumentOrRedirection::Argument(child) => child.accept(visitor, reversed),
            ArgumentOrRedirection::Redirection(child) => child.accept(visitor, reversed),
        }
    }
}

impl ArgumentOrRedirectionNode {
    /// \return whether this represents an argument.
    pub fn is_argument(&self) -> bool {
        matches!(*self.contents, ArgumentOrRedirection::Argument(_))
    }

    /// \return whether this represents a redirection
    pub fn is_redirection(&self) -> bool {
        matches!(*self.contents, ArgumentOrRedirection::Redirection(_))
    }

    /// \return this as an argument, assuming it wraps one.
    pub fn argument(&self) -> &ArgumentNode {
        match *self.contents {
            ArgumentOrRedirection::Argument(ref arg) => arg,
            _ => panic!("Is not an argument"),
        }
    }

    /// \return this as an argument, assuming it wraps one.
    pub fn redirection(&self) -> &RedirectionNode {
        match *self.contents {
            ArgumentOrRedirection::Redirection(ref arg) => arg,
            _ => panic!("Is not a redirection"),
        }
    }
}

pub enum Statement {
    NotStatement(NotStatementNode),
    BlockStatement(BlockStatementNode),
    IfStatement(IfStatementNode),
    SwitchStatement(SwitchStatementNode),
    DecoratedStatement(DecoratedStatementNode),
}

impl Acceptor for Statement {
    fn accept(&self, visitor: &mut dyn NodeVisitor, reversed: bool) {
        match self {
            Statement::NotStatement(node) => node.accept(visitor, reversed),
            Statement::BlockStatement(node) => node.accept(visitor, reversed),
            Statement::IfStatement(node) => node.accept(visitor, reversed),
            Statement::SwitchStatement(node) => node.accept(visitor, reversed),
            Statement::DecoratedStatement(node) => node.accept(visitor, reversed),
        }
    }
}

impl Statement {
    fn typ(&self) -> Type {
        match self {
            Statement::NotStatement(node) => node.typ(),
            Statement::BlockStatement(node) => node.typ(),
            Statement::IfStatement(node) => node.typ(),
            Statement::SwitchStatement(node) => node.typ(),
            Statement::DecoratedStatement(node) => node.typ(),
        }
    }
    // TODO don't return bool
    fn try_source_range_ffi(&self) -> bool {
        match self {
            Statement::NotStatement(node) => node.try_source_range().is_some(),
            Statement::BlockStatement(node) => node.try_source_range().is_some(),
            Statement::IfStatement(node) => node.try_source_range().is_some(),
            Statement::SwitchStatement(node) => node.try_source_range().is_some(),
            Statement::DecoratedStatement(node) => node.try_source_range().is_some(),
        }
    }
    fn source_range_ffi(&self) -> SourceRange {
        match self {
            Statement::NotStatement(node) => node.source_range(),
            Statement::BlockStatement(node) => node.source_range(),
            Statement::IfStatement(node) => node.source_range(),
            Statement::SwitchStatement(node) => node.source_range(),
            Statement::DecoratedStatement(node) => node.source_range(),
        }
    }
}

pub enum BlockStatementHeader {
    ForHeader(ForHeaderNode),
    WhileHeader(WhileHeaderNode),
    FunctionHeader(FunctionHeaderNode),
    BeginHeader(BeginHeaderNode),
}

impl Acceptor for BlockStatementHeader {
    fn accept(&self, visitor: &mut dyn NodeVisitor, reversed: bool) {
        match self {
            BlockStatementHeader::ForHeader(node) => node.accept(visitor, reversed),
            BlockStatementHeader::WhileHeader(node) => node.accept(visitor, reversed),
            BlockStatementHeader::FunctionHeader(node) => node.accept(visitor, reversed),
            BlockStatementHeader::BeginHeader(node) => node.accept(visitor, reversed),
        }
    }
}

impl BlockStatementHeader {
    fn typ(&self) -> Type {
        match self {
            BlockStatementHeader::ForHeader(node) => node.typ(),
            BlockStatementHeader::WhileHeader(node) => node.typ(),
            BlockStatementHeader::FunctionHeader(node) => node.typ(),
            BlockStatementHeader::BeginHeader(node) => node.typ(),
        }
    }
    fn try_source_range_ffi(&self) -> bool {
        match self {
            BlockStatementHeader::ForHeader(node) => node.try_source_range().is_some(),
            BlockStatementHeader::WhileHeader(node) => node.try_source_range().is_some(),
            BlockStatementHeader::FunctionHeader(node) => node.try_source_range().is_some(),
            BlockStatementHeader::BeginHeader(node) => node.try_source_range().is_some(),
        }
    }
    fn source_range_ffi(&self) -> SourceRange {
        match self {
            BlockStatementHeader::ForHeader(node) => node.source_range(),
            BlockStatementHeader::WhileHeader(node) => node.source_range(),
            BlockStatementHeader::FunctionHeader(node) => node.source_range(),
            BlockStatementHeader::BeginHeader(node) => node.source_range(),
        }
    }
}

// TODO prettify
nodes!(
    // A redirection has an operator like > or 2>, and a target like /dev/null or &1.
    // Note that pipes are not redirections.
    (
        RedirectionNode, redirection : none,
        branch,
        (oper: TokenRedirectionNode),
        (target: StringNode),
    );
    // A variable_assignment contains a source range like FOO=bar.
    (
        VariableAssignmentNode, variable_assignment : none,
        leaf,
    );
    (
        VariableAssignmentListNode, variable_assignment_list : none,
        list : VariableAssignmentNode,
    );
    // An argument or redirection holds either an argument or redirection.
    (
        ArgumentOrRedirectionNode, argument_or_redirection : none,
        leaf,
        (contents: Box<ArgumentOrRedirection>),
    );
    (
        ArgumentOrRedirectionListNode, argument_or_redirection_list : none,
        list : ArgumentOrRedirectionNode,
    );
    // A statement is a normal command, or an if / while / etc
    (
        StatementNode, statement : none,
        branch,
        (contents: Box<Statement>),
    );
    // A job is a non-empty list of statements, separated by pipes. (Non-empty is useful for cases
    // like if statements, where we require a command).
    (
        JobPipelineNode, job_pipeline : none,
        branch,
        // A (possibly empty) list of variable assignments.
        (time: Option<KeywordTimeNode>),
        // The statement.
        (statement: StatementNode),
        // Piped remainder.
        (continuation: JobContinuationListNode),
        // Maybe backgrounded.
        (bg: Option<TokenBackgroundNode>),
    );
    // A job_conjunction is a job followed by a && or || continuations.
    (
        JobConjunctionNode, job_conjunction : none,
        branch,
        // The job conjunction decorator.
        (decorator: Option<JobConjunctionDecoratorNode>),
        // The job itself.
        (job: JobPipelineNode),
        // The rest of the job conjunction, with && or ||s.
        (continuations: JobConjunctionContinuationListNode),
        // A terminating semicolon or newline.
        // This is marked optional because it may not be present, for example the command `echo foo` may
        // not have a terminating newline. It will only fail to be present if we ran out of tokens.
        (semi_nl: Option<SemiNlNode>),
    );
    (
        ForHeaderNode, for_header : none,
        branch,
        // 'for'
        (kw_for: KeywordForNode),
        // var_name
        (var_name: StringNode),
        // 'in'
        (kw_in: KeywordInNode),
        // list of arguments
        (args: ArgumentListNode),
        // newline or semicolon
        (semi_nl: SemiNlNode),
    );
    (
        WhileHeaderNode, while_header : none,
        branch,
        // 'while'
        (kw_while: KeywordWhileNode),
        (condition: JobConjunctionNode),
        (andor_tail: AndorJobListNode),
    );
    (
        FunctionHeaderNode, function_header : none,
        branch,
        (kw_function: KeywordFunctionNode),
        // functions require at least one argument.
        (first_arg: ArgumentNode),
        (args: ArgumentListNode),
        (semi_nl: SemiNlNode),
    );
    (
        BeginHeaderNode, begin_header : none,
        branch,
        (kw_begin: KeywordBeginNode),
        // Note that 'begin' does NOT require a semi or nl afterwards.
        // This is valid: begin echo hi; end
        (semi_nl: Option<SemiNlNode>),
    );
    (
        BlockStatementNode, block_statement : none,
        branch,
        // A header like for, while, etc.
        (header: Box<BlockStatementHeader>),
        // List of jobs in this block.
        (jobs: JobListNode),
        // The 'end' node.
        (end: KeywordEndNode),
        // Arguments and redirections associated with the block.
        (args_or_redirs: ArgumentOrRedirectionListNode),
    );
    (
        IfClauseNode, if_clause : none,
        branch,
        // The 'if' keyword.
        (kw_if: KeywordIfNode),
        // The 'if' condition.
        (condition: JobConjunctionNode),
        // 'and/or' tail.
        (andor_tail: AndorJobListNode),
        // The body to execute if the condition is true.
        (body: JobListNode),
    );
    (
        ElseifClauseNode, elseif_clause : none,
        branch,
        // The 'else' keyword.
        (kw_else: KeywordElseNode),
        // The 'if' clause following it.
        (if_clause: IfClauseNode),
    );
    (
        ElseifClauseListNode, elseif_clause_list : none,
        list : ElseifClauseNode,
    );
    (
        ElseClauseNode, else_clause : none,
        branch,
        // else ; body
        (kw_else: KeywordElseNode),
        (semi_nl: SemiNlNode),
        (body: JobListNode),
    );
    (
        IfStatementNode, if_statement : none,
        branch,
        // if part
        (if_clause: IfClauseNode),
        // else if list
        (elseif_clauses: ElseifClauseListNode),
        // else part
        (else_clause: Option<ElseClauseNode>),
        // literal end
        (end: KeywordEndNode),
        // block args / redirs
        (args_or_redirs: ArgumentOrRedirectionListNode),
    );
    (
        CaseItemNode, case_item : none,
        branch,
        // case <arguments> ; body
        (kw_case: KeywordCaseNode),
        (arguments: ArgumentListNode),
        (semi_nl: SemiNlNode),
        (body: JobListNode),
    );
    (
        SwitchStatementNode, switch_statement : none,
        branch,
        // switch <argument> ; body ; end args_redirs
        (argument: ArgumentNode),
        (semi_nl: SemiNlNode),
        (cases: CaseItemListNode),
        (end: KeywordEndNode),
        (args_or_redirs: ArgumentOrRedirectionListNode),
    );
    // A decorated_statement is a command with a list of arguments_or_redirections, possibly with
    // "builtin" or "command" or "exec"
    (
        DecoratedStatementNode, decorated_statement : none,
        branch,
        // An optional decoration (command, builtin, exec, etc).
        (opt_decoration: Option<DecoratedStatementDecoratorNode>),
        // Command to run.
        (command: StringNode),
        // Args and redirs
        (args_or_redirs: ArgumentOrRedirectionListNode),
    );
    // A not statement like `not true` or `! true`
    (
        NotStatementNode, not_statement : none,
        branch,
        // Keyword, either not or exclam.
        (kw: KeywordNotNode),
        (variables: VariableAssignmentListNode),
        (time: Option<KeywordTimeNode>),
        (contents: StatementNode),
    );
    (
        JobContinuationNode, job_continuation : none,
        branch,
        (pipe: TokenPipeNode),
        (newlines: MaybeNewlinesNode),
        (variables: VariableAssignmentListNode),
        (statement: StatementNode),
    );
    (
        JobContinuationListNode, job_continuation_list : none,
        list : JobContinuationNode,
    );
    (
        JobConjunctionContinuationNode, job_conjunction_continuation : none,
        branch,
        // The && or || token.
        (conjunction: TokenConjunctionNode),
        (newlines: MaybeNewlinesNode),
        // The job itself.
        (job: JobPipelineNode),
    );
    // An andor_job just wraps a job, but requires that the job have an 'and' or 'or' job_decorator.
    // Note this is only used for andor_job_list; jobs that are not part of an andor_job_list are not
    // instances of this.
    (
        AndorJobNode, andor_job : none,
        branch,
        (job: JobConjunctionNode),
    );
    (
        AndorJobListNode, andor_job_list : none,
        list : AndorJobNode,
    );
    // A freestanding_argument_list is equivalent to a normal argument list, except it may contain
    // TOK_END (newlines, and even semicolons, for historical reasons).
    // In practice the tok_ends are ignored by fish code so we do not bother to store them.
    (
        FreestandingArgumentListNode, freestanding_argument_list : none,
        branch,
        (arguments: ArgumentListNode),
    );
    (
        TokenConjunctionNode, token_conjunction : (Tokenish(ParseTokenType::andand, ParseTokenType::oror)),
        leaf,
    );
    (
        JobConjunctionContinuationListNode, job_conjunction_continuation_list : none,
        list : JobConjunctionContinuationNode,
    );
    (
        MaybeNewlinesNode, maybe_newlines : none,
        leaf,
    );
    (
        TokenPipeNode, token_pipe : (Tokenish(ParseTokenType::pipe)),
        leaf,
    );
    (
        KeywordNotNode, keyword_not : (Keywordish(ParseKeyword::kw_not, ParseKeyword::kw_builtin, ParseKeyword::kw_exclam)),
        leaf,
    );
    (
        DecoratedStatementDecoratorNode, decorated_statement_decorator : (Keywordish(ParseKeyword::kw_command, ParseKeyword::kw_builtin, ParseKeyword::kw_exec)),
        leaf,
    );
    (
        CaseItemListNode, case_item_list : none,
        list : CaseItemNode,
    );
    (
        KeywordEndNode, keyword_end : (Keywordish(ParseKeyword::kw_end)),
        leaf,
    );
    (
        KeywordCaseNode, keyword_case : (Keywordish(ParseKeyword::kw_case)),
        leaf,
    );
    (
        KeywordElseNode, keyword_else : (Keywordish(ParseKeyword::kw_else)),
        leaf,
    );
    (
        KeywordIfNode, keyword_if : (Keywordish(ParseKeyword::kw_if)),
        leaf,
    );
    (
        KeywordBeginNode, keyword_begin : (Keywordish(ParseKeyword::kw_begin)),
        leaf,
    );
    (
        KeywordFunctionNode, keyword_function : (Keywordish(ParseKeyword::kw_function)),
        leaf,
    );
    (
        KeywordWhileNode, keyword_while : (Keywordish(ParseKeyword::kw_while)),
        leaf,
    );
    (
        KeywordForNode, keyword_for : (Keywordish(ParseKeyword::kw_for)),
        leaf,
    );
    (
        KeywordInNode, keyword_in : (Keywordish(ParseKeyword::kw_in)),
        leaf,
    );
    (
        SemiNlNode, semi_nl : (Tokenish(ParseTokenType::end)),
        leaf,
    );
    (
        JobConjunctionDecoratorNode, job_conjunction_decorator : (Keywordish(ParseKeyword::kw_and, ParseKeyword::kw_or)),
        leaf,
    );
    (
        TokenBackgroundNode, token_background : (Tokenish(ParseTokenType::background)),
        leaf,
    );
    (
        KeywordTimeNode, keyword_time : none,
        leaf,
    );
    (
        TokenRedirectionNode, token_redirection : (Tokenish(ParseTokenType::redirection)),
        leaf,
    );
    (
        StringNode, string : (Tokenish(ParseTokenType::string)),
        leaf,
    );
    (
        ArgumentNode, argument : none,
        leaf,
    );
    (
        ArgumentListNode, argument_list : none,
        list : ArgumentNode,
    );
    (
        JobListNode, job_list : none,
        list : JobConjunctionNode,
    );
);

impl DecoratedStatementNode {
    fn decoration(&self) -> StatementDecoration {
        let Some(decorator) = &self.opt_decoration else {
            return StatementDecoration::none;
        };
        let decorator: &dyn Keywordish = decorator;
        match decorator.keyword() {
            ParseKeyword::kw_command => StatementDecoration::command,
            ParseKeyword::kw_builtin => StatementDecoration::builtin,
            ParseKeyword::kw_exec => StatementDecoration::exec,
            _ => panic!("Unexpected keyword in statement decoration"),
        }
    }
}

/**
 * A node visitor is like a field visitor, but adapted to only visit actual nodes, as const
 * references. It calls the visit() function of its visitor with a const reference to each node
 * found under a given node.
 *
 * Example:
 * struct MyNodeVisitor {
 *    template <typename Node>
 *    void visit(const Node &n) {...}
 * };
 */
struct NodeVisitation<T: NodeVisitor> {
    visitor: T,
    reverse: bool,
}

impl<T: NodeVisitor> NodeVisitation<T> {
    pub fn new(visitor: T, reverse: bool) -> Self {
        NodeVisitation { visitor, reverse }
    }
    // Visit the (direct) child nodes of a given node.
    fn accept_children_of<Node: Nodeish>(&mut self, n: &Node) {
        n.accept(&mut self.visitor, self.reverse)
    }
    fn accept_children_of2(&mut self, n: &dyn Nodeish) {
        n.accept(&mut self.visitor, self.reverse)
    }
}

// A way to visit nodes iteratively.
// This is pre-order. Each node is visited before its children.
// Example:
//    traversal_t tv(start);
//    while (const node_t *node = tv.next()) {...}
pub struct Traversal {
    stack: Vec<*const dyn Nodeish>,
}

impl Traversal {
    // Construct starting with a node
    pub fn new(n: *const dyn Nodeish) -> Self {
        Traversal { stack: vec![n] }
    }

    // \return the next node, or nullptr if exhausted.
    pub fn next(&mut self) -> Option<&dyn Nodeish> {
        let Some(node) = self.stack.pop() else {
            return None;
        };
        // We want to visit in reverse order so the first child ends up on top of the stack.
        NodeVisitation::new(self, true /* reverse */);
        // .accept_children_of(node);
        Some(unsafe { &*node })
    }
}

impl NodeVisitor for &mut Traversal {
    // Callback for node_visitation_t.
    fn visit(&mut self, node: *const dyn Nodeish) {
        self.stack.push(node)
    }
}

type SourceRangeList = Vec<SourceRange>;

/// Extra source ranges.
/// These are only generated if the corresponding flags are set.
#[derive(Default)]
pub struct Extras {
    /// Set of comments, sorted by offset.
    pub comments: SourceRangeList,

    /// Set of semicolons, sorted by offset.
    pub semis: SourceRangeList,

    /// Set of error ranges, sorted by offset.
    pub errors: SourceRangeList,
}

/// The ast type itself.
pub struct Ast {
    // The top node.
    // Its type depends on what was requested to parse.
    top: Box<*const dyn Nodeish>,
    /// Whether any errors were encountered during parsing.
    any_error: bool,
    /// Extra fields.
    extras: Extras,
}

impl Ast {
    /// Construct an ast by parsing \p src as a job list.
    /// The ast attempts to produce \p type as the result.
    /// \p type may only be JobListNode or FreestandingArgumentListNode.
    pub fn parse(src: &wstr, flags: ParseTreeFlags, errors: &mut ParseErrorList) -> Self {
        todo!()
    }
    /// Like parse(), but constructs a freestanding_argument_list.
    pub fn parse_argument_list(
        src: &wstr,
        flags: ParseTreeFlags,
        errors: &mut ParseErrorList,
    ) -> Self {
        todo!()
    }
    /// \return a traversal, allowing iteration over the nodes.
    pub fn walk(&self) -> Traversal {
        Traversal::new(*self.top)
    }
    /// \return the top node. This has the type requested in the 'parse' method.
    pub fn top(&self) -> &Box<*const dyn Nodeish> {
        &self.top
    }
    /// \return whether any errors were encountered during parsing.
    pub fn errored(&self) -> bool {
        todo!()
    }
    /// \return a textual representation of the tree.
    /// Pass the original source as \p orig.
    fn dump(&self, orig: &wstr) -> WString {
        todo!()
    }

    fn extras(&self) -> &Extras {
        &self.extras
    }
}

/// \return tokenizer flags corresponding to parse tree flags.
impl From<ParseTreeFlags> for TokFlags {
    fn from(flags: ParseTreeFlags) -> Self {
        let mut tok_flags = TokFlags(0);
        // Note we do not need to respect parse_flag_show_blank_lines, no clients are interested
        // in them.
        if flags & PARSE_FLAG_INCLUDE_COMMENTS {
            tok_flags |= TOK_SHOW_COMMENTS;
        }
        if flags & PARSE_FLAG_ACCEPT_INCOMPLETE_TOKENS {
            tok_flags |= TOK_ACCEPT_UNFINISHED;
        }
        if flags & PARSE_FLAG_CONTINUE_AFTER_ERROR {
            tok_flags |= TOK_CONTINUE_AFTER_ERROR
        }
        tok_flags
    }
}

fn is_keyword_char(c: char) -> bool {
    (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c == '\''
        || c == '"'
        || c == '\\'
        || c == '\n'
        || c == '!'
}

/// Given a token, returns the keyword it matches, or parse_keyword_t::none.
fn keyword_for_token(tok: TokenType, token: &wstr) -> ParseKeyword {
    /* Only strings can be keywords */
    if tok != TokenType::string {
        return ParseKeyword::none;
    }

    // If token is clean (which most are), we can compare it directly. Otherwise we have to expand
    // it. We only expand quotes, and we don't want to do expensive expansions like tilde
    // expansions. So we do our own "cleanliness" check; if we find a character not in our allowed
    // set we know it's not a keyword, and if we never find a quote we don't have to expand! Note
    // that this lowercase set could be shrunk to be just the characters that are in keywords.
    let result = ParseKeyword::none;
    let mut needs_expand = false;
    let mut all_chars_valid = true;
    for c in token.chars() {
        if !is_keyword_char(c) {
            all_chars_valid = false;
            break;
        }
        // If we encounter a quote, we need expansion.
        needs_expand = needs_expand || c == '"' || c == '\'' || c == '\\'
    }

    if all_chars_valid {}
    todo!()
}

pub use ast_ffi::Type;

#[cxx::bridge]
mod ast_ffi {
    extern "C++" {
        include!("tokenizer.h");
        include!("parse_constants.h");
    }
    extern "C++" {
        pub type ParseKeyword = crate::parse_constants::ParseKeyword;
        pub type SourceRange = crate::parse_constants::SourceRange;
        pub type ParseErrorList = crate::parse_constants::ParseErrorList;
        pub type StatementDecoration = crate::parse_constants::StatementDecoration;
    }

    pub enum Type {
        // TODO
        keyword_base_t,
        token_base_t,

        redirection,
        variable_assignment,
        variable_assignment_list,
        argument_or_redirection,
        argument_or_redirection_list,
        statement,
        job_pipeline,
        job_conjunction,
        for_header,
        while_header,
        function_header,
        begin_header,
        block_statement,
        if_clause,
        elseif_clause,
        elseif_clause_list,
        else_clause,
        if_statement,
        case_item,
        switch_statement,
        decorated_statement,
        not_statement,
        job_continuation,
        job_continuation_list,
        job_conjunction_continuation,
        andor_job,
        andor_job_list,
        freestanding_argument_list,
        token_conjunction,
        job_conjunction_continuation_list,
        maybe_newlines,
        token_pipe,
        keyword_not,
        decorated_statement_decorator,
        case_item_list,
        keyword_end,
        keyword_case,
        keyword_else,
        keyword_if,
        keyword_begin,
        keyword_function,
        keyword_while,
        keyword_for,
        keyword_in,
        semi_nl,
        job_conjunction_decorator,
        token_background,
        keyword_time,
        token_redirection,
        string,
        argument,
        argument_list,
        job_list,
    }

    extern "Rust" {
        type Ast;
        type NodeFFI;
        fn ast_parse_ffi(src: &CxxWString, flags: u8, errors: *mut ParseErrorList) -> Box<Ast>;
        fn ast_parse_argument_list_ffi(
            src: &CxxWString,
            flags: u8,
            errors: *mut ParseErrorList,
        ) -> Box<Ast>;
        fn errored(self: &Ast) -> bool;
        #[cxx_name = "top"]
        fn top_ffi(self: &Ast) -> Box<NodeFFI>;
    }

    #[rustfmt::skip]
    extern "Rust" {
        type Category;


        type BlockStatementHeader;
        type Statement;
        fn typ(self: &BlockStatementHeader) -> Type;
        fn typ(self: &Statement) -> Type;

        fn contents(self: &StatementNode) -> &Statement;

        type AndorJobListNode;
        type AndorJobNode;
        type ArgumentListNode;
        type ArgumentNode;
        type ArgumentOrRedirectionListNode;
        type ArgumentOrRedirectionNode;
        type BeginHeaderNode;
        type BlockStatementNode;
        type CaseItemListNode;
        type CaseItemNode;
        type DecoratedStatementDecoratorNode;
        type DecoratedStatementNode;
        type ElseClauseNode;
        type ElseifClauseListNode;
        type ElseifClauseNode;
        type ForHeaderNode;
        type FreestandingArgumentListNode;
        type FunctionHeaderNode;
        type IfClauseNode;
        type IfStatementNode;
        type JobConjunctionContinuationListNode;
        type JobConjunctionContinuationNode;
        type JobConjunctionDecoratorNode;
        type JobConjunctionNode;
        type JobContinuationListNode;
        type JobContinuationNode;
        type JobListNode;
        type JobPipelineNode;
        type KeywordBeginNode;
        type KeywordCaseNode;
        type KeywordElseNode;
        type KeywordEndNode;
        type KeywordForNode;
        type KeywordFunctionNode;
        type KeywordIfNode;
        type KeywordInNode;
        type KeywordNotNode;
        type KeywordTimeNode;
        type KeywordWhileNode;
        type MaybeNewlinesNode;
        type NotStatementNode;
        type RedirectionNode;
        type SemiNlNode;
        type StatementNode;
        type StringNode;
        type SwitchStatementNode;
        type TokenBackgroundNode;
        type TokenConjunctionNode;
        type TokenPipeNode;
        type TokenRedirectionNode;
        type VariableAssignmentListNode;
        type VariableAssignmentNode;
        type WhileHeaderNode;

        fn has_value(self: &NodeFFI) -> bool;

        #[cxx_name = "parent"] fn parent_ffi(self: &NodeFFI) -> Box<NodeFFI>;
        #[cxx_name = "parent"] fn parent_ffi(self: &JobConjunctionNode) -> Box<NodeFFI>;
        #[cxx_name = "parent"] fn parent_ffi(self: &JobPipelineNode) -> Box<NodeFFI>;
        #[cxx_name = "parent"] fn parent_ffi(self: &DecoratedStatementNode) -> Box<NodeFFI>;

        fn count(self: &ArgumentListNode) -> usize;
        fn empty(self: &ArgumentListNode) -> bool;
        fn at(self: &ArgumentListNode, i: usize) -> *const ArgumentNode;

        fn count(self: &ArgumentOrRedirectionListNode) -> usize;
        fn empty(self: &ArgumentOrRedirectionListNode) -> bool;
        fn at(self: &ArgumentOrRedirectionListNode, i: usize) -> *const ArgumentOrRedirectionNode;

        fn count(self: &JobListNode) -> usize;
        fn empty(self: &JobListNode) -> bool;
        fn at(self: &JobListNode, i: usize) -> *const JobConjunctionNode;

        fn count(self: &JobContinuationListNode) -> usize;
        fn empty(self: &JobContinuationListNode) -> bool;
        fn at(self: &JobContinuationListNode, i: usize) -> *const JobContinuationNode;

        fn kw(self: &JobConjunctionDecoratorNode) -> ParseKeyword;
        fn decoration(self: &DecoratedStatementNode) -> StatementDecoration;

        fn is_argument(self: &ArgumentOrRedirectionNode) -> bool;
        fn argument(self: &ArgumentOrRedirectionNode) -> &ArgumentNode;
        fn is_redirection(self: &ArgumentOrRedirectionNode) -> bool;
        fn redirection(self: &ArgumentOrRedirectionNode) -> &RedirectionNode;

        fn oper(self: &RedirectionNode) -> &TokenRedirectionNode;
        fn target(self: &RedirectionNode) -> &StringNode;
        fn argument_ffi(self: &ArgumentOrRedirectionNode) -> &ArgumentNode;
        fn redirection_ffi(self: &ArgumentOrRedirectionNode) -> &RedirectionNode;
        fn has_time(self: &JobPipelineNode) -> bool;
        fn time(self: &JobPipelineNode) -> &KeywordTimeNode;
        fn statement(self: &JobPipelineNode) -> &StatementNode;
        fn continuation(self: &JobPipelineNode) -> &JobContinuationListNode;
        fn has_bg(self: &JobPipelineNode) -> bool;
        fn bg(self: &JobPipelineNode) -> &TokenBackgroundNode;
        fn has_decorator(self: &JobConjunctionNode) -> bool;
        fn decorator(self: &JobConjunctionNode) -> &JobConjunctionDecoratorNode;
        fn job(self: &JobConjunctionNode) -> &JobPipelineNode;
        fn continuations(self: &JobConjunctionNode) -> &JobConjunctionContinuationListNode;
        fn has_semi_nl(self: &JobConjunctionNode) -> bool;
        fn semi_nl(self: &JobConjunctionNode) -> &SemiNlNode;
        fn var_name(self: &ForHeaderNode) -> &StringNode;
        fn args(self: &ForHeaderNode) -> &ArgumentListNode;
        fn semi_nl(self: &ForHeaderNode) -> &SemiNlNode;
        fn condition(self: &WhileHeaderNode) -> &JobConjunctionNode;
        fn andor_tail(self: &WhileHeaderNode) -> &AndorJobListNode;
        fn first_arg(self: &FunctionHeaderNode) -> &ArgumentNode;
        fn args(self: &FunctionHeaderNode) -> &ArgumentListNode;
        fn semi_nl(self: &FunctionHeaderNode) -> &SemiNlNode;
        fn has_semi_nl(self: &BeginHeaderNode) -> bool;
        fn semi_nl(self: &BeginHeaderNode) -> &SemiNlNode;
        fn header(self: &BlockStatementNode) -> &BlockStatementHeader;
        fn jobs(self: &BlockStatementNode) -> &JobListNode;
        fn args_or_redirs(self: &BlockStatementNode) -> &ArgumentOrRedirectionListNode;
        fn condition(self: &IfClauseNode) -> &JobConjunctionNode;
        fn andor_tail(self: &IfClauseNode) -> &AndorJobListNode;
        fn body(self: &IfClauseNode) -> &JobListNode;
        fn if_clause(self: &ElseifClauseNode) -> &IfClauseNode;
        fn semi_nl(self: &ElseClauseNode) -> &SemiNlNode;
        fn body(self: &ElseClauseNode) -> &JobListNode;
        fn if_clause(self: &IfStatementNode) -> &IfClauseNode;
        fn elseif_clauses(self: &IfStatementNode) -> &ElseifClauseListNode;
        fn has_else_clause(self: &IfStatementNode) -> bool;
        fn else_clause(self: &IfStatementNode) -> &ElseClauseNode;
        fn args_or_redirs(self: &IfStatementNode) -> &ArgumentOrRedirectionListNode;
        fn arguments(self: &CaseItemNode) -> &ArgumentListNode;
        fn semi_nl(self: &CaseItemNode) -> &SemiNlNode;
        fn body(self: &CaseItemNode) -> &JobListNode;
        fn argument(self: &SwitchStatementNode) -> &ArgumentNode;
        fn semi_nl(self: &SwitchStatementNode) -> &SemiNlNode;
        fn cases(self: &SwitchStatementNode) -> &CaseItemListNode;
        fn args_or_redirs(self: &SwitchStatementNode) -> &ArgumentOrRedirectionListNode;
        fn has_opt_decoration(self: &DecoratedStatementNode) -> bool;
        fn opt_decoration(self: &DecoratedStatementNode) -> &DecoratedStatementDecoratorNode;
        fn command(self: &DecoratedStatementNode) -> &StringNode;
        fn args_or_redirs(self: &DecoratedStatementNode) -> &ArgumentOrRedirectionListNode;
        fn variables(self: &NotStatementNode) -> &VariableAssignmentListNode;
        fn has_time(self: &NotStatementNode) -> bool;
        fn time(self: &NotStatementNode) -> &KeywordTimeNode;
        fn contents(self: &NotStatementNode) -> &StatementNode;
        fn pipe(self: &JobContinuationNode) -> &TokenPipeNode;
        fn newlines(self: &JobContinuationNode) -> &MaybeNewlinesNode;
        fn variables(self: &JobContinuationNode) -> &VariableAssignmentListNode;
        fn statement(self: &JobContinuationNode) -> &StatementNode;
        fn conjunction(self: &JobConjunctionContinuationNode) -> &TokenConjunctionNode;
        fn newlines(self: &JobConjunctionContinuationNode) -> &MaybeNewlinesNode;
        fn job(self: &JobConjunctionContinuationNode) -> &JobPipelineNode;
        fn job(self: &AndorJobNode) -> &JobConjunctionNode;
        fn arguments(self: &FreestandingArgumentListNode) -> &ArgumentListNode;

        fn end(self: &BlockStatementNode) -> &KeywordEndNode;

        #[cxx_name="source"] fn source_ffi(self: &NodeFFI, orig: &CxxWString) -> UniquePtr<CxxWString>;
        #[cxx_name="source"] fn source_ffi(self: &ArgumentNode, orig: &CxxWString) -> UniquePtr<CxxWString>;
        #[cxx_name="source"] fn source_ffi(self: &VariableAssignmentNode, orig: &CxxWString) -> UniquePtr<CxxWString>;
        #[cxx_name="source"] fn source_ffi(self: &StringNode, orig: &CxxWString) -> UniquePtr<CxxWString>;

        #[cxx_name = "try_source_range"] fn try_source_range_ffi(self: &NodeFFI) -> bool;
        #[cxx_name = "try_source_range"] fn try_source_range_ffi(self: &ArgumentNode) -> bool;
        #[cxx_name = "try_source_range"] fn try_source_range_ffi(self: &JobPipelineNode) -> bool;
        #[cxx_name = "try_source_range"] fn try_source_range_ffi(self: &BlockStatementHeader) -> bool;
        #[cxx_name = "try_source_range"] fn try_source_range_ffi(self: &Statement) -> bool;
        #[cxx_name = "try_source_range"] fn try_source_range_ffi(self: &BlockStatementNode) -> bool;

        #[cxx_name = "source_range"] fn source_range_ffi(self: &JobConjunctionDecoratorNode) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &DecoratedStatementNode) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &NodeFFI) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &ArgumentNode) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &JobPipelineNode) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &StringNode) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &BlockStatementHeader) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &Statement) -> SourceRange;
        #[cxx_name = "source_range"] fn source_range_ffi(self: &BlockStatementNode) -> SourceRange;
    }

    #[rustfmt::skip]
    extern "Rust" {
        fn try_as_not_statement(self: &Statement) -> *const NotStatementNode;
        fn try_as_block_statement(self: &Statement) -> *const BlockStatementNode;
        fn try_as_if_statement(self: &Statement) -> *const IfStatementNode;
        fn try_as_switch_statement(self: &Statement) -> *const SwitchStatementNode;
        fn try_as_decorated_statement(self: &Statement) -> *const DecoratedStatementNode;
    }

    #[rustfmt::skip]
    extern "Rust" {
        fn try_as_argument(self: &NodeFFI) -> *const ArgumentNode;
        fn try_as_begin_header(self: &NodeFFI) -> *const BeginHeaderNode;
        fn try_as_block_statement(self: &NodeFFI) -> *const BlockStatementNode;
        fn try_as_decorated_statement(self: &NodeFFI) -> *const DecoratedStatementNode;
        fn try_as_for_header(self: &NodeFFI) -> *const ForHeaderNode;
        fn try_as_function_header(self: &NodeFFI) -> *const FunctionHeaderNode;
        fn try_as_if_clause(self: &NodeFFI) -> *const IfClauseNode;
        fn try_as_if_statement(self: &NodeFFI) -> *const IfStatementNode;
        fn try_as_job_conjunction(self: &NodeFFI) -> *const JobConjunctionNode;
        fn try_as_job_conjunction_continuation(self: &NodeFFI) -> *const JobConjunctionContinuationNode;
        fn try_as_job_continuation(self: &NodeFFI) -> *const JobContinuationNode;
        fn try_as_job_list(self: &NodeFFI) -> *const JobListNode;
        fn try_as_job_pipeline(self: &NodeFFI) -> *const JobPipelineNode;
        fn try_as_not_statement(self: &NodeFFI) -> *const NotStatementNode;
        fn try_as_switch_statement(self: &NodeFFI) -> *const SwitchStatementNode;
        fn try_as_while_header(self: &NodeFFI) -> *const WhileHeaderNode;
    }

    #[rustfmt::skip]
    extern "Rust" {
        fn as_argument(self: &NodeFFI) -> *const ArgumentNode;
        fn as_begin_header(self: &NodeFFI) -> *const BeginHeaderNode;
        fn as_block_statement(self: &NodeFFI) -> *const BlockStatementNode;
        fn as_decorated_statement(self: &NodeFFI) -> *const DecoratedStatementNode;
        fn as_for_header(self: &NodeFFI) -> *const ForHeaderNode;
        fn as_freestanding_argument_list(self: &NodeFFI) -> *const FreestandingArgumentListNode;
        fn as_function_header(self: &NodeFFI) -> *const FunctionHeaderNode;
        fn as_if_statement(self: &NodeFFI) -> *const IfStatementNode;
        fn as_job_conjunction_continuation(self: &NodeFFI) -> *const JobConjunctionContinuationNode;
        fn as_job_continuation(self: &NodeFFI) -> *const JobContinuationNode;
        fn as_job_list(self: &NodeFFI) -> *const JobListNode;
        fn as_not_statement(self: &NodeFFI) -> *const NotStatementNode;
        fn as_redirection(self: &NodeFFI) -> *const RedirectionNode;
        fn as_statement(self: &NodeFFI) -> *const StatementNode;
        fn as_switch_statement(self: &NodeFFI) -> *const SwitchStatementNode;
        fn as_while_header(self: &NodeFFI) -> *const WhileHeaderNode;
    }

    #[rustfmt::skip]
    extern "Rust" {
        fn ptr(self: &ArgumentNode) -> Box<NodeFFI>;
        fn ptr(self: &BeginHeaderNode) -> Box<NodeFFI>;
        fn ptr(self: &BlockStatementNode) -> Box<NodeFFI>;
        fn ptr(self: &DecoratedStatementNode) -> Box<NodeFFI>;
        fn ptr(self: &ForHeaderNode) -> Box<NodeFFI>;
        fn ptr(self: &FunctionHeaderNode) -> Box<NodeFFI>;
        fn ptr(self: &IfClauseNode) -> Box<NodeFFI>;
        fn ptr(self: &IfStatementNode) -> Box<NodeFFI>;
        fn ptr(self: &JobConjunctionNode) -> Box<NodeFFI>;
        fn ptr(self: &JobConjunctionContinuationNode) -> Box<NodeFFI>;
        fn ptr(self: &JobContinuationNode) -> Box<NodeFFI>;
        fn ptr(self: &JobListNode) -> Box<NodeFFI>;
        fn ptr(self: &JobPipelineNode) -> Box<NodeFFI>;
        fn ptr(self: &NotStatementNode) -> Box<NodeFFI>;
        fn ptr(self: &SwitchStatementNode) -> Box<NodeFFI>;
        fn ptr(self: &WhileHeaderNode) -> Box<NodeFFI>;
        fn ptr(self: &SemiNlNode) -> Box<NodeFFI>;
    }

    #[rustfmt::skip]
    extern "Rust" {
        fn range(self: &VariableAssignmentNode) -> SourceRange;
        fn range(self: &ArgumentOrRedirectionNode) -> SourceRange;
        fn range(self: &TokenConjunctionNode) -> SourceRange;
        fn range(self: &MaybeNewlinesNode) -> SourceRange;
        fn range(self: &TokenPipeNode) -> SourceRange;
        fn range(self: &KeywordNotNode) -> SourceRange;
        fn range(self: &DecoratedStatementDecoratorNode) -> SourceRange;
        fn range(self: &KeywordEndNode) -> SourceRange;
        fn range(self: &KeywordCaseNode) -> SourceRange;
        fn range(self: &KeywordElseNode) -> SourceRange;
        fn range(self: &KeywordIfNode) -> SourceRange;
        fn range(self: &KeywordBeginNode) -> SourceRange;
        fn range(self: &KeywordFunctionNode) -> SourceRange;
        fn range(self: &KeywordWhileNode) -> SourceRange;
        fn range(self: &KeywordForNode) -> SourceRange;
        fn range(self: &KeywordInNode) -> SourceRange;
        fn range(self: &SemiNlNode) -> SourceRange;
        fn range(self: &JobConjunctionDecoratorNode) -> SourceRange;
        fn range(self: &TokenBackgroundNode) -> SourceRange;
        fn range(self: &KeywordTimeNode) -> SourceRange;
        fn range(self: &TokenRedirectionNode) -> SourceRange;
        fn range(self: &StringNode) -> SourceRange;
        fn range(self: &ArgumentNode) -> SourceRange;
    }
}

unsafe impl ExternType for Ast {
    type Id = type_id!("Ast");
    type Kind = cxx::kind::Opaque;
}

/// Bogus default implementation, only used for FFI.
impl Default for Ast {
    fn default() -> Self {
        Self {
            top: Box::new(&StringNode {
                parent: None,
                children: (),
                parse_token_type: ParseTokenType::invalid,
                keyword: (),
                unsourced: false,
                range: SourceRange::new(0, 0),
            }),
            any_error: false,
            extras: Extras::default(),
        }
    }
}
impl Ast {
    fn top_ffi(self: &Ast) -> Box<NodeFFI> {
        todo!()
        // Box::new(NodeFFI(if self.top.is_null() {
        //     None
        // } else {
        //     Some(&*self.top)
        // }))
    }
}

fn ast_parse_ffi(src: &CxxWString, flags: u8, errors: *mut ParseErrorList) -> Box<Ast> {
    // ParseTreeFlags
    todo!()
}

fn ast_parse_argument_list_ffi(
    src: &CxxWString,
    flags: u8,
    errors: *mut ParseErrorList,
) -> Box<Ast> {
    // ParseTreeFlags
    todo!()
}

impl StatementNode {
    fn contents(&self) -> &Statement {
        &self.contents
    }
}

struct NodeFFI(Option<*const dyn Nodeish>);

impl NodeFFI {
    fn new(node: *const dyn Nodeish) -> Self {
        NodeFFI(Some(node))
    }
    fn deref(&self) -> &dyn Nodeish {
        unsafe { &*self.0.unwrap() }
    }
}

impl NodeFFI {
    fn source_ffi(&self, orig: &CxxWString) -> UniquePtr<CxxWString> {
        self.deref().source(&orig.from_ffi()).to_ffi()
    }
}
impl ArgumentNode {
    fn source_ffi(&self, orig: &CxxWString) -> UniquePtr<CxxWString> {
        self.source(&orig.from_ffi()).to_ffi()
    }
}
impl VariableAssignmentNode {
    fn source_ffi(&self, orig: &CxxWString) -> UniquePtr<CxxWString> {
        self.source(&orig.from_ffi()).to_ffi()
    }
}
impl StringNode {
    fn source_ffi(&self, orig: &CxxWString) -> UniquePtr<CxxWString> {
        self.source(&orig.from_ffi()).to_ffi()
    }
}

impl NodeFFI {
    fn has_value(&self) -> bool {
        self.0.is_some()
    }
    fn parent_ffi(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI(self.deref().parent()))
    }
    fn try_source_range_ffi(&self) -> bool {
        self.deref().try_source_range().is_some()
    }
    fn source_range_ffi(&self) -> SourceRange {
        self.deref().source_range()
    }
}

impl JobConjunctionNode {
    fn parent_ffi(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI(self.parent()))
    }
}
impl JobPipelineNode {
    fn parent_ffi(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI(self.parent()))
    }
}

impl ArgumentListNode {
    fn count(&self) -> usize {
        self.children.len()
    }
    fn empty(&self) -> bool {
        self.children.is_empty()
    }
    fn at(&self, i: usize) -> *const ArgumentNode {
        &self.children[i]
    }
}

impl ArgumentOrRedirectionListNode {
    fn count(&self) -> usize {
        self.children.len()
    }
    fn empty(&self) -> bool {
        self.children.is_empty()
    }
    fn at(&self, i: usize) -> *const ArgumentOrRedirectionNode {
        &self.children[i]
    }
}

impl JobListNode {
    fn count(&self) -> usize {
        self.children.len()
    }
    fn empty(&self) -> bool {
        self.children.is_empty()
    }
    fn at(&self, i: usize) -> *const JobConjunctionNode {
        &self.children[i]
    }
}

impl JobContinuationListNode {
    fn count(&self) -> usize {
        self.children.len()
    }
    fn empty(&self) -> bool {
        self.children.is_empty()
    }
    fn at(&self, i: usize) -> *const JobContinuationNode {
        &self.children[i]
    }
}
impl JobConjunctionDecoratorNode {
    fn kw(&self) -> ParseKeyword {
        self.keyword()
    }
    fn source_range_ffi(&self) -> SourceRange {
        self.source_range()
    }
}
impl DecoratedStatementNode {
    fn parent_ffi(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI(self.parent()))
    }
    fn source_range_ffi(&self) -> SourceRange {
        self.source_range()
    }
}

impl ArgumentNode {
    fn try_source_range_ffi(&self) -> bool {
        self.try_source_range().is_some()
    }
    fn source_range_ffi(&self) -> SourceRange {
        self.source_range()
    }
}

impl JobPipelineNode {
    fn try_source_range_ffi(&self) -> bool {
        self.try_source_range().is_some()
    }
    fn source_range_ffi(&self) -> SourceRange {
        self.source_range()
    }
}

impl StringNode {
    fn source_range_ffi(&self) -> SourceRange {
        self.source_range()
    }
}

impl BlockStatementNode {
    fn try_source_range_ffi(&self) -> bool {
        self.try_source_range().is_some()
    }
    fn source_range_ffi(&self) -> SourceRange {
        self.source_range()
    }
}

impl RedirectionNode {
    fn oper(&self) -> &TokenRedirectionNode {
        &self.oper
    }
}
impl RedirectionNode {
    fn target(&self) -> &StringNode {
        &self.target
    }
}
impl ArgumentOrRedirectionNode {
    fn argument_ffi(&self) -> &ArgumentNode {
        &self.argument()
    }
}
impl ArgumentOrRedirectionNode {
    fn redirection_ffi(&self) -> &RedirectionNode {
        &self.redirection()
    }
}
impl JobPipelineNode {
    fn has_time(&self) -> bool {
        self.time.is_some()
    }
}
impl JobPipelineNode {
    fn time(&self) -> &KeywordTimeNode {
        &self.time.as_ref().unwrap()
    }
}
impl JobPipelineNode {
    fn statement(&self) -> &StatementNode {
        &self.statement
    }
}
impl JobPipelineNode {
    fn continuation(&self) -> &JobContinuationListNode {
        &self.continuation
    }
}
impl JobPipelineNode {
    fn has_bg(&self) -> bool {
        self.bg.is_some()
    }
}
impl JobPipelineNode {
    fn bg(&self) -> &TokenBackgroundNode {
        &self.bg.as_ref().unwrap()
    }
}
impl JobConjunctionNode {
    fn has_decorator(&self) -> bool {
        self.decorator.is_some()
    }
}
impl JobConjunctionNode {
    fn decorator(&self) -> &JobConjunctionDecoratorNode {
        &self.decorator.as_ref().unwrap()
    }
}
impl JobConjunctionNode {
    fn job(&self) -> &JobPipelineNode {
        &self.job
    }
}
impl JobConjunctionNode {
    fn continuations(&self) -> &JobConjunctionContinuationListNode {
        &self.continuations
    }
}
impl JobConjunctionNode {
    fn has_semi_nl(&self) -> bool {
        self.semi_nl.is_some()
    }
}
impl JobConjunctionNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl.as_ref().unwrap()
    }
}
impl ForHeaderNode {
    fn var_name(&self) -> &StringNode {
        &self.var_name
    }
}
impl ForHeaderNode {
    fn args(&self) -> &ArgumentListNode {
        &self.args
    }
}
impl ForHeaderNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl
    }
}
impl WhileHeaderNode {
    fn condition(&self) -> &JobConjunctionNode {
        &self.condition
    }
}
impl WhileHeaderNode {
    fn andor_tail(&self) -> &AndorJobListNode {
        &self.andor_tail
    }
}
impl FunctionHeaderNode {
    fn first_arg(&self) -> &ArgumentNode {
        &self.first_arg
    }
}
impl FunctionHeaderNode {
    fn args(&self) -> &ArgumentListNode {
        &self.args
    }
}
impl FunctionHeaderNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl
    }
}
impl BeginHeaderNode {
    fn has_semi_nl(&self) -> bool {
        self.semi_nl.is_some()
    }
}
impl BeginHeaderNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl.as_ref().unwrap()
    }
}
impl BlockStatementNode {
    fn header(&self) -> &BlockStatementHeader {
        &self.header
    }
}
impl BlockStatementNode {
    fn jobs(&self) -> &JobListNode {
        &self.jobs
    }
}
impl BlockStatementNode {
    fn args_or_redirs(&self) -> &ArgumentOrRedirectionListNode {
        &self.args_or_redirs
    }
}
impl IfClauseNode {
    fn condition(&self) -> &JobConjunctionNode {
        &self.condition
    }
}
impl IfClauseNode {
    fn andor_tail(&self) -> &AndorJobListNode {
        &self.andor_tail
    }
}
impl IfClauseNode {
    fn body(&self) -> &JobListNode {
        &self.body
    }
}
impl ElseifClauseNode {
    fn if_clause(&self) -> &IfClauseNode {
        &self.if_clause
    }
}
impl ElseClauseNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl
    }
}
impl ElseClauseNode {
    fn body(&self) -> &JobListNode {
        &self.body
    }
}
impl IfStatementNode {
    fn if_clause(&self) -> &IfClauseNode {
        &self.if_clause
    }
}
impl IfStatementNode {
    fn elseif_clauses(&self) -> &ElseifClauseListNode {
        &self.elseif_clauses
    }
}
impl IfStatementNode {
    fn has_else_clause(&self) -> bool {
        self.else_clause.is_some()
    }
}
impl IfStatementNode {
    fn else_clause(&self) -> &ElseClauseNode {
        &self.else_clause.as_ref().unwrap()
    }
}
impl IfStatementNode {
    fn args_or_redirs(&self) -> &ArgumentOrRedirectionListNode {
        &self.args_or_redirs
    }
}
impl CaseItemNode {
    fn arguments(&self) -> &ArgumentListNode {
        &self.arguments
    }
}
impl CaseItemNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl
    }
}
impl CaseItemNode {
    fn body(&self) -> &JobListNode {
        &self.body
    }
}
impl SwitchStatementNode {
    fn argument(&self) -> &ArgumentNode {
        &self.argument
    }
}
impl SwitchStatementNode {
    fn semi_nl(&self) -> &SemiNlNode {
        &self.semi_nl
    }
}
impl SwitchStatementNode {
    fn cases(&self) -> &CaseItemListNode {
        &self.cases
    }
}
impl SwitchStatementNode {
    fn args_or_redirs(&self) -> &ArgumentOrRedirectionListNode {
        &self.args_or_redirs
    }
}
impl DecoratedStatementNode {
    fn has_opt_decoration(&self) -> bool {
        self.opt_decoration.is_some()
    }
}
impl DecoratedStatementNode {
    fn opt_decoration(&self) -> &DecoratedStatementDecoratorNode {
        &self.opt_decoration.as_ref().unwrap()
    }
}
impl DecoratedStatementNode {
    fn command(&self) -> &StringNode {
        &self.command
    }
}
impl DecoratedStatementNode {
    fn args_or_redirs(&self) -> &ArgumentOrRedirectionListNode {
        &self.args_or_redirs
    }
}
impl NotStatementNode {
    fn variables(&self) -> &VariableAssignmentListNode {
        &self.variables
    }
}
impl NotStatementNode {
    fn has_time(&self) -> bool {
        self.time.is_some()
    }
}
impl NotStatementNode {
    fn time(&self) -> &KeywordTimeNode {
        &self.time.as_ref().unwrap()
    }
}
impl NotStatementNode {
    fn contents(&self) -> &StatementNode {
        &self.contents
    }
}
impl JobContinuationNode {
    fn pipe(&self) -> &TokenPipeNode {
        &self.pipe
    }
}
impl JobContinuationNode {
    fn newlines(&self) -> &MaybeNewlinesNode {
        &self.newlines
    }
}
impl JobContinuationNode {
    fn variables(&self) -> &VariableAssignmentListNode {
        &self.variables
    }
}
impl JobContinuationNode {
    fn statement(&self) -> &StatementNode {
        &self.statement
    }
}
impl JobConjunctionContinuationNode {
    fn conjunction(&self) -> &TokenConjunctionNode {
        &self.conjunction
    }
}
impl JobConjunctionContinuationNode {
    fn newlines(&self) -> &MaybeNewlinesNode {
        &self.newlines
    }
}
impl JobConjunctionContinuationNode {
    fn job(&self) -> &JobPipelineNode {
        &self.job
    }
}
impl AndorJobNode {
    fn job(&self) -> &JobConjunctionNode {
        &self.job
    }
}
impl FreestandingArgumentListNode {
    fn arguments(&self) -> &ArgumentListNode {
        &self.arguments
    }
}

impl BlockStatementNode {
    fn end(&self) -> &KeywordEndNode {
        &self.end
    }
}

impl Statement {
    fn try_as_not_statement(&self) -> *const NotStatementNode {
        match self {
            Statement::NotStatement(node) => node,
            _ => std::ptr::null(),
        }
    }
    fn try_as_block_statement(&self) -> *const BlockStatementNode {
        match self {
            Statement::BlockStatement(node) => node,
            _ => std::ptr::null(),
        }
    }
    fn try_as_if_statement(&self) -> *const IfStatementNode {
        match self {
            Statement::IfStatement(node) => node,
            _ => std::ptr::null(),
        }
    }
    fn try_as_switch_statement(&self) -> *const SwitchStatementNode {
        match self {
            Statement::SwitchStatement(node) => node,
            _ => std::ptr::null(),
        }
    }
    fn try_as_decorated_statement(&self) -> *const DecoratedStatementNode {
        match self {
            Statement::DecoratedStatement(node) => node,
            _ => std::ptr::null(),
        }
    }
}

impl NodeFFI {
    fn try_as_argument(&self) -> *const ArgumentNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_begin_header(&self) -> *const BeginHeaderNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_block_statement(&self) -> *const BlockStatementNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_decorated_statement(&self) -> *const DecoratedStatementNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_for_header(&self) -> *const ForHeaderNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_function_header(&self) -> *const FunctionHeaderNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_if_clause(&self) -> *const IfClauseNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_if_statement(&self) -> *const IfStatementNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_job_conjunction(&self) -> *const JobConjunctionNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_job_conjunction_continuation(&self) -> *const JobConjunctionContinuationNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_job_continuation(&self) -> *const JobContinuationNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_job_list(&self) -> *const JobListNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_job_pipeline(&self) -> *const JobPipelineNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_not_statement(&self) -> *const NotStatementNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_switch_statement(&self) -> *const SwitchStatementNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
    fn try_as_while_header(&self) -> *const WhileHeaderNode {
        match self.deref().as_any().downcast_ref() {
            Some(node) => node,
            None => std::ptr::null(),
        }
    }
}

#[rustfmt::skip]
impl NodeFFI {
        fn as_argument(self: &NodeFFI) -> *const ArgumentNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_begin_header(self: &NodeFFI) -> *const BeginHeaderNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_block_statement(self: &NodeFFI) -> *const BlockStatementNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_decorated_statement(self: &NodeFFI) -> *const DecoratedStatementNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_for_header(self: &NodeFFI) -> *const ForHeaderNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_freestanding_argument_list(self: &NodeFFI) -> *const FreestandingArgumentListNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_function_header(self: &NodeFFI) -> *const FunctionHeaderNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_if_statement(self: &NodeFFI) -> *const IfStatementNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_job_conjunction_continuation(self: &NodeFFI) -> *const JobConjunctionContinuationNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_job_continuation(self: &NodeFFI) -> *const JobContinuationNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_job_list(self: &NodeFFI) -> *const JobListNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_not_statement(self: &NodeFFI) -> *const NotStatementNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_redirection(self: &NodeFFI) -> *const RedirectionNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_statement(self: &NodeFFI) -> *const StatementNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_switch_statement(self: &NodeFFI) -> *const SwitchStatementNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
        fn as_while_header(self: &NodeFFI) -> *const WhileHeaderNode {
            unsafe {&*self.0.unwrap() }.as_any().downcast_ref().unwrap()
        }
}

impl ArgumentNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl BeginHeaderNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl BlockStatementNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl DecoratedStatementNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl ForHeaderNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl FunctionHeaderNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl IfClauseNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl IfStatementNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl JobConjunctionNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl JobConjunctionContinuationNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl JobContinuationNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl JobListNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl JobPipelineNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl NotStatementNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl SwitchStatementNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl WhileHeaderNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}
impl SemiNlNode {
    fn ptr(&self) -> Box<NodeFFI> {
        Box::new(NodeFFI::new(self))
    }
}

impl VariableAssignmentNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl ArgumentOrRedirectionNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl TokenConjunctionNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl MaybeNewlinesNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl TokenPipeNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordNotNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl DecoratedStatementDecoratorNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordEndNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordCaseNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordElseNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordIfNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordBeginNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordFunctionNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordWhileNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordForNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordInNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl SemiNlNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl JobConjunctionDecoratorNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl TokenBackgroundNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl KeywordTimeNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl TokenRedirectionNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl StringNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
impl ArgumentNode {
    fn range(&self) -> SourceRange {
        self.range
    }
}
