/**
 * @Author: turk
 * @Description: Sintaksni analizator.
 */

package compiler.parser;

import static compiler.lexer.TokenType.*;
import static common.RequireNonNull.requireNonNull;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import common.Report;
import compiler.lexer.Position;
import compiler.lexer.Symbol;
import compiler.lexer.TokenType;
import compiler.lexer.Position.Location;
import compiler.parser.ast.Ast;
import compiler.parser.ast.def.Defs;
import compiler.parser.ast.def.FunDef;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.Binary;
import compiler.parser.ast.expr.Block;
import compiler.parser.ast.expr.Call;
import compiler.parser.ast.expr.Binary.Operator;
import compiler.parser.ast.expr.Expr;
import compiler.parser.ast.expr.For;
import compiler.parser.ast.expr.Literal;
import compiler.parser.ast.expr.Unary;
import compiler.parser.ast.expr.Where;
import compiler.parser.ast.expr.While;
import compiler.parser.ast.expr.IfThenElse;
import compiler.parser.ast.expr.Name;
import compiler.parser.ast.def.TypeDef;
import compiler.parser.ast.def.VarDef;
import compiler.parser.ast.type.TypeName;
import compiler.parser.ast.type.Type;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.def.Def;

public class Parser {
    /**
     * Seznam leksikalnih simbolov.
     */
    private final List<Symbol> symbols;

    private int currIndex = 0;

    /**
     * Ciljni tok, kamor izpisujemo produkcije. Če produkcij ne želimo izpisovati,
     * vrednost opcijske spremenljivke nastavimo na Optional.empty().
     */
    private final Optional<PrintStream> productionsOutputStream;

    public Parser(List<Symbol> symbols, Optional<PrintStream> productionsOutputStream) {
        requireNonNull(symbols, productionsOutputStream);
        this.symbols = symbols;       
        this.productionsOutputStream = productionsOutputStream;
    }

        /**
     * Izpiše produkcijo na izhodni tok.
     */
    private void dump(String production) {
        if (productionsOutputStream.isPresent()) {
            productionsOutputStream.get().println(production);
        }
    }    
    
    /**
     * Preveri leksem
     */
    private boolean check(TokenType tokenType) {
        return this.symbols.get(currIndex).tokenType == tokenType;
    }     

    /**
     * Preveri leksem
     */
    private boolean checkSkip(TokenType tokenType) {
        if (this.check(tokenType)) {
            //this.skip();
            this.currIndex++;
            return true;
        }
        return false;
    }    

    /**
     * Preskoci leksem
     */
    private void skip() {
        this.currIndex++;
    }

    /**
     * Pridobi ime
     */
    private String getLexeme() {
        return this.symbols.get(this.currIndex).lexeme;
    }

    /**
     * Pridobi pozicijo leksem
     */
    private Position getPosition() {
        return this.symbols.get(this.currIndex).position;
    }

    /**
     * Pridobi pozicijo prejsnega leksema
     */
    private Position getPreviousPosition() {
        if (this.currIndex > 0)
            return this.symbols.get(this.currIndex - 1).position;
        else
            Report.error("Code Error: Previous position less than 0");
        return null;
    }

    /**
     * Pridobi pozicijo leksem
     */
    private Position getFinalPosition(Location startLoc) {
        var endLoc = this.getPreviousPosition().end;
        return new Position(startLoc, endLoc);
    }

    /**
     * Izvedi sintaksno analizo.
     */
    public Ast parse() {
        var ast = parseSource();
        return ast;
    }

    // DONE
    private Ast parseSource() {

        this.dump("source -> definitions");

        var startLoc = this.getPosition().start;
        var defs = this.parseDefinitions();

        if (this.symbols.get(this.currIndex).tokenType != EOF)
            Report.error(this.getPosition(), "Wrong definition (ending). Expected $ (EOP)"); 

        return new Defs(this.getFinalPosition(startLoc), defs);
    }

    // DONE
    private List<Def> parseDefinitions() {
        this.dump("definitions -> definition definitions2");
        var def = this.parseDefinition();
        var def2 = this.parseDefinition2();

        def2.add(0, def);
        return def2;
    }

    // DONE
    private List<Def> parseDefinition2() {        
        if (this.checkSkip(OP_SEMICOLON)) {
            this.dump("definitions2 -> ';' definitions");
            return this.parseDefinitions();
        }
        else 
            this.dump("definitions2 -> e");
        return new ArrayList<Def>();
    }

    // DONE
    private Def parseDefinition() {
        var startLoc = this.getPosition().start;
        if(this.checkSkip(KW_TYP)) {
            this.dump("definition -> type_definition");
            return this.parseTypeDef(startLoc);
        }
        else if (this.checkSkip(KW_FUN)) {
            this.dump("definition -> function_definition");
            return this.parseFunDef(startLoc);
        }
        else if (this.checkSkip(KW_VAR)) {
            this.dump("definition -> variable_definition");
            return this.parseVarDef(startLoc);
        }
        else 
            Report.error(this.getPosition(), "Wrong definition.");
        return null;
    }
    
    // DONE
    private TypeDef parseTypeDef(Location start) {        
        // typ is alredy skiped
        if (this.check(IDENTIFIER)) {
            var name = this.getLexeme();
            this.skip();
            if (this.checkSkip(OP_COLON)) {
                this.dump("type_definition -> 'typ' identifier ':' type");
                var type = this.parseType();
                return new TypeDef(this.getFinalPosition(start), name, type);
            }
            else 
                Report.error(this.getPosition(), "Wrong type_definition: Expected ':' (colon).");
        }
        else 
            Report.error(this.getPosition(), "Wrong type_definition: Expected identifier.");
        return null;
    }

    // DONE
    private FunDef parseFunDef(Location start) {
        // fun is alredy skiped
        this.dump("function_definition -> 'fun' identifier '(' parameters ')' ':' type '=' expression"); 

        if (this.check(IDENTIFIER)) {
            var name = this.getLexeme();
            this.skip();
            if (this.checkSkip(OP_LPARENT)) {
                var params = this.parseParameters();
                if (this.checkSkip(OP_RPARENT))
                    if (this.checkSkip(OP_COLON)) {
                        var type = this.parseType();
                        if (this.checkSkip(OP_ASSIGN)) {
                            var expr = this.parseExpression(); 
                            return new FunDef(this.getFinalPosition(start), name, params, type, expr);     
                        }                
                        else 
                            Report.error(this.getPosition(), "Wrong function_definition: Expected '=' (equals).");
                    } 
                    else 
                        Report.error(this.getPosition(), "Wrong function_definition: Expected ':' (colon).");
                else                 
                    Report.error(this.getPosition(), "Wrong function_definition: Expected ')' (right parantheses).");
            }
            else 
                Report.error(this.getPosition(), "Wrong function_definition: Expected '(' (left parantheses).");
        }
        else 
            Report.error(this.getPosition(), "Wrong function_definition: Expected identifier.");
        return null;
    }    
    
    // DONE
    private Type parseType() {
        var startLoc = this.getPosition().start;
        if (this.check(IDENTIFIER)) {
            var name = this.getLexeme();
            this.skip();
            this.dump("type -> identifier");
            return new TypeName(this.getPreviousPosition(), name);
        }
        else if (this.checkSkip(AT_LOGICAL)) {
            this.dump("type -> 'logical'");
            return Atom.LOG(this.getPreviousPosition());
        }
        else if (this.checkSkip(AT_INTEGER)) {
            this.dump("type -> 'integer'");
            return Atom.INT(this.getPreviousPosition());
        }
        else if (this.checkSkip(AT_STRING)) {
            this.dump("type -> 'string'");
            return Atom.STR(this.getPreviousPosition());
        }
        else if (this.checkSkip(KW_ARR)) {            
            this.dump("type -> 'arr' '[' int_const ']' type");
            if (this.checkSkip(OP_LBRACKET))
                if (this.check(C_INTEGER)){
                    var number = Integer.parseInt(this.getLexeme());
                    this.skip();
                    if (this.checkSkip(OP_RBRACKET)) {
                        var type = this.parseType();
                        return new Array(getFinalPosition(startLoc), number, type);
                    }
                    else
                        Report.error(this.getPosition(), "Wrong type: Expected ']' (right bracket)");   
                }              
                else
                    Report.error(this.getPosition(), "Wrong type: Expected int_constant");                
            else
                Report.error(this.getPosition(), "Wrong type: Expected '[' (left bracket)");   
        }              
        else 
            Report.error(this.getPosition(), "Wrong type");
        return null;
    }
    
    // DONE
    private List<Parameter> parseParameters() {
        this.dump("parameters -> parameter parameters2");
        var param = this.parseParameter();
        var params2 = this.parseParameters2();

        params2.add(0, param);
        return params2;
    }
    
    // DONE
    private List<Parameter> parseParameters2() {
        if (this.checkSkip(OP_COMMA)) {
            this.dump("parameters2 -> ',' parameters");
            return this.parseParameters();
        }
        else 
            this.dump("parameters2 -> e");
        return new ArrayList<Parameter>();
    }      
    
    // DONE
    private Parameter parseParameter() {
        this.dump("identifier ':' type");
        var startLoc = this.getPosition().start;
        if (this.check(IDENTIFIER)) {
            var name = this.getLexeme();
            this.skip();
            if (this.checkSkip(OP_COLON)) {                
                var type = this.parseType();
                return new Parameter(this.getFinalPosition(startLoc), name, type);
            }
            else
                Report.error(this.getPosition(), "Wrong parameter: Expected ':' (colon)");
        }
        else
            Report.error(this.getPosition(), "Wrong parameter: Expected identifier");
        return null;
    }  
    
    // DONE
    private Expr parseExpression() {
        this.dump("expression -> logical_ior_expression expression2");
        var startLoc = this.getPosition().start;
        var expr = this.parseLogicalIorExpression();

        return this.parseExpression2(startLoc, expr);
    }
    // DONE
    private Expr parseExpression2(Location start, Expr expr) {
        if (this.checkSkip(OP_LBRACE)) {
            this.dump("expression2 -> '{' 'WHERE' definitions '}'");
            if (this.checkSkip(KW_WHERE)) {    
                var newStart = this.getPosition().start;          
                var defs = this.parseDefinitions();
                if (this.check(OP_RBRACE)) {         
                    var defsNew =  new Defs(this.getFinalPosition(newStart), defs);    
                    this.skip();   
                    var where = new Where(this.getFinalPosition(start), expr, defsNew);                    
                    return where;
                }
                else 
                    Report.error(this.getPosition(), "Wrong expression2: Expected '}' (right curly bracket)");                             
            }
            else
                Report.error(this.getPosition(), "Wrong expression2: Expected WHERE");
        }
        else
            this.dump("expression2 -> e");
        return expr;
    }    

    // DONE
    private Expr parseLogicalIorExpression() {
        this.dump("logical_ior_expression -> logical_and_expression logical_ior_expression2");
        var startLoc = this.getPosition().start;
        var left = this.parseLogicalAndExpression();
        return this.parseLogicalIorExpression2(left, startLoc);
    }

    // DONE
    private Expr parseLogicalIorExpression2(Expr left, Location start) {
        if (this.checkSkip(OP_OR)) {
            this.dump("logical_ior_expression2 -> '|' logical_ior_expression");    
            var right = this.parseLogicalAndExpression();
            var bin = new Binary(this.getFinalPosition(start), left, Operator.OR, right);
            return this.parseLogicalIorExpression2(bin, start);
        }
        else
            this.dump("logical_ior_expression2 -> e");
        return left;
    }

    // DONE
    private Expr parseLogicalAndExpression() {
        this.dump("logical_and_expression -> compare_expression logical_and_expression2");
        var startLoc = this.getPosition().start;
        var left = this.parseCompareExpression();
        return this.parseLogicalAndExpression2(left, startLoc);
    }

    // DONE
    private Expr parseLogicalAndExpression2(Expr left, Location start) {
        if (this.checkSkip(OP_AND)) {
            this.dump("logical_and_expression2 -> '&' logical_and_expression");
            var right = this.parseCompareExpression();
            var bin = new Binary(this.getFinalPosition(start), left, Operator.AND, right);
            return this.parseLogicalAndExpression2(bin, start);
        }
        else                     
            this.dump("logical_and_expression2 -> e");
        return left;
    }

    // DONE
    private Expr parseCompareExpression() {
        this.dump("compare_expression -> additive_expression compare_expression2");
        var startLoc = this.getPosition().start;
        var left = this.parseAdditiveExpression();
        return this.parseCompareExpression2(left, startLoc);
    }
    
    // DONE
    private Expr parseCompareExpression2(Expr left, Location start) {
        if (this.checkSkip(OP_EQ)) {
            this.dump("compare_expression2 -> '==' additive_expression");
            var right = this.parseAdditiveExpression();
            return new Binary(this.getFinalPosition(start), left, Operator.EQ, right);
        }        
        else if (this.checkSkip(OP_NEQ)) {
            this.dump("compare_expression2 -> '!=' additive_expression");
            var right = this.parseAdditiveExpression();
            return new Binary(this.getFinalPosition(start), left, Operator.NEQ, right);
        }      
        else if (this.checkSkip(OP_LEQ)) {
            this.dump("compare_expression2 -> '<=' additive_expression");
            var right = this.parseAdditiveExpression();
            return new Binary(this.getFinalPosition(start), left, Operator.LEQ, right);
        }      
        else if (this.checkSkip(OP_GEQ)) {
            this.dump("compare_expression2 -> '>=' additive_expression");
            var right = this.parseAdditiveExpression();
            return new Binary(this.getFinalPosition(start), left, Operator.GEQ, right);
        }      
        else if (this.checkSkip(OP_LT)) {
            this.dump("compare_expression2 -> '<' additive_expression");
            var right = this.parseAdditiveExpression();
            return new Binary(this.getFinalPosition(start), left, Operator.LT, right);
        }      
        else if (this.checkSkip(OP_GT)) {
            this.dump("compare_expression2 -> '>' additive_expression");
            var right = this.parseAdditiveExpression();
            return new Binary(this.getFinalPosition(start), left, Operator.GT, right);
        }
        else                     
            this.dump("compare_expression2 -> e");
        return left;
    }

    // DONE
    private Expr parseAdditiveExpression() {
        this.dump("additive_expression -> multiplicative_expression additive_expression2");
        var startLoc = this.getPosition().start;
        var left = this.parseMultiplicativeExpression();
        return this.parseAdditiveExpression2(left, startLoc);
    }

    // DONE
    private Expr parseAdditiveExpression2(Expr left, Location start) {
        if (this.checkSkip(OP_ADD)) {
            this.dump("additive_expression2 -> '+' additive_expression");
            var right = this.parseMultiplicativeExpression();
            var bin = new Binary(this.getFinalPosition(start), left, Operator.ADD, right);
            return this.parseAdditiveExpression2(bin, start);
        }
        else if (this.checkSkip(OP_SUB)) {
            this.dump("additive_expression2 -> '-' additive_expression");
            var right = this.parseMultiplicativeExpression();
            var bin = new Binary(this.getFinalPosition(start), left, Operator.SUB, right);
            return this.parseAdditiveExpression2(bin, start);
        }
        else
            this.dump("additive_expression2 -> e");
        return left;
    }

    // DONE
    private Expr parseMultiplicativeExpression() {
        this.dump("multiplicative_expression -> prefix_expression multiplicative_expression2");
        var startLoc = this.getPosition().start;
        var left = this.parsePrefixExpression(startLoc);
        return this.parseMultiplicativeExpression2(left, startLoc);
    }

    // DONE
    private Expr parseMultiplicativeExpression2(Expr left, Location start) {
        if (this.checkSkip(OP_MUL)) {
            this.dump("multiplicative_expression2 -> '*' multiplicative_expression");
            var right = this.parsePrefixExpression(start);
            var bin = new Binary(this.getFinalPosition(start), left, Operator.MUL, right);
            return this.parseMultiplicativeExpression2(bin, start);
        }
        else if (this.checkSkip(OP_DIV)) {
            this.dump("multiplicative_expression2 -> '/' multiplicative_expression");
            var right = this.parsePrefixExpression(start);
            var bin = new Binary(this.getFinalPosition(start), left, Operator.DIV, right);
            return this.parseMultiplicativeExpression2(bin, start);
        }
        else if (this.checkSkip(OP_MOD)) {
            this.dump("multiplicative_expression2 -> '%' multiplicative_expression");
            var right = this.parsePrefixExpression(start);
            var bin = new Binary(this.getFinalPosition(start), left, Operator.MOD, right);
            return this.parseMultiplicativeExpression2(bin, start);
        }
        else
            this.dump("multiplicative_expression2 -> e");
        return left;
    }

    // DONE
    private Expr parsePrefixExpression(Location start) {
        var newStart = this.getPosition().start;
        if (this.checkSkip(OP_ADD)){
            this.dump("prefix_expression -> '+' prefix_expression");            
            var expr = this.parsePrefixExpression(newStart);
            return new Unary(this.getFinalPosition(newStart), expr, Unary.Operator.ADD);
        }
        else if (this.checkSkip(OP_SUB)){
            this.dump("prefix_expression -> '-' prefix_expression");
            var expr = this.parsePrefixExpression(newStart);
            return new Unary(this.getFinalPosition(newStart), expr, Unary.Operator.SUB);
        }
        else if (this.checkSkip(OP_NOT)){
            this.dump("prefix_expression -> '!' prefix_expression");
            var expr = this.parsePrefixExpression(newStart);
            return new Unary(this.getFinalPosition(newStart), expr, Unary.Operator.NOT);
        }
        else {
            this.dump("prefix_expression -> postfix_expression");
            return this.parsePostfixExpression();
        }
    }

    // DONE
    private Expr parsePostfixExpression() {
        this.dump("postfix_expression -> atom_expression postfix_expression2");
        var startLoc = this.getPosition().start;
        var atomExpr = this.parseAtomExpression(startLoc);
        return this.parsePostfixExpression2(startLoc, atomExpr);
    }

    // DONE
    private Expr parsePostfixExpression2(Location start, Expr atomExpr) {
        if (this.checkSkip(OP_LBRACKET)) {
            this.dump("postfix_expression2 -> '[' expression ']' postfix_expression2");
            var expr1 = this.parseExpression();
            if (this.checkSkip(OP_RBRACKET)) {
                var bin = new Binary(getFinalPosition(start), atomExpr, Operator.ARR, expr1);  
                return parsePostfixExpression2(start, bin);
            }          
            else 
                Report.error(this.getPosition(), "Wrong postfix_expression: Expected ']' (right bracket).");
        }  
        else
            this.dump("postfix_expression2 -> e");
        return atomExpr;
    }

    // DONE
    private Expr parseAtomExpression(Location start) {
        if (this.check(C_LOGICAL)) {
            var lex = this.getLexeme();
            skip();
            this.dump("atom_expression -> log_constant");
            return new Literal(getFinalPosition(start), lex, Atom.Type.LOG);
        }
        else if (this.check(C_INTEGER)){
            var lex = this.getLexeme();
            skip();
            this.dump("atom_expression -> int_constant");
            return new Literal(getFinalPosition(start), lex, Atom.Type.INT);
        }
        else if (this.check(C_STRING)) {
            var lex = this.getLexeme();
            skip();
            this.dump("atom_expression -> str_constant");  
            return new Literal(getFinalPosition(start), lex, Atom.Type.STR);
        }      
        else if (this.check(IDENTIFIER)) {
            this.dump("atom_expression -> identifier atom_expression_Identifier");
            var name = this.symbols.get(this.currIndex);
            this.skip();
            return this.parseAtomExpressionIdentifier(start, name);
        }
        else if (this.checkSkip(OP_LBRACE)) {
            this.dump("atom_expression -> '{' atom_expression_LBracket");
            return this.parseAtomExpressionLBracket(start);
        }
        else if (this.checkSkip(OP_LPARENT)) {
            this.dump("atom_expression -> '(' expressions ')'");
            var exprs = this.parseExpressions();
            if (!this.checkSkip(OP_RPARENT))
                Report.error(this.getPosition(), "Wrong atom_expression: Expected ')' (right parantheses).");
            return new Block(getFinalPosition(start), exprs);
        }
        else
            Report.error(this.getPosition(), "Wrong atom_expression.");
        return null;
    }

    // DONE
    private Expr parseAtomExpressionIdentifier(Location start, Symbol name) {
        if (this.checkSkip(OP_LPARENT)) {
            this.dump("atom_expression_Identifier -> '(' expressions ')'");
            var exprs = this.parseExpressions();
            if (!this.checkSkip(OP_RPARENT))
                Report.error(this.getPosition(), "Wrong atom_expression_Identifier: Expected ')' (right parantheses).");
            return new Call(getFinalPosition(start), exprs, name.lexeme);
        }
        else 
            this.dump("atom_expression_Identifier -> e");
        return new Name(name.position, name.lexeme);
    }

    // DONE
    private Expr parseAtomExpressionLBracket(Location start) {
        if (this.checkSkip(KW_IF)) {
            this.dump("atom_expression_LBracket -> 'if' expression 'then' expression atom_expression_LBracket_IF");
            var expr1 = this.parseExpression();
            if (this.checkSkip(KW_THEN)) {
                var expr2 = this.parseExpression();
                return this.parseAtomExpressionLBracketIF(start, expr1, expr2);
            }
            else
                Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected 'then'.");
        }
        else if (this.checkSkip(KW_WHILE)) {
            this.dump("atom_expression_LBracket -> 'while' expression ':' expression '}'");
            var expr1 = this.parseExpression();
            if (this.checkSkip(OP_COLON))  {
                var expr2 = this.parseExpression();
                if (!this.checkSkip(OP_RBRACE))
                    Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected '}' (right curly bracket).");
                return new While(getFinalPosition(start), expr1, expr2);
            }
            else 
                Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected ':' (column).");
        }
        else if (this.checkSkip(KW_FOR)) {
            this.dump("atom_expression_LBracket -> 'for' identifier '=' expression ',' expression ',' expression ':' expression '}'");
            if (this.check(IDENTIFIER)) {
                var nameSymbol = this.symbols.get(currIndex);
                var name = new Name(nameSymbol.position, nameSymbol.lexeme);
                this.skip();
                if (this.checkSkip(OP_ASSIGN)) {                    
                    var expr1 = this.parseExpression();
                    if (this.checkSkip(OP_COMMA)) {
                        var expr2 =this.parseExpression();
                        if (this.checkSkip(OP_COMMA)) {
                            var expr3 = this.parseExpression();
                            if (this.checkSkip(OP_COLON)) {
                                var expr4 = this.parseExpression();
                                if (!this.checkSkip(OP_RBRACE))
                                    Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected '}' (right curly bracket).");
                                return new For(getFinalPosition(start), name, expr1, expr2, expr3, expr4);
                            }
                            else 
                                Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected ':' (colon).");
                        }
                        else 
                            Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected ',' (comma).");    
                    }
                    else 
                        Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected ',' (comma).");
                }
                else
                    Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected '=' (assign).");
            }
            else 
                Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected identifier.");
        }
        else {
            this.dump("atom_expression_LBracket -> expression '=' expression '}'");
            var expr1 = this.parseExpression();
            if (this.checkSkip(OP_ASSIGN)) {
                var expr2 = this.parseExpression();
                if (!this.checkSkip(OP_RBRACE))
                    Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected '}' (right curly bracket).");
                return new Binary(getFinalPosition(start), expr1, Operator.ASSIGN, expr2);
            }
            else
                Report.error(this.getPosition(), "Wrong atom_expression_LBracket: Expected '=' (assign).");
        }
        return null;
    }

    // DONE
    private Expr parseAtomExpressionLBracketIF(Location start, Expr expr1, Expr expr2) {
        if (this.checkSkip(OP_RBRACE)) {
            this.dump("'}'");
            return new IfThenElse(getFinalPosition(start), expr1, expr2);
        }
        else if (this.checkSkip(KW_ELSE)) {
            this.dump("'else' expression '}'");
            var expr3 = this.parseExpression();            
            if (!this.checkSkip(OP_RBRACE)) 
                Report.error(this.getPosition(), "Wrong atom_expression_LBracket_IF: Expected '}' (right curly bracket).");
            return new IfThenElse(getFinalPosition(start), expr1, expr2, expr3);
        }
        else 
            Report.error(this.getPosition(), "Wrong atom_expression_LBracket_IF.");
        return null;
    }

    // DONE
    private List<Expr> parseExpressions() {
        this.dump("expressions -> expression expressions2");
        var expr = this.parseExpression();
        var exprs = this.parseExpressions2();

        exprs.add(0, expr);
        return exprs;
    }

    // DONE
    private List<Expr> parseExpressions2() {
        if (this.checkSkip(OP_COMMA)) {
            this.dump("expressions2 -> ',' expressions");
            return this.parseExpressions();
        }
        else 
            this.dump("expressions2 -> e ");
        return new ArrayList<Expr>();
    }

    // DONE
    private VarDef parseVarDef(Location start) {
        // var is alredy skiped        
        this.dump("variable_definition -> 'var' identifer ':' type");
        if (this.check(IDENTIFIER)) {
            var name = this.getLexeme();
            this.skip();
            if (this.checkSkip(OP_COLON)) {                
                var type = this.parseType();
                var endLoc = this.getPreviousPosition().end;
                return new VarDef(new Position(start, endLoc), name, type);
            }
            else 
                Report.error(this.getPosition(), "Wrong variable_definition: Expected ':' (colon).");
        }
        else 
            Report.error(this.getPosition(), "Wrong variable_definition: Expected identifier.");
        return null;
    }    
}
