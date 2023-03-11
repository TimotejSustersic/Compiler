/**
 * @Author: turk
 * @Description: Sintaksni analizator.
 */

package compiler.parser;

import static compiler.lexer.TokenType.*;
import static common.RequireNonNull.requireNonNull;

import java.io.PrintStream;
import java.util.List;
import java.util.Optional;

import common.Report;
import compiler.lexer.Position;
import compiler.lexer.Symbol;
import compiler.lexer.TokenType;

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
     * Izvedi sintaksno analizo.
     */
    public void parse() {
        parseSource();
    }

    private void parseSource() {

        this.dump("source -> definitions");
        this.parseDefinitions();

        // nisem vedu a bi blo bol delat error je tko da ti pove kaj manjka/prcakuje alli celotno strukture k jo prckuje
        // in sem se odlocu da pove samo za naslednji pricakovani leksem
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
            this.skip();
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
     * Preskoci leksem
     */
    private Position getPosition() {
        return this.symbols.get(this.currIndex).position;
    }

    // DONE
    private void parseDefinitions() {
        this.dump("definitions -> definition definitions2");
        this.parseDefinition();
        this.parseDefinition2();
    }

    // DONE
    private void parseDefinition2() {        
        if (this.checkSkip(OP_SEMICOLON)) {
            this.dump("definitions2 -> ';' definitions");
            // zdej pa lahko
            this.parseDefinitions();
            // ali pa napisemo ze njegovo izpeljavo
            //this.parseDefinition();
            //this.parseDefinition2();
        }
        else 
            this.dump("definitions2 -> e");
    }

    // DONE
    private void parseDefinition() {
        if(this.checkSkip(KW_TYP)) {
            this.dump("definition -> type_definition");
            this.parseTypeDef();
        }
        else if (this.checkSkip(KW_FUN)) {
            this.dump("definition -> function_definition");
            this.parseFunDef();
        }
        else if (this.checkSkip(KW_VAR)) {
            this.dump("definition -> variable_definition");
            this.parseVarDef();
        }
        else 
            Report.error(this.getPosition(), "Wrong definition.");
    }
    
    // DONE
    private void parseTypeDef() {        
        // typ is alredy skiped
        if (this.checkSkip(IDENTIFIER))
            if (this.checkSkip(OP_COLON)) {
                this.dump("type_definition -> typ identifier ':' type");
                this.parseType();
            }
            else 
                Report.error(this.getPosition(), "Wrong type_definition: Expected ':' (colon).");
        else 
            Report.error(this.getPosition(), "Wrong type_definition: Expected identifier.");
    }

    // DONE
    private void parseFunDef() {
        // fun is alredy skiped
        if (this.checkSkip(IDENTIFIER))
            if (this.checkSkip(OP_LPARENT)) {    
                this.dump("function_definition -> fun identifier '(' parameters");            
                this.parseParameters();
                if (this.checkSkip(OP_RPARENT))
                    if (this.checkSkip(OP_COLON)) {
                        this.dump(" ')' ':' type");
                        this.parseType();
                        if (this.checkSkip(OP_ASSIGN)) {
                            this.dump(" '=' expression");
                            this.parseExpression();
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
        else 
            Report.error(this.getPosition(), "Wrong function_definition: Expected identifier.");
    }    
    
    // DONE
    private void parseVarDef() {
        // var is alredy skiped
        if (this.checkSkip(IDENTIFIER))
            if (this.checkSkip(OP_COLON)) {
                this.dump("variable_definition -> var identifer ':' type");
                this.parseType();
            }
            else 
                Report.error(this.getPosition(), "Wrong variable_definition: Expected ':' (colon).");
        else 
            Report.error(this.getPosition(), "Wrong variable_definition: Expected identifier.");
    }

    // DONE
    private void parseType() {
        if (this.checkSkip(IDENTIFIER)) 
            this.dump("type -> identifier");
        else if (this.checkSkip(AT_LOGICAL))
            this.dump("type -> logical");
        else if (this.checkSkip(AT_INTEGER))
            this.dump("type -> integer");
        else if (this.checkSkip(AT_STRING))
            this.dump("type -> string");
        else if (this.checkSkip(KW_ARR))
            if (this.checkSkip(OP_LBRACKET))
                if (this.checkSkip(C_INTEGER))
                    if (this.checkSkip(OP_RBRACKET))  {
                        this.dump("type -> arr '[' int_const ']' type");
                        this.parseType(); 
                    }
                    else
                        Report.error(this.getPosition(), "Wrong type: Expected ']' (right bracket)");                 
                else
                    Report.error(this.getPosition(), "Wrong type: Expected int_constant");                
            else
                Report.error(this.getPosition(), "Wrong type: Expected '[' (left bracket)");                 
        else 
            Report.error(this.getPosition(), "Wrong type");
    }
    
    // DONE
    private void parseParameters() {
        this.dump("parameters -> parameter parameters2");
        this.parseParameter();
        this.parseParameters2();
    }
    
    // DONE
    private void parseParameters2() {
        if (this.checkSkip(OP_COMMA)) {
            this.dump("parameters2 -> ',' parameters");
            this.parseParameters();
        }
        else 
            this.dump("parameters2 -> e");
    }      
    
    // DONE
    private void parseParameter() {
        if (this.checkSkip(IDENTIFIER))
            if (this.checkSkip(OP_COLON)) {
                this.dump("identifier ':' type");
                this.parseType();
            }
            else
                Report.error(this.getPosition(), "Wrong parameter: Expected ':' (colon)");
        else
            Report.error(this.getPosition(), "Wrong parameter: Expected identifier");
    }  
    
    // DONE
    private void parseExpression() {
        this.dump("expression -> logical_ior_expression expression2");
        this.parseLogicalIorExpression();
        this.parseExpression2();
    }       

    // DONE
    private void parseExpression2() {
        if (this.checkSkip(OP_LBRACE))
            if (this.checkSkip(KW_WHERE)) {
                this.dump("expression2 -> '{' WHERE definitions");
                this.parseDefinitions();
                if (this.checkSkip(OP_RBRACE))
                    this.dump(" '}'");
                else
                    Report.error(this.getPosition(), "Wrong expression: Expected '}' (right curly bracket)");
            }
            else
                Report.error(this.getPosition(), "Wrong expression: Expected WHERE");
        else
            this.dump("expression2 -> e");
    }    

    // DONE
    private void parseLogicalIorExpression() {
        this.dump("logical_ior_expression -> logical_and_expression logical_ior_expression2");
        this.parseLogicalAndExpression();
        this.parseLogicalIorExpression2();
    }

    // DONE
    private void parseLogicalIorExpression2() {
        if (this.checkSkip(OP_OR)) {
            this.dump("logical_ior_expression2 -> '|' logical_ior_expression");
            this.parseLogicalIorExpression();
        }
        else                     
            this.dump("logical_ior_expression2 -> e");
    }

    // DONE
    private void parseLogicalAndExpression() {
        this.dump("logical_and_expression -> compare_expression logical_and_expression2");
        this.parseCompareExpression();
        this.parseLogicalAndExpression2();
    }

    // DONE
    private void parseLogicalAndExpression2() {
        if (this.checkSkip(OP_AND)) {
            this.dump("logical_and_expression2 -> '&' logical_and_expression");
            this.parseLogicalAndExpression();
        }
        else                     
            this.dump("logical_and_expression2 -> e");
    }

    // DONE
    private void parseCompareExpression() {
        this.dump("compare_expression -> additive_expression compare_expression2");
        this.parseAdditiveExpression();
        this.parseCompareExpression2();
    }
    
    // DONE
    private void parseCompareExpression2() {
        if (this.checkSkip(OP_EQ)) {
            this.dump("compare_expression2 -> '==' additive_expression");
            this.parseAdditiveExpression();
        }        
        else if (this.checkSkip(OP_NEQ)) {
            this.dump("compare_expression2 -> '!=' additive_expression");
            this.parseAdditiveExpression();
        }      
        else if (this.checkSkip(OP_LEQ)) {
            this.dump("compare_expression2 -> '<=' additive_expression");
            this.parseAdditiveExpression();
        }      
        else if (this.checkSkip(OP_GEQ)) {
            this.dump("compare_expression2 -> '>=' additive_expression");
            this.parseAdditiveExpression();
        }      
        else if (this.checkSkip(OP_LT)) {
            this.dump("compare_expression2 -> '<' additive_expression");
            this.parseAdditiveExpression();
        }      
        else if (this.checkSkip(OP_GT)) {
            this.dump("compare_expression2 -> '>' additive_expression");
            this.parseAdditiveExpression();
        }
        else                     
            this.dump("compare_expression2 -> e");
    }

    // TEST
    private void parseAdditiveExpression() {
        this.dump("additive_expression -> multiplicative_expression additive_expression2");
        this.parseMultiplicativeExpression();
        this.parseAdditiveExpression2();
    }

    // TEST
    private void parseAdditiveExpression2() {
        if (this.checkSkip(OP_ADD)) {
            this.dump("additive_expression2 -> '+' additive_expression");
            this.parseAdditiveExpression();
        }
        else if (this.checkSkip(OP_SUB)) {
            this.dump("additive_expression2 -> '-' additive_expression");
            this.parseAdditiveExpression();
        }
        else
            this.dump("additive_expression2 -> e");
    }

    // TEST
    private void parseMultiplicativeExpression() {
        this.dump("multiplicative_expression -> prefix_expression multiplicative_expression2");
        this.parsePrefixExpression();
        this.parseMultiplicativeExpression2();
    }

    // TEST
    private void parseMultiplicativeExpression2() {
        if (this.checkSkip(OP_MUL)) {
            this.dump("multiplicative_expression2 -> '*' multiplicative_expression");
            this.parseMultiplicativeExpression();
        }
        else if (this.checkSkip(OP_DIV)) {
            this.dump("multiplicative_expression2 -> '/' multiplicative_expression");
            this.parseMultiplicativeExpression();
        }
        else if (this.checkSkip(OP_MOD)) {
            this.dump("multiplicative_expression2 -> '%' multiplicative_expression");
            this.parseMultiplicativeExpression();
        }
        else
            this.dump("multiplicative_expression2 -> e");
    }

    // DONE
    private void parsePrefixExpression() {
        if (this.checkSkip(OP_ADD)){
            this.dump("prefix_expression -> '+' prefix_expression");
            this.parsePrefixExpression();
        }
        if (this.checkSkip(OP_SUB)){
            this.dump("prefix_expression -> '-' prefix_expression");
            this.parsePrefixExpression();
        }
        if (this.checkSkip(OP_NOT)){
            this.dump("prefix_expression -> '!' prefix_expression");
            this.parsePrefixExpression();
        }
        else {
            this.dump("prefix_expression -> postfix_expression");
            this.parsePostfixExpression();
        }
    }

    // DONE
    private void parsePostfixExpression() {
        this.dump("postfix_expression -> atom_expression postfix_expression2");
        this.parseAtomExpression();
        this.parsePostfixExpression2();
    }

    // DONE
    private void parsePostfixExpression2() {
        if (this.checkSkip(OP_LBRACKET)) {
            this.dump("postfix_expression2 -> '[' expression");
            this.parseExpression();
            if (this.checkSkip(OP_RBRACKET)){
                this.dump(" ']' postfix_expression2");
                this.parsePostfixExpression2();
            }
            else 
                Report.error(this.getPosition(), "Wrong postfix_expression: Expected ']' (right bracket).");
        }  
        else
            this.dump("postfix_expression2 -> e");
    }

    // TODO
    private void parseAtomExpression() {

    }

}
