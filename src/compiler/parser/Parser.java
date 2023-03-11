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
        this.dump("definitions -> definition definitions2 ");
        this.parseDefinition();
        this.parseDefinition2();
    }

    // DONE
    private void parseDefinition2() {        
        if (this.checkSkip(OP_SEMICOLON)) {
            this.dump("definitions_-> ';' definitions");
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
                Report.error(this.getPosition(), "Wrong type_definition: Missing ':' (colon).");
        else 
            Report.error(this.getPosition(), "Wrong type_definition: Missing identifier.");
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
                        if (this.checkSkip(OP_EQ)) {
                            this.dump(" '=' expression");
                            this.parseExpression();
                        }
                        else 
                            Report.error(this.getPosition(), "Wrong function_definition: Missing '=' (equals).");
                    } 
                    else 
                        Report.error(this.getPosition(), "Wrong function_definition: Missing ':' (colon).");
                else                 
                    Report.error(this.getPosition(), "Wrong function_definition: Missing ')' (right parantheses).");
            }
            else 
                Report.error(this.getPosition(), "Wrong function_definition: Missing '(' (left parantheses).");
        else 
            Report.error(this.getPosition(), "Wrong function_definition: Missing identifier.");
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
                Report.error(this.getPosition(), "Wrong variable_definition: Missing ':' (colon).");
        else 
            Report.error(this.getPosition(), "Wrong variable_definition: Missing identifier.");
    }

    // DONE
    private void parseType() {
        if (this.checkSkip(IDENTIFIER)) 
            this.dump("type -> identifier");
        if (this.checkSkip(AT_LOGICAL))
            this.dump("type -> logical");
        if (this.checkSkip(AT_INTEGER))
            this.dump("type -> integer");
        if (this.checkSkip(AT_STRING))
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
    
    private void parseParameters() {

    }    
    private void parseExpression() {

    }
}
