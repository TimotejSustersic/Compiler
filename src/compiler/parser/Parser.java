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

    private void parseDefinitions() {
        this.dump("definitions -> definition definitions2 ");
        this.parseDefinition();
        this.parseDefinition2();
    }

    private void parseDefinition2() {        
        if (this.check(OP_SEMICOLON)) {
            this.dump("definitions_-> ; definitions");
            this.skip();
            // zdej pa lahko
            this.parseDefinitions();
            // ali pa napisemo ze njegovo izpeljavo
            //this.parseDefinition();
            //this.parseDefinition2();
        }
        else 
            this.dump("definitions2 -> e");
    }

    private void parseDefinition() {
        if(this.check(KW_TYP)) {
            this.dump("definition -> type_definition");
            this.skip();
            this.parseTypeDef();
        }
        else if (this.check(KW_FUN)) {
            this.dump("definition -> function_definition");
            this.skip();
            //this.parseTypeFun();
        }
        else if (this.check(KW_VAR)) {
            this.dump("definition -> variable_definition");
            this.skip();
            //this.parseTypeVar();
        }
        else 
            Report.error(this.getPosition(), "Wrong definition.");
    }
    
    private void parseTypeDef() {

        this.dump("type_definition -> typ identifier : type");
        // typ is alredy skiped
        if (this.checkSkip(IDENTIFIER))
            if (this.checkSkip(OP_COLON)) {
               this.parseType();
            }
            else 
                Report.error(this.getPosition(), "Missing colon.");
        else 
            Report.error(this.getPosition(), "Missing identifier.");
    }

    private void parseType() {
        
    }
}
