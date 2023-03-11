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

        //for (Symbol symbol: this.symbols) {
        //    this.dump("ups");
        //}
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
     * Preveri
     */
    private boolean check(TokenType production) {
        return true;
    }    

    /**
     * Skipi
     */
    private void skip() {
       
    }

    private void parseDefinitions() {
        this.dump("definitions->definition definitions' ");
        this.parseDefinition();
        this.parseDefinition_();
    }
    
    private void parseDefinition() {
        if(this.check(KW_TYP)) {
            this.dump("def.->type_def");
            this.skip();
            this.parseTypeDef();
        }
        else if (this.check(KW_FUN))
            this.dump("//def.->type_def");
        else if (this.check(IDENTIFIER))
            this.dump("//def.->type_def");
        else 
            Report.error("neki");
    }
    
    private void parseTypeDef() {

        this.dump("type_def->typ id : X");

        if (this.check(IDENTIFIER)) {
            this.skip();
        //
        }
        else 
            Report.error("n");
    }
            
    private void parseDefinition_() {
        
        if (this.check(OP_SEMICOLON)) {
            this.dump("definitions-> definitions");
            this.skip();
            // zdej pa lahko
            this.parseDefinitions();
            // ali pa napisemo ze njegovo izpeljavo
            this.parseDefinition();
            this.parseDefinition_();
        }
        else {
            this.dump("definitions'->e");
        }
    }
}
