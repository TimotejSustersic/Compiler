/**
 * @Author: turk
 * @Description: Leksikalni analizator.
 */

package compiler.lexer;

import static common.RequireNonNull.requireNonNull;
import static compiler.lexer.TokenType.*;
import compiler.lexer.Position.Location;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.Report;

public class Lexer {
    /**
     * Izvorna koda.
     */
    private final String source;

    int line = 1;
    int column = 1;

    Location startLocation;
    String word;

    ArrayList<Symbol> symbols;

    /**
     * Preslikava iz ključnih besed v vrste simbolov.
     */
    private final static Map<String, TokenType> keywordMapping;

    static {
        keywordMapping = new HashMap<>();
        for (var token : TokenType.values()) {
            var str = token.toString();
            if (str.startsWith("KW_")) {
                keywordMapping.put(str.substring("KW_".length()).toLowerCase(), token);
            }
            if (str.startsWith("AT_")) {
                keywordMapping.put(str.substring("AT_".length()).toLowerCase(), token);
            }
        }
        System.out.println(keywordMapping);
    }

    /**
     * Ustvari nov analizator.
     * 
     * @param source Izvorna koda programa.
     */
    public Lexer(String source) {
        requireNonNull(source);
        this.source = source;
    }

    public void addOperator(TokenType tokenType, String operator) {

        // if there is a word infront with no space
        if (word.length() > 0) {
            this.processWord();
            this.startLocation = new Location(this.line, this.column);      
            this.word = "";          
        }

        // process add operator
        ++this.column;
        if (operator.length() == 2)
            ++this.column;
        var endLocation = new Location(this.line, this.column);        
        symbols.add(new Symbol(this.startLocation, endLocation, tokenType, operator));

        // update start
        this.startLocation = new Location(this.line, this.column); 
    }

    // start location is neccesery mybe endLocation will become too
    public void processWord() {
        var endLocation = new Location(this.line, this.column);
        if (keywordMapping.containsKey(this.word)) 
            symbols.add(new Symbol(this.startLocation, endLocation, keywordMapping.get(this.word), this.word));
        else if (this.word == "EOF") 
            symbols.add(new Symbol(this.startLocation, endLocation, EOF, this.word));                    
        else 
            symbols.add(new Symbol(this.startLocation, endLocation, IDENTIFIER, this.word));        
    }

    /**
     * Izvedi leksikalno analizo.
     * 
     * @return seznam leksikalnih simbolov.
     */
    public List<Symbol> scan() {
        this.symbols = new ArrayList<Symbol>();
        
        // current symbol
        this.word = "";
        this.startLocation = new Location(this.line, this.column);

        for (int i = 0; i < this.source.length(); i++) {

            char letter = this.source.charAt(i);

            // it has to be done this way sice operators like semicoloumn arent separated with space
            if (letter == '+')
                this.addOperator(OP_ADD, String.valueOf(letter));
            else if (letter == '-') 
                this.addOperator(OP_SUB, String.valueOf(letter));
            else if (letter == '*') 
                this.addOperator(OP_MUL, String.valueOf(letter));     
            else if (letter == '/') 
                this.addOperator(OP_DIV, String.valueOf(letter));     
            else if (letter == '%') 
                this.addOperator(OP_MOD, String.valueOf(letter));    

            else if (letter == '&') 
                this.addOperator(OP_AND, String.valueOf(letter));     
            else if (letter == '|') 
                this.addOperator(OP_OR, String.valueOf(letter));    
                
            // dvojni
            else if (letter == '!') {
                if (this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_NEQ, "!=");
                    i++;
                }
                else
                    this.addOperator(OP_NOT, String.valueOf(letter));
            }
            else if (letter == '=') {
                if (this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_EQ, "==");
                    i++;
                }
                else
                    this.addOperator(OP_ASSIGN, String.valueOf(letter));
            }            
            else if (letter == '<') {
                if (this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_LEQ, "<=");
                    i++;
                }
                else
                    this.addOperator(OP_LT, String.valueOf(letter));
            }            
            else if (letter == '>') {
                if (this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_GEQ, ">=");
                    i++;
                }
                else
                    this.addOperator(OP_GT, String.valueOf(letter));
            }

            else if (letter == '(') 
                this.addOperator(OP_LPARENT, String.valueOf(letter));     
            else if (letter == ')') 
                this.addOperator(OP_RPARENT, String.valueOf(letter));     
            else if (letter == '[') 
                this.addOperator(OP_LBRACKET, String.valueOf(letter));     
            else if (letter == ']') 
                this.addOperator(OP_RBRACKET, String.valueOf(letter));                    
            else if (letter == '{') 
                this.addOperator(OP_LBRACE, String.valueOf(letter));     
            else if (letter == '}') 
                this.addOperator(OP_RBRACE, String.valueOf(letter));  

            else if (letter == ':') 
                this.addOperator(OP_COLON, String.valueOf(letter));     
            else if (letter == ';') 
                this.addOperator(OP_SEMICOLON, String.valueOf(letter));        
            else if (letter == '.') 
                this.addOperator(OP_DOT, String.valueOf(letter));        
            else if (letter == ',') 
                this.addOperator(OP_COMMA, String.valueOf(letter));

            // space or endLine
            else if (letter == ' ' || letter == '\n' || letter == '\r') {

                if (word.length() > 0) 
                   this.processWord();
                
                // if endline 
                if (letter == '\n') {
                    ++this.line;
                    this.column = 1;
                }
                else {
                    ++this.column;
                }

                // new word ... reset
                this.word = "";
                this.startLocation = new Location(this.line, this.column);
            }
            else {
                this.word += letter;
                ++this.column;
            }            
        }

        return symbols;
    }


}
