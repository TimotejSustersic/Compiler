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

        // if operator has 2 letters
        if (operator.length() == 2)
            ++this.column;

        var endLocation = new Location(this.line, this.column - 1);        
        symbols.add(new Symbol(this.startLocation, endLocation, tokenType, operator));

        // update start
        this.startLocation = new Location(this.line, this.column); 
    }

    // if the word is a integer
    public boolean isInteger() {

        for (int i = 1; i < this.word.length(); i++) {

            char letter = this.word.charAt(i);

            if (letter < 48 || letter > 57)                 
                return false;
        }
        return true;
    }

    // if the word is a identifier // mogoce raj nared da crke preverja in da ne gre skos skozi zanke
    // sprotno preverjanje je implementirano ampak ne vem ce dela u nulo, tole pa 100% dela ampak je velik pocasnej
    public boolean isIdent() {
        for (int i = 1; i < this.word.length(); i++) {

            char letter = this.word.charAt(i);

            if (!((letter >= 65 && letter <= 90) || (letter >= 97 && letter <= 122) || (letter >= 48 && letter <= 57) || letter == 95))                 
                return false;
        }
        return true;
    }

    // start location is neccesery mybe endLocation will become too
    public void processWord() {
        var endLocation = new Location(this.line, this.column - 1);
        if (keywordMapping.containsKey(this.word)) 
            symbols.add(new Symbol(this.startLocation, endLocation, keywordMapping.get(this.word), this.word));
        //else if (this.word.equals("EOF")) 
            //symbols.add(new Symbol(this.startLocation, endLocation, EOF, this.word));           
        else if (this.word.equals("true") || this.word.equals("false") )
            symbols.add(new Symbol(this.startLocation, endLocation, C_LOGICAL, this.word));
        else if (this.word.charAt(0) >= 48 && this.word.charAt(0) <= 57) 
            if (this.isInteger())
                symbols.add(new Symbol(this.startLocation, endLocation, C_INTEGER, this.word));   
            else
                Report.error(new Position(this.startLocation, this.startLocation), "IDENTIFIER can't start with an integer.");           
        else// if (this.isIdent())
            symbols.add(new Symbol(this.startLocation, endLocation, IDENTIFIER, this.word)); 
        //else
            //Report.error(new Position(this.startLocation, endLocation), "IDENTIFIER has invalid character.");           
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
        var endLocation = new Location(this.line, this.column);

        var isString = false;
        var isComment = false;

        for (int i = 0; i < this.source.length(); i++) {

            char letter = this.source.charAt(i);

            
            // string inside 39 == '
            if (letter == 39) {
                // string end (string is in procces and we hit 39)
                if (isString) {

                    ++this.column;

                    // string end 1 before single quote
                    endLocation = new Location(this.line, this.column - 1);
                    symbols.add(new Symbol(this.startLocation, endLocation, C_STRING, this.word.substring(1, this.word.length())));
                    
                    // reset
                    this.word = "";
                    this.startLocation = new Location(this.line, this.column);
                    isString = false;
                    continue;
                }
                // string start
                else {
                    if (word.length() > 0) 
                    this.processWord();

                    this.word = "";
                    // start of string is after single quote
                    this.startLocation = new Location(this.line, this.column + 1);
                    isString = true;
                }
            }
            // if word is string
            if (isString) {

                // check if end of file
                if (i == (this.source.length() - 1))
                    Report.error(new Position(startLocation, new Location(this.line, this.column)), "String is not finnished with single quote.");                    

                if (letter == '\n') 
                    Report.error(new Position(new Location(this.line, this.column), new Location(this.line, this.column)), "String doesn't support end line.");

                if (letter < 32 || letter > 126)
                    Report.error(new Position(new Location(this.line, this.column), new Location(this.line, this.column)), "String contains a forbiden character.");

                this.word += letter;
                ++this.column;
                continue;
            }

            // comment
            if (letter ==  35 && !isComment) {

                if (word.length() > 0) 
                this.processWord();

                // start of string is after single quote
                this.startLocation = new Location(this.line, this.column + 1);

                isComment = true;

            }
            // if word is comment 
            if (isComment) {

                this.word += letter;
                ++this.column;
                
                if (letter == '\n') {
                    ++this.line;
                    this.column = 1;

                    // comment includes break line but ends one before it
                    //endLocation = new Location(this.line, this.column - 1);
                    // there is no comment type
                    //symbols.add(new Symbol(this.startLocation, endLocation , C_STRING, this.word));

                    // reset
                    isComment = false;
                    this.word = "";
                    this.startLocation = new Location(this.line, this.column);
                }               
                
                continue;
            }


            // it has to be done this way sice operators like semicoloumn arent separated with space
            else if (letter == '+')
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
                if (i != (this.source.length() - 1) && this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_NEQ, "!=");
                    i++;
                }
                else
                    this.addOperator(OP_NOT, String.valueOf(letter));
            }
            else if (letter == '=') {
                if (i != (this.source.length() - 1) && this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_EQ, "==");
                    i++;
                }
                else
                    this.addOperator(OP_ASSIGN, String.valueOf(letter));
            }            
            else if (letter == '<') {
                if (i != (this.source.length() - 1) && this.source.charAt(i + 1) == '=') {
                    this.addOperator(OP_LEQ, "<=");
                    i++;
                }
                else
                    this.addOperator(OP_LT, String.valueOf(letter));
            }            
            else if (letter == '>') {
                if (i != (this.source.length() - 1) && this.source.charAt(i + 1) == '=') {
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
            else if (letter == ' ' || letter == '\n' || letter == '\r' || letter == '\t') {

                if (word.length() > 0) 
                   this.processWord();
                
                // if endline 
                if (letter == '\n') {
                    ++this.line;
                    this.column = 1;
                }
                else if (letter == '\t') {
                    this.column += 4;
                }
                else {
                    ++this.column;
                }

                // new word ... reset
                this.word = "";
                this.startLocation = new Location(this.line, this.column);
            }
            else {
                if (!((letter >= 65 && letter <= 90) || (letter >= 97 && letter <= 122) || (letter >= 48 && letter <= 57) || letter == 95))                 
                    Report.error(new Position(new Location(this.line, this.column), new Location(this.line, this.column)), "Invalid character.");

                this.word += letter;
                ++this.column;
            }            
        }

        // final word
        if (word.length() > 0 && !isComment) 
            this.processWord();

        symbols.add(new Symbol(this.startLocation, endLocation, EOF, "EOF"));  

        return symbols;
    }


}
