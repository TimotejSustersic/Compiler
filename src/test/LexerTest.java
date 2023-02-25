package test;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;

import org.junit.Test;

import cli.PINS;
import compiler.lexer.Lexer;

public class LexerTest {
    
    @Test
    public void positionTest_KW_IDENTIFIER() throws IOException  {
        String[] args = {"PINS", "positionTest_KW_IDENTIFIER.txt"};
        var cli = PINS.parse(args);
        //var sourceCode = Files.readString(Paths.get(cli.sourceFile));
        // var symbols = new Lexer(sourceCode).scan();

        String givenResult = "";
        // for (var symbol : symbols) {
        //     givenResult += symbol.toString();
        // }

        String expectedResult = "[1:1-1:6] IDENTIFIER:codex[2:1-2:4] IDENTIFIER:kul[2:5-2:9] IDENTIFIER:\r\n[2:10-2:15] IDENTIFIER:buddy[3:1-3:6] KW_WHILE:while";

        assertEquals(expectedResult, givenResult);
    }    
    
    @Test
    public void positionTest_OP() throws IOException  {
        String[] args = {"PINS", "positionTest_OP.txt"};
        var cli = PINS.parse(args);
        //var sourceCode = Files.readString(Paths.get(cli.sourceFile));
        // var symbols = new Lexer(sourceCode).scan();

        String givenResult = "";
        // for (var symbol : symbols) {
        //     givenResult += symbol.toString();
        // }

        String expectedResult = "[1:1-1:8] AT_INTEGER:integer[1:9-1:10] IDENTIFIER:x[1:11-1:12] OP_ASSIGN:=[1:13-1:14] IDENTIFIER:2[1:14-1:15] OP_ADD:+[1:15-1:16] IDENTIFIER:3[1:16-1:17] OP_SEMICOLON:;[3:1-3:3] KW_IF:if[3:4-3:5] OP_LPARENT:([3:5-3:6] IDENTIFIER:x[3:6-3:8] OP_NEQ:!=[3:8-3:9] IDENTIFIER:3[3:10-3:11] OP_AND:&[3:11-3:12] OP_AND:&[3:13-3:14] IDENTIFIER:x[3:15-3:17] OP_GEQ:>=[3:18-3:19] IDENTIFIER:5[3:19-3:20] OP_RPARENT:)[3:21-3:22] OP_LBRACE:{[4:5-4:12] AT_INTEGER:integer[4:13-4:14] IDENTIFIER:b[4:15-4:16] OP_ASSIGN:=[4:17-4:18] IDENTIFIER:1[4:18-4:19] OP_SEMICOLON:;[5:1-5:2] OP_RBRACE:}";

        assertEquals(expectedResult, givenResult);
    }

    @Test
    public void positionTest_Number() throws IOException  {
        String[] args = {"PINS", "positionTest_Number.txt"};
        var cli = PINS.parse(args);
        //var sourceCode = Files.readString(Paths.get(cli.sourceFile));
        // var symbols = new Lexer(sourceCode).scan();

        String givenResult = "";
        // for (var symbol : symbols) {
        //     givenResult += symbol.toString();
        // }

        String expectedResult = "";

        assertEquals(expectedResult, givenResult);
    }    

    @Test
    public void positionTest_IDENTIFIER_Wrong_Decleration() throws IOException  {
        String[] args = {"PINS", "positionTest_IDENTIFIER_Wrong_Decleration.txt"};
        var cli = PINS.parse(args);
        //var sourceCode = Files.readString(Paths.get(cli.sourceFile));
        // var symbols = new Lexer(sourceCode).scan();

        String givenResult = "";
        // for (var symbol : symbols) {
        //     givenResult += symbol.toString();
        // }

        String expectedResult = "";

        assertEquals(expectedResult, givenResult);
    }   

    @Test
    public void positionTest_String() throws IOException  {
        String[] args = {"PINS", "positionTest_String.txt"};
        var cli = PINS.parse(args);
        //var sourceCode = Files.readString(Paths.get(cli.sourceFile));
        // var symbols = new Lexer(sourceCode).scan();

        String givenResult = "";
        // for (var symbol : symbols) {
        //     givenResult += symbol.toString();
        // }

        String expectedResult = "[1:1-1:7] AT_STRING:string[1:8-1:9] IDENTIFIER:s[1:10-1:11] OP_ASSIGN:=[1:13-1:16] C_STRING:niz[1:17-1:18] OP_SEMICOLON:;";

        assertEquals(expectedResult, givenResult);
    }    

    @Test
    public void positionTest_Unfinnished_String() throws IOException  {
        String[] args = {"PINS", "positionTest_Unfinnished_String.txt"};
        var cli = PINS.parse(args);
        //var sourceCode = Files.readString(Paths.get(cli.sourceFile));
        // var symbols = new Lexer(sourceCode).scan();

        String givenResult = "";
        // for (var symbol : symbols) {
        //     givenResult += symbol.toString();
        // }

        String expectedResult = "";

        assertEquals(expectedResult, givenResult);
    }   
}
