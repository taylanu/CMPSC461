# authored by Gang Tan; all rights reserved; do not distribute

# CMPSC 461 Project 1 Taylan Unal (tuu2)
# introducing some constants; can also use the enum type if Python 3.4
# is available
DIGIT, LETTER, INT, FLOAT, ID, KEYWORD, OPERATOR, COMMA, EOI, INVALID = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

def typeToString (tp): #Adding Keywords
    if(tp == DIGIT): return "Digit"
    elif(tp == LETTER): return "Letter"
    elif (tp == INT): return "Int"
    elif (tp == FLOAT): return "Float"
    elif (tp == ID): return "ID"
    elif (tp == KEYWORD): return "Keyword"
    elif (tp == OPERATOR): return "Operator"
    elif (tp == EOI): return "EOI"
    return "Invalid"

class Token:
    "A class for representing Tokens"
    # a Token object has two fields: the token's type and its value
    def __init__ (self, tokenType, tokenVal):
        self.type = tokenType
        self.val = tokenVal

    def getTokenType(self):
        return self.type

    def getTokenValue(self):
        return self.val

    def __repr__(self):
        if (self.type in [INT, FLOAT, ID]): #updating type list
            return self.val
        elif (self.type == KEYWORD):
            return "SELECT" or "FROM" or "WHERE" or "AND"
        elif (self.type == OPERATOR):
            return "=" or "<" or ">"
        elif (self.type == COMMA):
            return ","
        elif (self.type == EOI):
            return ""
        else:
            return "invalid"

LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
DIGITS = "0123456789"

class Lexer:
    # stmt is the current statement to perform the lexing;
    # index is the index of the next char in the statement
    def __init__ (self, s):
        self.stmt = s + "$"
        self.index = 0
        self.nextChar()

    def nextToken(self):
        while True:
            if self.ch.isalpha(): # is a letter
                id = self.consumeChars(LETTERS+DIGITS)
                return Token(ID, id)

            elif self.ch.isdigit():
                num = self.consumeChars(DIGITS)
                if self.ch != ".":
                    return Token(INT, num)
                num += self.ch
                self.nextChar()
                if self.ch.isdigit(): 
                    num += self.consumeChars(DIGITS)
                    return Token(FLOAT, num)
                else: 
                    return Token(INVALID, num)

            elif self.ch ==' ': 
                self.nextChar() #ignore whitespaces.

            elif self.ch ==',': #comma type
                self.nextChar()
                return Token(COMMA, "")

            elif self.ch == '=' or '<' or '>': #assignment operators
                self.nextChar()
                return Token(OPERATOR, "")

            elif self.ch == 'SELECT' or 'FROM' or 'WHERE' or 'AND': # defines keyword types
                self.nextChar()
                return Token(KEYWORD, "")

            elif self.ch =='$': #Expected end of instruction
                return Token(EOI,"")

            else:
                self.nextChar()
                return Token(INVALID, self.ch)

    def nextChar(self): #leave alone
        self.ch = self.stmt[self.index] 
        self.index = self.index + 1

    def consumeChars (self, charSet): #leave alone
        r = self.ch
        self.nextChar()
        while (self.ch in charSet):
            r = r + self.ch
            self.nextChar()
        return r

    def checkChar(self, c): #leave alone
        self.nextChar()
        if (self.ch==c):
            self.nextChar()
            return True
        else: return False

import sys

class Parser:
    def __init__(self, s):
        self.lexer = Lexer(s + "$")# statement and EOI
        self.token = self.lexer.nextToken()

    def run(self):
        self.Query() # start symbol
    
    def next(self):
        self.token = self.lexer.nextToken()

    def Query(self): # Defines different types of queries using KEYWORD types
    # <Query> -> SELECT <IDList> FROM <IDList> [WHERE <CondList>]
        print "<Query>"
        if self.token.getTokenValue() == "SELECT":
            print ("\t<Keyword>" + self.token.getTokenValue() + "</Keyword>")
            #self.token = self.lexer.nextToken() # TEST 2/10/20
            self.next() #self.next is just self.lexer.nextToken(). Its just above line
            self.IDList()
        else:
            self.error(KEYWORD)

        if self.token.getTokenValue() == 'FROM':
            print ("\t<Keyword>" + self.token.getTokenValue() + "</Keyword>")
            #self.token = self.lexer.nextToken() # TEST 2/10/20
            self.next()
            self.IDList()
        else: 
            self.error(KEYWORD)

        if self.token.getTokenValue() == 'WHERE': # optional
            print ("\t<Keyword>" + self.token.getTokenValue() + "</Keyword>")
            #self.token = self.lexer.nextToken() # already was there for where.
            self.next()
            self.CondList()
            #self.match(EOI)
            print ("</Query>")
        else:
            self.match(EOI)
            print ("</Query>")
            #self.error(KEYWORD)
    
    def IDList(self): # defines rules for ID in E-BNF
    # <IDList> -> <id> {, <id>}
        print ("\t<IDList>")
        if self.token.getTokenType() == ID:
            print ("\t\t<ID>" + self.token.getTokenValue() + "</ID>")
            self.next()

        while self.token.getTokenType() == COMMA:
            print ("\t\t<Comma>,</Comma>")
            self.next()
            if self.token.getTokenType() == ID:
                print ("\t\t<ID>" + self.token.getTokenValue() + "</ID>")
                self.next()
            else:
                self.error(ID)
        print ("\t</IDList>")
    
    def CondList(self): # defines rules for conditions
    # <CondList> -> <Cond> {AND <Cond>}
        print ("\t<CondList>") 
        if self.token.getTokenType() == ID: # Only Cond case
            self.Cond()
        else:
            self.error(self.token.getTokenType())
        
        while self.token.getTokenValue == 'AND': # Cond with AND case
            print ("\t<Keyword>" + self.token.getTokenValue() + "</Keyword>")
            self.next()

            if self.token.getTokenType() == ID:
                self.Cond()
            else:
                self.error(self.token.getTokenType())
        print ("\t</CondList>")

    def Cond(self): # sets up conditional expressions
    # <Cond> -> <id> <operator> <Term>
        print ("\t\t<Cond>")
        if not (self.token.getTokenType() == ID):
            self.error(self.token.getTokenType())
        else:
            print ("\t\t\t<ID>" + self.token.getTokenValue() + "</ID>")
            self.next()
            if not (self.token.getTokenType() == OPERATOR):
                self.error(self.token.getTokenType())
            else:
                print ("\t\t\t<Operator>=</Operator>")
                self.next()
                self.Term() 
        print ("\t\t</Cond>")

    def Term(self): # defines ruleset for terms
    # <Term> -> <id> | <int> | <float>
        print ("\t\t\t<Term>")
        if self.token.getTokenType() == ID:
            print ("\t\t\t\t<ID>" + self.token.getTokenValue() + "</ID>")
        elif self.token.getTokenType() == INT:
            print ("\t\t\t\t<Int>" + self.token.getTokenValue() + "</Int>")
        elif self.token.getTokenType() == FLOAT:
            print ("\t\t\t\t<Float>" + self.token.getTokenValue() + "</Float>")
        else:
            print ("Syntax error: expecting an ID, an int, or a float" + "; saw:" + typeToString(self.token.getTokenType()))
            sys.exit(1)
        print ("\t\t\t</Term>")

    def match (self, tp):
        val = self.token.getTokenValue()
        if (self.token.getTokenType() == tp):
            self.token = self.lexer.nextToken()
        else: self.error(tp)
        return val

    def error(self, tp): # hits this error every time.
        print "Syntax error: expecting: " + typeToString(tp) + "; saw: " + typeToString(self.token.getTokenType())
        sys.exit(1)


# PARSER TESTBENCH
print "START PARSER TESTBENCH\n"
print "Testcase from Assignment" # WORKING
parser = Parser ("SELECT C1,C2 FROM T1 WHERE C1=5.23")
parser.run()

print "\n Testcase 1."
parser = Parser ("SELECT C1, C2 FROM T6 WHERE C2=6.23")
parser.run()

print("\nEND PARSER TESTBENCH")