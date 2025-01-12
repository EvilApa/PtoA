//Antonio Rodriguez and Gerardo Zertuche
//CS 4301
//Stage 1

#include <stage1.h>
#include <iostream>
#include <ctime>
#include <string>
#include <cctype>
#include <iomanip>
#include <unordered_map>
#include <functional>

using namespace std;

static bool hasErrorBeenFound = false;

Compiler::Compiler(char** argv) {
    sourceFile.open(argv[1]);
    listingFile.open(argv[2]);
    objectFile.open(argv[3]);

    if (!sourceFile) processError("Cannot open source file.");
    if (!listingFile) processError("Cannot open listing file.");
    if (!objectFile) processError("Cannot open object file.");
}
Compiler::~Compiler() {
    sourceFile.close();
    listingFile.close();
    objectFile.close();
}
void Compiler::createListingHeader(){
    time_t result = time(nullptr);
	listingFile << "STAGE1:          " << "Antonio Rodriguez, Gerardo Zertuche         " << ctime(&result) << endl;
	listingFile << "LINE NO.              " << "SOURCE STATEMENT" << "\r\n";
	listingFile << endl;
}
void Compiler::parser() {
	nextChar();
	nextToken();
	if(token != "program"){
		processError("keyword \"program\" expected");
	}
	prog();
}
void Compiler::createListingTrailer(){
	if(hasErrorBeenFound == true){
    	listingFile << "COMPILATION TERMINATED      " << "1"  << " ERROR ENCOUNTERED" << endl;
	}
	else {
		listingFile << "COMPILATION TERMINATED      " << "0"  << " ERRORS ENCOUNTERED" << endl;
	}
}
void Compiler:: processError(string err) {
    //output error to listing file and call exit()
    listingFile << '\n' << "Error: Line " << lineNo << ": " << err << endl;
	listingFile << endl;
	hasErrorBeenFound = true;
	createListingTrailer();
    exit(0);
}
char Compiler::nextChar() {
    sourceFile.get(ch);
    static char prevCh = '\n';

    if (sourceFile.eof()) ch = END_OF_FILE;

    if (ch != END_OF_FILE) {
        if (prevCh == '\n') {
            lineNo++;
            listingFile << setw(5) << right << lineNo << "|";
        }
        listingFile << ch;
    } else {
        listingFile << endl;
    }
    prevCh = ch;
    return ch;
}
string Compiler::nextToken() {
    token.clear();
    while (token.empty()) {
        if (ch == '{') { 
            // Skip comment logic
            while (ch != END_OF_FILE && ch != '}') nextChar();
            if (ch == END_OF_FILE) processError("unexpected end of file");
            nextChar();
        } else if (isspace(ch)) {
            nextChar();
        } else if (isSpecialSymbol(ch)) {
            // Handle special symbol logic
            token = ch;
            nextChar();
            if (token == ":") {
                if (ch == '=') {
                    token += ch;
                    nextChar();
                }
            } else if (token == "<") {
                if (ch == '=' || ch == '>') {
                    token += ch;
                    nextChar();
                }
            } else if (token == ">") {
                if (ch == '=') {
                    token += ch;
                    nextChar();
                }
            }
        } else if (islower(ch)) {
            // Handle identifier or keyword logic
            token = ch;
            nextChar();
            while (isalnum(ch) || ch == '_') {
                token += ch;
                nextChar();
            }
            if (!isNonKeyId(token) && !isKeyword(token)) {
                processError("invalid identifier format");
            }
        } else if (isdigit(ch)) {
            // Handle numeric literal logic
            token = ch;
            nextChar();
            while (isdigit(ch)) {
                token += ch;
                nextChar();
            }
        } else if (ch == END_OF_FILE) {
            token = ch;
        } else {
            processError("illegal symbol");
        }
    }
    return token;
}
bool Compiler::isKeyword(string s) const {
    const string keywords[] = {"program", "const", "var", "integer", "boolean",
                               "begin", "end", "true", "false", "not",
                               "and", "or", "mod", "div", "read", "write"};
    for (const auto& keyword : keywords) {
        if (s == keyword) {
            return true;
        }
    }
    return false;
}

bool Compiler::isSpecialSymbol(char c) const {
    const char specialSymbols[] = {':', ',', ';', '=', '+', '-', '.', '(', ')', '*', '>', '<'};
    for (const auto& symbol : specialSymbols) {
        if (c == symbol) {
            return true;
        }
    }
    return false;
}
bool Compiler :: isNonKeyId(string s) const {
	if(islower(s[0]) == false){
		return false;
	}
	if(isKeyword(s)){
		return false;
	}
	for(uint i = 0; i < s.length(); i++){
		if(isdigit(s[i]) == false && islower(s[i]) == false && s[i] != '_'){
				return false;
		}
	}
	if(s.length() >= 15){
		if(s[14] == '_'){
			return false;
		}
	}
	int uscoreCount = 0;
    for(uint i = 0; i < s.length(); i++){
		if(isSpecialSymbol(s[i])){
			if(s[i] == '_' && uscoreCount < 1){
				uscoreCount++;
			}
			else{
				return false;
			}
		}
	}
	return true;
}
bool Compiler :: isInteger(string s) const {
	if( s == "integer")
		return true;
	for(uint i = 0; i < s.length() - 1; i++){
		if(!isdigit(s[i]) && s[i] != '-' && s[i] != '+'){
			return false;
		}
	}
	return true;
}
bool Compiler :: isBoolean(string s) const{
	if(s == "boolean")
		return true;
    if (s == "true" || s == "false") {
        return true;
    }
    else {
        return false;
    }
}
bool Compiler :: isLiteral(string s) const {
    if(isBoolean(s) == false && isInteger(s) == false){
		return false;
	}
	return true;
}
string Compiler::ids()
{
	string temp, tempString;
	if (!isNonKeyId(token)) {
		processError("non-keyword identifier expected");
	}
	tempString = token;
	temp = token;
	if (nextToken() == ",") {
		if (!isNonKeyId(nextToken())) {
			processError("non-keyword identifier expected");
		}
		tempString = temp + "," + ids();
	}
	return tempString;
}
storeTypes Compiler::whichType(string name) {
    if (isLiteral(name) && !isNonKeyId(name)) {
        return isBoolean(name) ? BOOLEAN : INTEGER;
    }

    if (symbolTable.count(name) == 0) {
        processError("reference to undefined symbol: " + name);
    }

    return symbolTable.at(name).getDataType();
}
string Compiler::whichValue(string name) {
    if (isLiteral(name)) {
        return name;
    }

    if (symbolTable.count(name) == 0) {
        processError("reference to undefined constant: " + name);
    }

    return symbolTable.at(name).getValue();
}
void Compiler::insert(string externalName, storeTypes inType, modes inMode, string inValue, allocation inAlloc, int inUnits) {
    unsigned int pos = 0;
    string name;

    while (pos < externalName.length()) {
        name.clear();

        // Extract a single name from the comma-separated list
        while (pos < externalName.length() && externalName[pos] != ',') {
            name += externalName[pos];
            pos++;
        }
        pos++; // Skip the comma

        name = name.substr(0, 15); // Truncate to 15 characters

        // Check for invalid names and duplicates
        if (symbolTable.count(name) > 0) {
            processError("symbol " + name + " is multiply defined");
        } else if (isKeyword(name) && name != "true" && name != "false") {
            processError("illegal use of keyword");
        } else if (name == "TRUE" || name == "FALSE") {
            // Handle special cases for Boolean constants
            symbolTable.insert({
                name == "TRUE" ? "true" : "false",
                SymbolTableEntry(name, inType, inMode, inValue, inAlloc, inUnits)
            });
        } else {
            string internalName = isupper(name[0]) ? name : genInternalName(inType);
            symbolTable.insert({ name, SymbolTableEntry(internalName, inType, inMode, inValue, inAlloc, inUnits) });
        }

        // Ensure symbol table size limit is not exceeded
        if (symbolTable.size() > 256) {
            processError("symbol table cannot exceed 256 entries");
        }
    }
}
void Compiler::constStmts()
{
	string x,y;
	if (!isNonKeyId(token)) {
		processError("non-keyword identifier expected");
	}
	x = token;
	if (nextToken() != "=") {
		if (token == ",") {
			processError("no lists of constants allowed");
		}
		if (isNonKeyId(token)) {
			processError("no spaces in a variable name");
		}
		processError("\"=\" expected");
	}
	y = nextToken();
	if (y != "+" && y != "-" && y != "not" && !isNonKeyId(y) && y != "true" && y != "false" && whichType(y) != INTEGER) {
		processError("token to right of \"=\" illegal");
	}
	if (y == "+" || y == "-") {
		if (whichType(nextToken()) != INTEGER) {
			processError("integer expected after sign");
		}
		y = y + token;
	}
	if (y == "not") {
		if (whichType(nextToken()) != BOOLEAN) {
			processError("boolean expected after \"not\"");
		}
		if (token == "true") {
			y = "false";
		}
		else {
			y = "true";
		}
	}
	if (nextToken() != ";") {
		processError("semicolon expected");
	}
	if (whichType(y) != INTEGER && whichType(y) != BOOLEAN) {
		processError("data type of token on the right-hand side must be INTEGER or BOOLEAN");
	}
	insert(x, whichType(y), CONSTANT, whichValue(y), YES, 1);
	x = nextToken();
	if (x != "begin" && x != "var" && !isNonKeyId(x)) {
		processError("non-keyword identifier, \"begin\", or \"var\" expected");
	}
	if (isNonKeyId(x)) {
		constStmts();
	}

}
void Compiler :: consts() {
	if(token != "const"){
		processError("keyword \"const\" expected");
	}
	if(isNonKeyId(nextToken()) == false){
		processError("non-keyword identifier must follow \"const\"");
	}
	constStmts();
}
void Compiler :: vars(){
	if(token != "var"){
		processError("keyword \"var\" expected");
	}
	if(isNonKeyId(nextToken()) == false){
		processError("non-keyword identifier must follow \"var\"");
	}
	varStmts();
}
void Compiler::prog()
{
	if (token != "program") {
		processError("keyword \"program\" expected");
	}
	progStmt();
	if (token == "const") {
		consts();
	}
	if (token == "var") {
		vars();
	}
	if (token != "begin") {
		processError("keyword \"begin\" expected");
	}
	beginEndStmt();
	if (token != "$") { // ?
		processError("no text may follow \"end\"");
	}
}
void Compiler::progStmt()
{
	string x;
	if (token != "program") {
		processError("keyword \"program\" expected");
	}
	x = nextToken();
	if (isNonKeyId(token) == false) {
		processError("program name expected");
	}
	nextToken();
	if (token != ";") {
		processError("semicolon expected");
	}
	nextToken();
	code("program", x);
	insert(x, PROG_NAME, CONSTANT, x, NO, 0);
}
void Compiler::beginEndStmt() // token should be "begin"
{
	if (token != "begin") {
		processError("keyword \"begin\" expected");
	}
	nextToken();
	if(isNonKeyId(token) || token == "read" || token == "write") 
	{
		execStmts();
	}
	if (token != "end" && !isNonKeyId(token) && token != "read" && token != "write")
		processError("keyword \"end\" , NON_KEY_ID, \"read\", or \"write\" expected");
	if (nextToken() != ".") {
		processError("period expected");
	}
	nextToken();
	code("end", ".");
}
void Compiler :: varStmts(){
	string x, y;
	if(isNonKeyId(token) == false){
		processError("non-keyword identifier expected");
	}
	x = ids();
	if(token != ":"){
		processError("\":\" expected");
	}
	nextToken();
	if(isInteger(token) == false && isBoolean(token) == false){
		processError("illegal type follows \":\"");
	}
	y = token;
	if(nextToken() != ";"){
		processError("semicolon expected");
	}
	insert(x,whichType(y),VARIABLE,"",YES,1);
	nextToken();
	if(token != "begin" &&  isNonKeyId(token) == false){
		processError("(non-keyword identifier or \"begin\" expected");
	}
	if(isNonKeyId(token)){
		varStmts();
	}
}
void Compiler::code(string op, string operand1, string operand2) {
    static const std::unordered_map<string, std::function<void(string, string)>> operationMap = {
        {"program", [&](string op1, string op2) { emitPrologue(op1); }},
        {"end", [&](string op1, string op2) { emitEpilogue(); }},
        {"read", [&](string op1, string op2) { emitReadCode(op1, op2); }},
        {"write", [&](string op1, string op2) { emitWriteCode(op1, op2); }},
        {"+", [&](string op1, string op2) { emitAdditionCode(op1, op2); }},
        {"-", [&](string op1, string op2) { emitSubtractionCode(op1, op2); }},
        {"neg", [&](string op1, string op2) { emitNegationCode(op1, op2); }},
        {"not", [&](string op1, string op2) { emitNotCode(op1, op2); }},
        {"*", [&](string op1, string op2) { emitMultiplicationCode(op1, op2); }},
        {"div", [&](string op1, string op2) { emitDivisionCode(op1, op2); }},
        {"mod", [&](string op1, string op2) { emitModuloCode(op1, op2); }},
        {"and", [&](string op1, string op2) { emitAndCode(op1, op2); }},
        {"or", [&](string op1, string op2) { emitOrCode(op1, op2); }},
        {"=", [&](string op1, string op2) { emitEqualityCode(op1, op2); }},
        {":=", [&](string op1, string op2) { emitAssignCode(op1, op2); }},
        {"<>", [&](string op1, string op2) { emitInequalityCode(op1, op2); }},
        {"<", [&](string op1, string op2) { emitLessThanCode(op1, op2); }},
        {"<=", [&](string op1, string op2) { emitLessThanOrEqualToCode(op1, op2); }},
        {">", [&](string op1, string op2) { emitGreaterThanCode(op1, op2); }},
        {">=", [&](string op1, string op2) { emitGreaterThanOrEqualToCode(op1, op2); }}
    };

    auto it = operationMap.find(op);
    if (it != operationMap.end()) {
        it->second(operand1, operand2); // Call the corresponding function
    } else {
        processError("compiler error since function code should not be called with illegal arguments");
    }
}
void Compiler::emit(string label, string instruction, string operands, string comment) {
    objectFile << left << setw(8) << label;
    objectFile << left << setw(8) << instruction;
    objectFile << left << setw(24) << operands;
    objectFile << left << comment << endl;
}

void Compiler::emitPrologue(string progName, string operand2) {
    time_t result = time(nullptr);
    objectFile << "; ANTONIO RODRIGUEZ, GERARDO ZERTUCHE " << ctime(&result);
    objectFile << "%INCLUDE \"Along32.inc\"" << endl
               << "%INCLUDE \"Macros_Along.inc\"" << endl
               << endl;

    emit("SECTION", ".text");
    emit("global", "_start", "", "; program " + progName);
    objectFile << endl;
    emit("_start:");
}
void Compiler::emitEpilogue(string operand1, string operand2) {
    emit("", "Exit", "{0}", "");
    objectFile << endl;
    emitStorage();
}
void Compiler::emitStorage() {
    emit("SECTION", ".data", "", "");
    for (auto it = symbolTable.cbegin(); it != symbolTable.cend(); ++it) {
        if ((*it).second.getAlloc() == YES && (*it).second.getMode() == CONSTANT) {
            if ((*it).second.getValue() == "false") {
                emit((*it).second.getInternalName(), "dd", "0", "; " + (*it).first);
            } else if ((*it).second.getValue() == "true") {
                emit((*it).second.getInternalName(), "dd", "-1", "; " + (*it).first);
            } else if (isNonKeyId((*it).second.getValue())) {
                emit((*it).second.getInternalName(), "dd", "-1", "; " + (*it).first);
            } else {
                emit((*it).second.getInternalName(), "dd", (*it).second.getValue(), "; " + (*it).first);
            }
        }
    }

    objectFile << endl;

    emit("SECTION", ".bss", "", "");
    for (auto it = symbolTable.cbegin(); it != symbolTable.cend(); ++it) {
        if ((*it).second.getAlloc() == YES && (*it).second.getMode() == VARIABLE) {
            emit((*it).second.getInternalName(), "resd", "1", "; " + (*it).first);
        }
    }
}
static int boolCount = 0;
static int integerCount = 0;
string Compiler :: genInternalName(storeTypes stype) const{

	string internalName;

	if(stype == INTEGER){
		internalName =  "I" + to_string(integerCount);
		integerCount++;
	}
	if(stype == BOOLEAN){
		internalName = "B" + to_string(boolCount);
		boolCount++;
	}
    if(stype == PROG_NAME){
        internalName = "P" + to_string(0);
    }

	return internalName;
}
void Compiler::pushOperand(string name) {
    // Insert literal into symbol table if not already present
    if (isLiteral(name) && symbolTable.count(name) == 0) {
        if (name == "true") {
            insert("TRUE", BOOLEAN, CONSTANT, name, YES, 1);
        } else if (name == "false") {
            insert("FALSE", BOOLEAN, CONSTANT, name, YES, 1);
        } else {
            insert(name, whichType(name), CONSTANT, name, YES, 1);
        }
    } else if (!isLiteral(name) && symbolTable.count(name) == 0) {
        processError("reference to undefined symbol: " + name);
    }

    // Push the operand onto the stack
    operandStk.push(name);
}
string Compiler::popOperand() {
    if (operandStk.empty()) {
        processError("compiler error: operand stack underflow");
    }

    string topElement = operandStk.top();
    operandStk.pop();
    return topElement;
}
void Compiler :: pushOperator(string op){
    operatorStk.push(op);
}
string Compiler::popOperator() {
    if (operatorStk.empty()) {
        processError("compiler error: operator stack underflow");
    }

    string topElement = operatorStk.top();
    operatorStk.pop();
    return topElement;
}
void Compiler::part() {
    if (token == "not") {
        nextToken();
        if (token == "(") {
            nextToken();
            express();
            if (token != ")") processError("no closing parentheses to match opening.");
            code("not", popOperand());
            nextToken();
        } else if (isBoolean(token)) {
            pushOperand(token == "true" ? "false" : "true");
            nextToken();
        } else if (isNonKeyId(token)) {
            code("not", token);
            nextToken();
        } else {
            processError("Expected NON_KEY_ID, '(' or boolean literal after 'not'");
        }
    } else if (token == "+" || token == "-") {
        string op = token;
        nextToken();
        if (token == "(") {
            nextToken();
            express();
            if (token != ")") processError("no closing parentheses to match opening.");
            if (op == "-") code("neg", popOperand());
            nextToken();
        } else if (isInteger(token) || isNonKeyId(token)) {
            pushOperand(op == "-" ? "-" + token : token);
            nextToken();
        } else {
            processError("must have an integer value after '+' or '-'");
        }
    } else if (token == "(") {
        nextToken();
        express();
        if (token != ")") processError("no closing parentheses to match opening.");
        nextToken();
    } else if (isInteger(token) || isBoolean(token) || isNonKeyId(token)) {
        pushOperand(token);
        nextToken();
    } else {
        processError("Invalid factor in expression");
    }
}
void Compiler::factors() {
    while (token == "*" || token == "div" || token == "mod" || token == "and") {
        pushOperator(token);  // Push the operator onto the stack
        nextToken();          // Move to the next token

        part();               // Parse the next part

        // Pop operator and operands to generate code
        string popOprt = popOperator();
        string lhs = popOperand();
        string rhs = popOperand();
        code(popOprt, lhs, rhs);
    }
}
void Compiler::express() {
    // Parse the first term
    term();

    // Handle additional expressions (e.g., relational operators)
    expresses();
}
void Compiler::factor() {
    part();     // Parse the first part
    factors();  // Handle additional factors (e.g., multiplication, division)
}
void Compiler::term() {
    // Parse the first factor
    factor();

    // Handle additional terms (e.g., addition, subtraction)
    terms();
}
void Compiler::terms() {
    while (token == "+" || token == "-" || token == "or") {
        pushOperator(token);  // Push the operator onto the stack
        nextToken();          // Move to the next token

        factor();             // Parse the next factor

        // Pop operator and operands to generate code
        string popOprt = popOperator();
        string lhs = popOperand();
        string rhs = popOperand();
        code(popOprt, lhs, rhs);
    }
}
void Compiler::expresses() {
    while (token == "=" || token == "<>" || token == "<=" || token == ">=" || token == "<" || token == ">") {
        pushOperator(token);  // Push the operator onto the stack
        nextToken();          // Move to the next token

        term();               // Process the next term

        // Pop operator and operands to generate code
        string popOprt = popOperator();
        string lhs = popOperand();
        string rhs = popOperand();
        code(popOprt, lhs, rhs);
    }
}
void Compiler :: writeStmt(){

	string writeOut, temp;
    temp = "temp";
    if(token != "write")
        processError("Keyword \"write\" expected, recieved: " + token);
    nextToken();
    if(token!= "(")
        processError("Token '(' not found, required for write statements");
    nextToken();
    if(isNonKeyId(token) == false)
        processError("NON_KEY_ID expected, recieved: " +token);
    writeOut = ids();

    if(token!=")")
        processError("Token ')' not found, must have closing ')' for write statements found: " + token);
    nextToken();
    if(token!=";")
        processError("Token ';' not found, must have for concluding statements");
    uint i = 0;
    while(temp != "")
    {
        temp = "";
        while(writeOut[i] != ',' && i < writeOut.length())
        {
            temp += writeOut[i];
            i++;
        }
        i++;
        if(temp != "")
            code("write", temp);
    }
}
void Compiler::readStmt() {
    if (token != "read") processError("Keyword \"read\" expected");
    nextToken();

    if (token != "(") processError("Token '(' expected for read statement");
    nextToken();

    if (!isNonKeyId(token)) processError("NON_KEY_ID expected in read statement");
    string idsToRead = ids();

    if (token != ")") processError("Token ')' expected after identifiers");
    nextToken();

    if (token != ";") processError("Token ';' expected after read statement");

    // Inline logic to extract identifiers and emit code for each
    size_t i = 0;
    string currentId;
    while (i < idsToRead.length()) {
        currentId.clear();
        while (i < idsToRead.length() && idsToRead[i] != ',') {
            currentId += idsToRead[i];
            i++;
        }
        i++; // Skip the comma
        if (!currentId.empty()) {
            code("read", currentId);
        }
    }
}
void Compiler :: assignStmt(){

		pushOperand(token);
		nextToken();

		if(token != ":="){
			processError("cannot have an assignment statment without \" := \" ");
		}
		pushOperator(token);
		nextToken();
		if(token != "not" && token != "+" && token != "-" && isNonKeyId(token) == false && token != "(" && isLiteral(token) == false){
			processError("error, illegal token in assignment statement");
		}
		express();
		if(token != ";"){
			processError("one of \"*\", \"and\", \"div\", \"mod\", \")\", \"+\", \"-\", \";\", \"<\", \"<=\", \"<>\", \"=\", \">\", \">=\", or \"or\" expected");
		}
		
		string popOprt, lhs, rhs;
		popOprt = popOperator();
		lhs = popOperand();
		rhs = popOperand();
		code(popOprt, lhs, rhs);
}
void Compiler :: execStmt(){
	if(token == "read"){
		readStmt();
	}
	else if(token == "write"){
		writeStmt();
	}
	else if (isNonKeyId(token)){ 
		assignStmt();
	}
	else{
		processError("non_key_id, \"read\", or \"write\" expected");
	}
}
void Compiler :: execStmts(){
	while(token != "end"){
		execStmt();
		nextToken();
	}
}
void Compiler :: freeTemp(){
	currentTempNo--;
	if(currentTempNo < -1){
		processError("compiler error, current temp should not be less than <= -1");
	}
}
string Compiler:: getTemp(){
	string temp;
	currentTempNo++;
	temp = "T" + to_string(currentTempNo);
	if (currentTempNo > maxTempNo){
		insert(temp, UNKNOWN, VARIABLE, "", NO, 1);
		maxTempNo++;
	}

	return temp;
}
bool Compiler :: isTemporary(string s) const {
	if(s[0] == 'T'){
		return true;
	}
	else {
		return false;
	}
}
void Compiler::emitReadCode(string operand, string operand2) {
    string name;
    unsigned int nameOfCurrentList = 0;
    while (nameOfCurrentList < operand.length()) {
        name = "";
        while (name.empty()) {
            while (nameOfCurrentList < operand.length() && operand[nameOfCurrentList] != ',') {
                name += operand[nameOfCurrentList];
                nameOfCurrentList++;
            }
            nameOfCurrentList++;

            name = name.substr(0, 15);

            if (symbolTable.count(name) == 0) {
                processError("reference to undefined symbol");
            } else if (whichType(name) == BOOLEAN) {
                processError("can't read variables of this type");
            } else if (symbolTable.at(name).getMode() != VARIABLE) {
                processError("attempting to read to a read-only location");
            } else {
                emit("", "call", "ReadInt", "; read int; value placed in eax");
                emit("", "mov", "[" + symbolTable.at(name).getInternalName() + "],eax", "; store eax at " + operand);
                contentsOfAReg = name;
            }
        }
    }
}
void Compiler::emitWriteCode(string operand, string operand2) {
    string name;
    unsigned int nameOfCurrentOperand = 0;
    while (nameOfCurrentOperand < operand.length()) {
        name = "";
        while (name.empty()) {
            while (nameOfCurrentOperand < operand.length() && operand[nameOfCurrentOperand] != ',') {
                name += operand[nameOfCurrentOperand];
                nameOfCurrentOperand++;
            }
            nameOfCurrentOperand++;

            name = name.substr(0, 15);

            if (symbolTable.count(name) == 0) {
                processError("reference to undefined symbol");
            }
            if (name != contentsOfAReg) {
                emit("", "mov", "eax,[" + symbolTable.at(name).getInternalName() + "]", "; load " + name + " in eax");
                contentsOfAReg = name;
            }
            if (whichType(name) == INTEGER || whichType(name) == BOOLEAN) {
                emit("", "call", "WriteInt", "; write int in eax to standard out");
            }
            emit("", "call", "Crlf", "; write \\r\\n to standard out");
        }
    }
}
void Compiler::emitAssignCode(string operand1, string operand2) {
    if (whichType(operand1) != whichType(operand2)) {
        processError("incompatible types for operator ':='");
    }

    if (symbolTable.at(operand2).getMode() != VARIABLE) {
        processError("symbol on left-hand side of assignment must have a storage mode of VARIABLE");
    }

    if (operand1 == operand2) return; // No operation needed

    if (operand1 != contentsOfAReg) {
        emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
        contentsOfAReg = operand1;
    }

    emit("", "mov", "[" + symbolTable.at(operand2).getInternalName() + "],eax", "; " + operand2 + " = AReg");
    contentsOfAReg = operand2;

    if (isTemporary(operand1)) freeTemp();
}
void Compiler::emitAdditionCode(string operand1, string operand2) {
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary '+' requires integer operands");
    }

    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }

    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
        contentsOfAReg = operand2;
    }

    emit("", "add", "eax,[" + symbolTable.at(contentsOfAReg == operand1 ? operand2 : operand1).getInternalName() + "]",
         "; AReg = " + operand1 + " + " + operand2);

    if (isTemporary(operand1)) freeTemp();
    if (isTemporary(operand2)) freeTemp();

    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler::emitSubtractionCode(string operand1, string operand2) {
    if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
        processError("binary '-' requires integer operands");
    }

    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand2) {
        emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }

    if (contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
        contentsOfAReg = operand2;
    }

    emit("", "sub", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " - " + operand1);

    if (isTemporary(operand1)) freeTemp();
    if (isTemporary(operand2)) freeTemp();

    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(INTEGER);
    pushOperand(contentsOfAReg);
}
void Compiler:: emitMultiplicationCode(string operand1, string operand2) {

   if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
      processError("Only integers may be used with '*' operator");
   }
   if (isTemporary(contentsOfAReg) == true && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" +symbolTable.at(contentsOfAReg).getInternalName() + "],eax", "; deassign AReg" ); 
		symbolTable.at(contentsOfAReg).setAlloc(YES); 
		contentsOfAReg = ""; 
   }
   if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
      contentsOfAReg = ""; 
   }
   if (contentsOfAReg != operand1 && contentsOfAReg != operand2) { 
      emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2); 
	  contentsOfAReg = operand2;
   }
	if(contentsOfAReg == operand1){
		emit("", "imul", "dword [" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " * " + operand2);
	}
	else {
		emit("", "imul", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " * " + operand1);
	}
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitDivisionCode(string operand1, string operand2) {
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
		processError("Only integers may be used with 'div'");
	}
	if (isTemporary(contentsOfAReg) == true && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = ""; 
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand2) {
		contentsOfAReg = ""; 
	}
	if (contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	}
	emit("","cdq","","; sign extend dividend from eax to edx:eax");
	emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitModuloCode(string operand1, string operand2) {
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
		processError("Only integers may be used with 'mod'");
	}
	if (isTemporary(contentsOfAReg) == true && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; fill this in");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand2) {
		contentsOfAReg = "";
	}
	if (contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	}
	emit("","cdq","","; sign extend dividend from eax to edx:eax");
	emit("", "idiv", "dword [" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " div " + operand1);
	emit("", "xchg", "eax,edx", "; exchange quotient and remainder");
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitNegationCode(string operand1, string nothing){
	if (whichType(operand1) != INTEGER) {
		processError("Only integer values may be used with negation.");
	}
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES); 
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand1) {
		contentsOfAReg = ""; 
	}
	if(contentsOfAReg != operand1){
		emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
		contentsOfAReg = operand1;
	}
	emit("", "neg ", "eax", "; AReg = -AReg");
	if(isTemporary(operand1)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(INTEGER);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitNotCode(string operand1, string nothing){
	if (whichType(operand1) != BOOLEAN) {
		processError("Unary 'not' may only be used with boolean operand");
	}
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1) {
		emit("", "mov","[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES); // change the allocation to YES
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand1) {
		contentsOfAReg = ""; // deassign it
	}
	if(contentsOfAReg != operand1){
		emit("", "mov", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand1);
		contentsOfAReg = operand1;
	}
		emit("", "not", "eax", "; AReg = !AReg");
		//if operands are temporaroy deassign them
	if(isTemporary(operand1)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg); 
}
void Compiler::emitEqualityCode(string operand1, string operand2) {
    if (whichType(operand1) != whichType(operand2)) {
        processError("Equality operator requires matching operand types");
    }

    string label1 = getLabel();
    string label2 = getLabel();

    if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
        symbolTable.at(contentsOfAReg).setAlloc(YES);
        contentsOfAReg = "";
    }

    if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
        emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
    }

    if (contentsOfAReg == operand1) {
        emit("", "cmp", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; compare " + operand1 + " and " + operand2);
    } else {
        emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
    }

    emit("", "je", label1, "; if equal, jump to TRUE");
    emit("", "mov", "eax,[FALSE]", "; set AReg to FALSE");
    emit("", "jmp", label2, "; jump to end");
    emit(label1 + ":", "mov", "eax,[TRUE]", "; set AReg to TRUE");
    emit(label2 + ":", "", "", "");

    if (isTemporary(operand1)) freeTemp();
    if (isTemporary(operand2)) freeTemp();

    contentsOfAReg = getTemp();
    symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
    pushOperand(contentsOfAReg);
}
void Compiler :: emitInequalityCode(string operand1, string operand2) {
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();
	if (whichType(operand1) != whichType(operand2)) {
		processError("Inequality operator must be used with matching types.");
	}
	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg"); 
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = ""; 
	}
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		contentsOfAReg = ""; 
	}
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
	}
	emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1); 
	emit("", "jne", label1, "; if " + operand2 + " <> " + operand1 +" then jump to set eax to TRUE"); 
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE"); 
	emit("", "jmp", label2, "; unconditionally jump"); 
	emit(label1 + ":", "", "", ""); 
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE"); 
	emit(label2 + ":", "", "", "");
	if (symbolTable.count("true") == 0) {
		symbolTable.insert({ "true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1) });
	}
	if (symbolTable.count("false") == 0) {
		symbolTable.insert({ "false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1) });
	}
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitOrCode(string operand1, string operand2) {
	if (whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN) {
		processError("illegal type");
	}
	if (isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES); 
		contentsOfAReg = "";
	}
	if (isTemporary(contentsOfAReg) == false && contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		contentsOfAReg = "";
	}
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	if (contentsOfAReg == operand1) {
		emit("", "or", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " or " + operand2);
	}
	else {
		emit("", "or", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " or " + operand1);
	}
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitAndCode(string operand1, string operand2){
	if(whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN){
		processError("binary 'and' requires boolean operands");
	}
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov","[" + contentsOfAReg + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
	if(contentsOfAReg == operand1){
		emit("", "and ", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand1 + " and " + operand2);
	}
	else{
		emit("", "and ", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; AReg = " + operand2 + " and " + operand1);
	}
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitLessThanCode(string operand1, string operand2){
	string label1 = getLabel();
	string label2 = getLabel();

	if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER){
		processError("error only integers may be used with \"<\" ");
	}
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
		emit("", "cmp ", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
		emit("", "jl ", label1, "; if " + operand2 + " < " + operand1 + " then jump to set eax to TRUE");
		emit("", "mov ", "eax,[FALSE]", "; else set eax to FALSE");
		emit("", "jmp ", label2, "; unconditionally jump");
		emit(label1 + ":", "", "" ,"");
		emit("", "mov ", "eax,[TRUE]", "; set eax to TRUE");
		emit(label2 + ":", "", "" ,"");
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);

}
void Compiler :: emitLessThanOrEqualToCode(string operand1, string operand2){
	string label1 = getLabel();
	string label2 = getLabel();

	if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER){
		processError("error only integers may be used with \"<=\" ");
	}
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg" );
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		contentsOfAReg = "";
	}
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2);
		contentsOfAReg = operand2;
	}
		emit("", "cmp ", "eax,[" + symbolTable.at(operand1).getInternalName() + "]", "; compare " + operand2 + " and " + operand1);
		emit("", "jle ", label1, "; if " + operand2 + " <= " +operand1 + " then jump to set eax to TRUE" );
		emit("", "mov ", "eax,[FALSE]", "; else set eax to FALSE");
		emit("", "jmp ", label2, "; unconditionally jump");
		emit(label1 + ":", "", "" ,"");
		emit("", "mov ", "eax,[TRUE]", "; set eax to TRUE");
		emit(label2 + ":", "", "" ,"");
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
void Compiler :: emitGreaterThanCode(string operand1, string operand2) {
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
		processError("Only integers may be used with '>'");
	}
	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg"); 
		symbolTable.at(contentsOfAReg).setAlloc(YES); 
		contentsOfAReg = ""; 
	}
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		contentsOfAReg = ""; 
	}
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2); 
	}
	emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1); 
	emit("", "jg", label1, "; if " + operand2 + " > " + operand1 + " then jump to set eax to TRUE"); 
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE"); 
	emit("", "jmp", label2, "; unconditionally jump"); 
	emit(label1 + ":", "", "", ""); 
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE");
	emit(label2 + ":", "", "", "");
	if (symbolTable.count("true") == 0) {
		symbolTable.insert({ "true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1) });
	}
	if (symbolTable.count("false") == 0) {
		symbolTable.insert({ "false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1) });
	}
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg); 
}
void Compiler :: emitGreaterThanOrEqualToCode(string operand1, string operand2) {
	string label1, label2;
	label1 = getLabel();
	label2 = getLabel();
	if (whichType(operand1) != INTEGER || whichType(operand2) != INTEGER) {
		processError("Only integers may be used with '>='");
	}
	if (isTemporary(contentsOfAReg) && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		emit("", "mov", "[" + contentsOfAReg + "],eax", "; deassign AReg"); 
		symbolTable.at(contentsOfAReg).setAlloc(YES); 
		contentsOfAReg = ""; 
	}
	if (isTemporary(contentsOfAReg) == false && (contentsOfAReg != operand1 && contentsOfAReg != operand2)) {
		contentsOfAReg = ""; 
	}
	if (contentsOfAReg != operand1 && contentsOfAReg != operand2) {
		emit("", "mov", "eax,[" + symbolTable.at(operand2).getInternalName() + "]", "; AReg = " + operand2); 
		contentsOfAReg = operand2;
	}
	emit("", "cmp", "eax,[" + symbolTable.at(operand1).getInternalName() + "]","; compare " + operand2 + " and " + operand1); 
	emit("", "jge", label1, "; if " + operand2 + " >= " + operand1 + " then jump to set eax to TRUE"); 
	emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE"); 
	emit("", "jmp", label2, "; unconditionally jump"); 
	emit(label1 + ":", "", "", ""); 
	emit("", "mov", "eax,[TRUE]", "; set eax to TRUE"); 
	emit(label2 + ":", "", "", "");
	if (symbolTable.count("true") == 0) {
		symbolTable.insert({ "true", SymbolTableEntry("TRUE", BOOLEAN, CONSTANT, "-1", YES, 1) });
	}
	if (symbolTable.count("false") == 0) {
		symbolTable.insert({ "false", SymbolTableEntry("FALSE", BOOLEAN, CONSTANT, "0", YES, 1) });
	}
	if(isTemporary(operand1)){
		freeTemp();
	}
	if(isTemporary(operand2)){
		freeTemp();
	}
	contentsOfAReg = getTemp();
	symbolTable.at(contentsOfAReg).setDataType(BOOLEAN);
	pushOperand(contentsOfAReg);
}
string Compiler::getLabel() {
	static int labelCount = -1;
	string currentLabel;
	labelCount++; 
	currentLabel = ".L" + to_string(labelCount);
	return currentLabel; 
}

