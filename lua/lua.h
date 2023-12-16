class Lua {
public:

	Lua(const Bytecode& bytecode, const Ast& ast, const std::string& filePath, const bool& minimizeDiffs, const bool& unrestrictedAscii);
	~Lua();

	void operator()();

	const std::string filePath;

private:

	static constexpr char UTF8_BOM[] = "\xEF\xBB\xBF";
	static constexpr char NEW_LINE[] = "\r\n";

	void write_header();
	void write_block(const Ast::Function& function, const std::vector<Ast::Statement*>& block);
	void write_expression(const Ast::Expression& expression, const bool& useParentheses);
	void write_prefix_expression(const Ast::Expression& expression, const bool& isLineStart);
	void write_variable(const Ast::Variable& variable, const bool& isLineStart);
	void write_function_call(const Ast::FunctionCall& functionCall, const bool& isLineStart);
	void write_assignment(const std::vector<Ast::Variable>& variables, const std::vector<Ast::Expression*>& expressions, const std::string& seperator, const bool& isLineStart);
	void write_expression_list(const std::vector<Ast::Expression*>& expressions, const Ast::Expression* const& multres);
	void write_function_definition(const Ast::Function& function, const bool& isMethod);
	void write_number(const double& number);
	void write_string(const std::string& string);
	uint8_t get_operator_precedence(const Ast::Expression& expression);
	void write(const std::string& string);
	template <typename... Strings>
	void write(const std::string& string, const Strings&... strings);
	void write_indent();
	void create_file();
	void close_file();
	void write_file();

	const Bytecode& bytecode;
	const Ast& ast;
	const bool minimizeDiffs;
	const bool unrestrictedAscii;
	HANDLE file = INVALID_HANDLE_VALUE;
	std::string writeBuffer;
	uint32_t indentLevel = 0;
	uint64_t prototypeDataLeft = 0;
};
