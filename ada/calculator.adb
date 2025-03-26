with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Calculator is
   -- Define a map for variables
   package Variable_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Float,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   -- Token types
   type Token_Type is (Number, Identifier, Plus, Minus, Multiply, Divide, Equals, Left_Paren, Right_Paren, EOF);

   -- Token structure
   type Token is record
      Kind : Token_Type;
      Value : Unbounded_String;
      Number_Value : Float := 0.0;
   end record;

   -- Lexical analyzer
   type Lexer is record
      Input : Unbounded_String;
      Position : Natural := 1;
      Current_Token : Token;
   end record;

   -- Parser
   type Parser is record
      L : Lexer;
      Variables : Variable_Maps.Map;
   end record;

   -- Exception declarations
   Syntax_Error : exception;
   Undefined_Variable : exception;
   Divide_By_Zero : exception;

   -- Function to check if character is a digit
   function Is_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9';
   end Is_Digit;

   -- Function to check if character is a letter
   function Is_Letter (C : Character) return Boolean is
   begin
      return C in 'a' .. 'z' or C in 'A' .. 'Z';
   end Is_Letter;

   -- Function to get the current character from the lexer
   function Current_Char (L : Lexer) return Character is
   begin
      if L.Position <= Length (L.Input) then
         return Element (L.Input, L.Position);
      else
         return ASCII.NUL;
      end if;
   end Current_Char;

   -- Procedure to advance to the next character
   procedure Advance (L : in out Lexer) is
   begin
      L.Position := L.Position + 1;
   end Advance;

   -- Procedure to skip whitespace
   procedure Skip_Whitespace (L : in out Lexer) is
   begin
      while L.Position <= Length (L.Input) and then
            (Current_Char (L) = ' ' or
             Current_Char (L) = ASCII.HT or
             Current_Char (L) = ASCII.LF or
             Current_Char (L) = ASCII.CR) loop
         Advance (L);
      end loop;
   end Skip_Whitespace;

   -- Function to read a number
   function Read_Number (L : in out Lexer) return Token is
      Result : Token;
      Num_Str : Unbounded_String := To_Unbounded_String ("");
      Has_Decimal : Boolean := False;
   begin
      Result.Kind := Number;

      while L.Position <= Length (L.Input) and then
            (Is_Digit (Current_Char (L)) or
             (Current_Char (L) = '.' and not Has_Decimal)) loop
         if Current_Char (L) = '.' then
            Has_Decimal := True;
         end if;
         Append (Num_Str, Current_Char (L));
         Advance (L);
      end loop;

      Result.Value := Num_Str;
      Result.Number_Value := Float'Value (To_String (Num_Str));
      return Result;
   end Read_Number;

   -- Function to read an identifier
   function Read_Identifier (L : in out Lexer) return Token is
      Result : Token;
      Id_Str : Unbounded_String := To_Unbounded_String ("");
   begin
      Result.Kind := Identifier;

      while L.Position <= Length (L.Input) and then
            (Is_Letter (Current_Char (L)) or
             Is_Digit (Current_Char (L)) or
             Current_Char (L) = '_') loop
         Append (Id_Str, Current_Char (L));
         Advance (L);
      end loop;

      Result.Value := Id_Str;
      return Result;
   end Read_Identifier;

   -- Function to get the next token from the input
   procedure Next_Token (L : in out Lexer) is
   begin
      Skip_Whitespace (L);

      if L.Position > Length (L.Input) then
         L.Current_Token := (EOF, To_Unbounded_String (""), 0.0);
         return;
      end if;

      case Current_Char (L) is
         when '0' .. '9' =>
            L.Current_Token := Read_Number (L);
         when '+' =>
            L.Current_Token := (Plus, To_Unbounded_String ("+"), 0.0);
            Advance (L);
         when '-' =>
            L.Current_Token := (Minus, To_Unbounded_String ("-"), 0.0);
            Advance (L);
         when '*' =>
            L.Current_Token := (Multiply, To_Unbounded_String ("*"), 0.0);
            Advance (L);
         when '/' =>
            L.Current_Token := (Divide, To_Unbounded_String ("/"), 0.0);
            Advance (L);
         when '=' =>
            L.Current_Token := (Equals, To_Unbounded_String ("="), 0.0);
            Advance (L);
         when '(' =>
            L.Current_Token := (Left_Paren, To_Unbounded_String ("("), 0.0);
            Advance (L);
         when ')' =>
            L.Current_Token := (Right_Paren, To_Unbounded_String (")"), 0.0);
            Advance (L);
         when 'a' .. 'z' | 'A' .. 'Z' =>
            L.Current_Token := Read_Identifier (L);
         when others =>
            Put_Line ("Unexpected character: " & Current_Char (L));
            Advance (L);
            Next_Token (L);
      end case;
   end Next_Token;

   -- Function to parse a factor (number, variable, or parenthesized expression)
   function Parse_Factor (P : in out Parser) return Float is
      Result : Float;
   begin
      case P.L.Current_Token.Kind is
         when Number =>
            Result := P.L.Current_Token.Number_Value;
            Next_Token (P.L);
            return Result;

         when Identifier =>
            declare
               Var_Name : String := To_String (P.L.Current_Token.Value);
            begin
               Next_Token (P.L);
               if Variable_Maps.Contains (P.Variables, Var_Name) then
                  return Variable_Maps.Element (P.Variables, Var_Name);
               else
                  raise Undefined_Variable with "Variable '" & Var_Name & "' is not defined";
               end if;
            end;

         when Left_Paren =>
            Next_Token (P.L);
            Result := Parse_Expression (P);
            if P.L.Current_Token.Kind = Right_Paren then
               Next_Token (P.L);
               return Result;
            else
               raise Syntax_Error with "Expected )";
            end if;

         when others =>
            raise Syntax_Error with "Expected number, variable, or (";
      end case;
   end Parse_Factor;

   -- Function to parse a term (factor with optional * or /)
   function Parse_Term (P : in out Parser) return Float is
      Left : Float := Parse_Factor (P);
      Right : Float;
   begin
      while P.L.Current_Token.Kind = Multiply or P.L.Current_Token.Kind = Divide loop
         if P.L.Current_Token.Kind = Multiply then
            Next_Token (P.L);
            Right := Parse_Factor (P);
            Left := Left * Right;
         else -- Divide
            Next_Token (P.L);
            Right := Parse_Factor (P);
            if Right = 0.0 then
               raise Divide_By_Zero with "Division by zero";
            end if;
            Left := Left / Right;
         end if;
      end loop;
      return Left;
   end Parse_Term;

   -- Forward declaration for Parse_Expression
   function Parse_Expression (P : in out Parser) return Float;

   -- Function to parse an assignment
   function Parse_Assignment (P : in out Parser) return Float is
      Var_Name : String := To_String (P.L.Current_Token.Value);
      Value : Float;
   begin
      Next_Token (P.L);  -- Consume the identifier
      if P.L.Current_Token.Kind = Equals then
         Next_Token (P.L);  -- Consume the equals
         Value := Parse_Expression (P);
         Variable_Maps.Include (P.Variables, Var_Name, Value);
         return Value;
      else
         -- Not an assignment, so it's a variable reference in an expression
         if Variable_Maps.Contains (P.Variables, Var_Name) then
            return Variable_Maps.Element (P.Variables, Var_Name);
         else
            raise Undefined_Variable with "Variable '" & Var_Name & "' is not defined";
         end if;
      end if;
   end Parse_Assignment;

   -- Function to parse an expression (term with optional + or -)
   function Parse_Expression (P : in out Parser) return Float is
      Left : Float;
   begin
      -- Check if this is an assignment
      if P.L.Current_Token.Kind = Identifier then
         return Parse_Assignment (P);
      end if;

      Left := Parse_Term (P);

      while P.L.Current_Token.Kind = Plus or P.L.Current_Token.Kind = Minus loop
         if P.L.Current_Token.Kind = Plus then
            Next_Token (P.L);
            Left := Left + Parse_Term (P);
         else -- Minus
            Next_Token (P.L);
            Left := Left - Parse_Term (P);
         end if;
      end loop;

      return Left;
   end Parse_Expression;

   -- Function to evaluate an input string
   function Evaluate (P : in out Parser; Input : String) return Float is
   begin
      P.L.Input := To_Unbounded_String (Input);
      P.L.Position := 1;
      Next_Token (P.L);
      return Parse_Expression (P);
   end Evaluate;

   -- Main program
   Input : String (1 .. 100);
   Last : Natural;
   P : Parser;
   Result : Float;
begin
   Put_Line ("Ada Calculator Interpreter");
   Put_Line ("Enter expressions or 'quit'/'exit' to quit");

   loop
      Put ("> ");
      Get_Line (Input, Last);

      declare
         Line : String := Input (1 .. Last);
      begin
         exit when Line = "exit" or Line = "quit";

         if Line'Length > 0 then
            begin
               Result := Evaluate (P, Line);
               Put ("Result: ");
               Put (Result, Fore => 1, Aft => 6, Exp => 0);
               New_Line;
            exception
               when Syntax_Error =>
                  Put_Line ("Syntax Error");
               when Undefined_Variable =>
                  Put_Line ("Error: Undefined variable");
               when Divide_By_Zero =>
                  Put_Line ("Error: Division by zero");
               when others =>
                  Put_Line ("Error: Unknown error occurred");
            end;
         end if;
      end;
   end loop;
end Calculator; 