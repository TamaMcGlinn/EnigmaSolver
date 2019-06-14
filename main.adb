with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
  line1 : constant String := Get_Line;
  line2 : constant String := Get_Line;
  line3 : constant String := Get_Line;
  line4 : constant String := Get_Line;

  function replace(input : String; c : Character; r : Character) return String is
    result : String := input;
  begin
    for i in 1..input'Length loop
      if input(i) = c then
        result(i) := r;
      end if;
    end loop;
    return result;
  end replace;

  procedure replace(l1 : String; l2 : String; l3 : String; c : Character; r : Character;
                    out1 : out String; out2 : out String; out3 : out String) is
  begin
    out1 := replace(l1, c, r);
    out2 := replace(l2, c, r);
    out3 := replace(l3, c, r);
  end replace;

  subtype singleDigit is Integer range 0..9;
  subtype additionResult is Integer range 0 .. 9 * 2 + 1;
  subtype subtractionResult is Integer range 0 - 9 - 1 .. 9;

  function toChar(s : singleDigit) return Character is
  begin
    return Character'Val(Character'Pos('0') + s);
  end toChar;

  function digitFromChar(char : Character) return singleDigit is
  begin
    if char = ' ' then
      return 0;
    end if;
    if char < '0' or char > '9' then
      raise Constraint_Error with "decoded characters should be either ' ' or in range [0..9]";
    end if;
    return Character'Pos(char) - Character'Pos('0');
  end digitFromChar;

  function checkAdd(first : String; second : String; answer : String) return Boolean is
    n1 : Integer := Integer'Value(first);
    n2 : Integer := Integer'Value(second);
    n3 : Integer := Integer'Value(answer);
  begin
    return n3 = n1 + n2;
  end checkAdd;

  function checkSubtr(first : String; second : String; answer : String) return Boolean is
    n1 : Integer := Integer'Value(first);
    n2 : Integer := Integer'Value(second);
    n3 : Integer := Integer'Value(answer);
  begin
    return n3 = n1 - n2;
  end checkSubtr;

  function isDecoded(c : Character) return Boolean is
  begin
    return c = '-' or c = ' ' or (c >= '0' and c <= '9');
  end isDecoded;

  function checkDecoded(line : String) return Boolean is
  begin
    for c of line loop
      if not isDecoded(c) then
        return false;
      end if;
    end loop;
    return true;
  end checkDecoded;

  function getFirstUnfixedSymbol(first : String; second : String; answer : String) return Character is
    lines : String := first & second & answer;
  begin
    for c of lines loop
      if not isDecoded(c) then
        return c;
      end if;
    end loop;
    raise Program_Error with "Expected some unfixed symbol";
  end getFirstUnfixedSymbol;

  type availability is array (singleDigit'Range) of Boolean;

  procedure print(first : String; second : String; answer : String) is
  begin
    Put_Line("================");
    Put_Line(first);
    Put_Line(second);
    Put_Line(line3);
    Put_Line(answer);
    Put_Line("================");
  end print;

  function getAvailableNumbers(first : String; second : String; answer : String) return availability is
    lines   : String := first & second & answer;
    result  : availability := (others => true);
  begin
    for c of lines loop
      if c >= '0' and c <= '9' then
        result(digitFromChar(c)) := false;
      end if;
    end loop;
    return result;
  end getAvailableNumbers;

  type checking_function is access function(s1 : String; s2 : String; s3 : String) return Boolean;
  type binary_int_operator is access function(lhs : Integer; rhs : Integer) return Integer;

  function checkOperator(first : String; second : String; answer : String; op : binary_int_operator) return Boolean is
  begin
    return Integer'Value(answer) = op(Integer'Value(first), Integer'Value(second));
  end checkOperator;

  procedure solve(first : String; second : String; answer : String; availableNumbers : availability; op : binary_int_operator) is
  begin
    declare
      l1 : String := first;
      l2 : String := second;
      l3 : String := answer;
      a  : availability := availableNumbers;
    begin
      if checkDecoded(l1) and then checkDecoded(l2) and then checkDecoded(l3) then
        if checkOperator(l1, l2, l3, op) then
          print(l1, l2, l3);
        end if;
        return;
      end if;
      declare
        nextFix : Character := getFirstUnfixedSymbol(l1, l2, l3);
        numbers : availability := getAvailableNumbers(l1, l2, l3);
      begin
        for i in numbers'First..numbers'Last loop
          if numbers(i) then
            declare
              out1 : String := l1;
              out2 : String := l2;
              out3 : String := l3;
            begin
              replace(l1, l2, l3, nextFix, toChar(i), out1, out2, out3);
              solve(out1, out2, out3, availableNumbers, op);
            end;
          end if;
        end loop;
      end;
    end;
  end solve;

  --  you can reference operator subprograms
  --  (using the Access attribute) as long as they're not intrinsic.
  function "+" (Lhs, Rhs : Integer) return Integer is
    (Standard."+" (Lhs, Rhs));

  function "-" (Lhs, Rhs : Integer) return Integer is
    (Standard."-" (Lhs, Rhs));

  operator : Character := line3(line3'Last);

  function getCheckingFunction return binary_int_operator is
  begin
    case operator is
      when '+' => return "+"'Access;
      when '-' => return "-"'Access;
      when others => raise Program_Error with "Unsupported operator " & operator & " requested.";
    end case;
  end getCheckingFunction;

  initialNumbers : availability := getAvailableNumbers(line1, line2, line4);
  op : binary_int_operator := getCheckingFunction;
begin
  Put_Line("Printing all possible solutions:");
  solve(line1, line2, line4, initialNumbers, op);
end Main;

