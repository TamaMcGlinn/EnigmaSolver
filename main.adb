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
  subtype digitResult is Integer range singleDigit'First * 2 .. singleDigit'Last * 2 + 1;

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
    return Character'Pos(char) - Character'Pos('0');-- - '0''Position;
  end digitFromChar;

  function checkAdd(first : String; second : String; answer : String) return Boolean is
    n : constant Integer := first'Length;
    subtype overflow is Integer range 0..1;
    o : overflow := 0;
  begin
    if n /= second'Length or n /= answer'Length then
      raise Constraint_Error with "input lines should have the same length";
    end if;
    for i in reverse 1..n loop
      declare
        added : digitResult := digitFromChar(first(i)) + digitFromChar(second(i)) + o;
      begin
        if digitFromChar(answer(i)) /= added mod 10 then
          return false;
        end if;
        o := (if added >= 10 then 1 else 0);
      end;
    end loop;
    return o = 0;
  end checkAdd;

  function checkDecoded(line : String) return Boolean is
  begin
    for c of line loop
      if not (c = ' ' or (c >= '0' and c <= '9')) then
        return false;
      end if;
    end loop;
    return true;
  end checkDecoded;

  function getFirstUnfixedSymbol(first : String; second : String; answer : String) return Character is
    lines : String := first & second & answer;
  begin
    for c of lines loop
      if not (c = ' ' or (c >= '0' and c <= '9')) then
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

  procedure solve(first : String; second : String; answer : String; availableNumbers : availability) is
  begin
    declare
      l1 : String := first;
      l2 : String := second;
      l3 : String := answer;
      a  : availability := availableNumbers;
    begin
      if checkDecoded(l1) and then checkDecoded(l2) and then checkDecoded(l3) then
        if checkAdd(l1, l2, l3) then
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
              solve(out1, out2, out3, availableNumbers);
            end;
          end if;
        end loop;
      end;
    end;
  end solve;

  initialNumbers : availability := getAvailableNumbers(line1, line2, line4);
begin
  solve(line1, line2, line4, initialNumbers);
end Main;

