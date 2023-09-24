# Python script to read `main.sym` and construct a map from symbol name to value

import sys
import re

def read_sym_file(filename):
    symbol_map = {}
    try:
        with open(filename, 'r') as f:
            for line in f.readlines():
                if line.startswith('---'):
                    continue
                parts = line.split()
                if len(parts) >= 2:
                    symbol = parts[0].strip()
                    value = parts[1].strip()
                    symbol_map[symbol] = int(value, 16)

        return symbol_map
    
    except FileNotFoundError:
        return f"File {filename} not found."

    except Exception as e:
        return f"An error occurred: {e}"

syms = read_sym_file("main.sym")

assert((syms["BLOCKS_R"]) % 0x2000 + 0x400 == syms["BLOCKS_W"])
assert(syms["BLOCKS_W"] % 0x100 == 0) # sometimes accessed by writing to low byte
assert((syms["LINES_R"]) % 0x2000 + 0x400 == syms["LINES_W"])
assert((syms["LINES_CORE_R"] % 0x2000) + 0x400 == syms["LINES_CORE_W"])

SUCCESS = True

def replace_syms(input_str):
    return re.sub(r'\$\{(\w+)\}', r'syms["\1"]', input_str)

# Python script to read `main.asm`, find lines starting with `; [py]` and eval the remainder of the line
def execute_py_in_asm(filename):
    global syms
    try:
        with open(filename, 'r') as f:
            for line in f.readlines():
                line = line.strip();
                if line.startswith('; [py]'):
                    code_to_eval = replace_syms(line[6:].strip())
                    if eval(code_to_eval) is False:
                        print("Failed: ", code_to_eval)
    except FileNotFoundError:
        return f"File {filename} not found."
    except Exception as e:
        return f"An error occurred: {e}"
    
execute_py_in_asm("main.asm")
execute_py_in_asm("wram.asm")
execute_py_in_asm("xram.asm")

#for i in range(syms['ROWS'] * syms['ROWSUB']):
#   print(i, hex(syms['LINES_CORE_W'] + syms['ROWINSTRC']*i));

if not SUCCESS:
    sys.exit(1)