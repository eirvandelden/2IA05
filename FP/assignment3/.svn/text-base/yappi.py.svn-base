# Yet another Parser for PICO

# filter the non-terminal name for clashes and strange characters
def sanitize(name):
    if name == 'char':
        name = 'parse_char'
    elif name == 'type':
        name = 'parse_type'
    elif name == 'digit':
        name = 'parse_digit'
    elif name == '||':
        name = 'parse_or'

    name = name.replace('-','')

    return name

def is_quoted(s):
    return (s[0] == '"')

bnf_file = open("pico.bnf")
type_file = open("pico.type")
preamble_file = open("pico.pre")
output_file = open("test.hs", "w")

# prefix for the result variable name
result_prefix = 'result_'

# these characters are used for indentation
indentation_string = "    "

# output string
out = ""

# these are dictionaries of lists, since each terminal can occur multiple times
# multiple definitions are handled in order

# haskell types of the non terminals
types = dict()

# return types of the non terminals
rtypes = dict()

# returns the type of some token and removes it from the dictionary
def get_type(tok):

    tok = tok.lower()

    if (tok in types):
                
        # get the definition of the type from the dictionary
        type_list = types[tok]
        
        # now we can write the type into the haskell file
        result = type_list.pop(0)
        
        # and put the list back into the dict without its first element
        types[tok] = type_list
        
    else:
        result = '<NO DECLARATION>'
                
    return result
                
def get_rtype(tok):

    tok = tok.lower()

    if (tok in rtypes):
                
        # get the definition of the type from the dictionary
        type_list = rtypes[tok]
        
        # now we can write the type into the haskell file
        result = type_list.pop(0)
        
        # and put the list back into the dict without its first element
        rtypes[tok] = type_list
        
    else:
        result = '<NO RESULT>'
                
    return result

def add_to_dict(dictionary):
# following token is the type name
    type_name = tokens[1]
    
    # the token afterwards needs to be '='
    if (tokens[2] != '='):
        raise RuntimeError("'=' expected in line: " + line)
    
    definition = ""
    
    # the rest of the tokens is the definition
    for i in range(3, len(tokens)):
        definition += tokens[i] + ' '
    
    # append these to the dictionary (note that each key is associated with a list)
    if type_name not in dictionary:
        dictionary[type_name] = [definition.strip()]
    else:
        type_list = dictionary[type_name]
        type_list.append(definition)
        dictionary[type_name] = type_list
        
    return dictionary        

# add the preamble
output_file.write(preamble_file.read())

# parse the type file and store the types and return types in a list
for line in type_file:

    # tokenize the string
    tokens = line.split()

    # the first 2 tokens are always checked, the token[0][0] allows for comments to start with a #
    if (len(tokens) > 1) and (tokens[0][0] != '#'):
    
        if (tokens[0] == 'type'):
            
            types = add_to_dict(types)
            
        elif (tokens[0] == 'rtype'):
    
            rtypes = add_to_dict(rtypes)
        
        else:
            raise RuntimeError("unexpected token in type file '" + a_token + "' in line: " + line)

# parse the bcnf file and generate the code that goes along with it
for line in bnf_file:

    # variable names
    variable_names = []

    # tokenize the string
    tokens = line.split()
    
    # array index
    i = 0
    
    no_return_vars = False
    
    # TODO: remove for switch anti pattern!
    # token is a reserved word
    for a_token in tokens:
        
        # first token is the non terminal name
        if (i == 0):
            original_non_terminal_name = a_token.lower()
            
            non_terminal_name = sanitize(original_non_terminal_name)
            
            out += non_terminal_name + ' :: Parser '
            
            out += get_type(non_terminal_name)
            
            out += '\n'
            
            out += non_terminal_name + ' = do\n'
            indent = ' ' * (len(non_terminal_name) + 3)
            
            # TODO: Remove print name of non terminal
            #print sanitize(a_token.lower())
            
        # second token needs to be "::="
        elif (i == 1):
            if (a_token != '::='):
                raise RuntimeError("unexpected token in bcnf file '" + a_token + "' in line: " + line)
        
        # we are on the right hand side of an expression
        else:
        
            # TODO: handle this more elegantly
            # HACK: not for PICO id
            if ('|' in tokens) and (non_terminal_name != 'picoid'):
                if (i == 2):
                    out += indent + 'result <- (\n'
                    indent = indent + indentation_string
                
            # if the last line is a line break the next line should be indented
            if (out[len(out) - 1] == '\n'):
                out += indent
            
            # if it is a terminal
            if (is_quoted(a_token)):
                out += 'symbol ' + a_token + '\n'
            # if its the empty string use that parser instead
            elif (a_token == 'e'):
                out += 'empty\n' 
            # if its an or, just transform it to the '+++' operator
            elif (a_token == '|'):
                out += '+++\n'
            # hack: just assume that if we see a '(' that it is the start of a repetition for simplicity
            elif (a_token == '('):
                # TODO: make a more sensible name
                name = 'list'
                no_return_vars = True
                out += result_prefix + name + ' <- many (\n'
                indent = indent + indentation_string
            # hack: and ofcourse the )* should just become closing brackets
            elif (a_token == ')*' or a_token == ')'):
                out += ')\n'
                indent = indent[:-(len(indentation_string))]
            # it is a non_terminal
            else:
                name = sanitize(a_token.lower())
                
                # Increment the name if it already exists
                if (name in variable_names):
                    i = 2
                    rename = name
                    while (rename in variable_names):
                        rename = name + str(i)
                        i += 1
                    name = rename
                
                # TODO: remove hack
                # if the expression starts with the same as the non terminal name, do not add 'result'
                if (name[:len(original_non_terminal_name)] == original_non_terminal_name) or ('|' in tokens) or no_return_vars:
                    out += name + '\n'
                else:
                    out += result_prefix + name + ' <- ' + name + '\n'
                
                    
                variable_names.append(name)

        i += 1
        
    if ('|' in tokens):
        out += indent + ')\n'
        indent = indent[:-(len(indentation_string))]
    
    if (len(tokens) > 0):
        out += indent + 'return '
        
        
        out += get_rtype(non_terminal_name)
        
        #for name in variable_names:
        #    out += result_prefix + name
        #    
        #    # TODO: fix, check the index not the content
        #    if (variable_names[len(variable_names) - 1] != name):
        #        out += ', '
        out += '\n\n'

output_file.write(out)