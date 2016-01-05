# this python script converts a hs file to a lhs file

input_file = open("picoParser2011.hs", "r")
output_file = open("picoParser2011.lhs", "w")

consecutive_comment = False

for line in input_file:
    
    if len(line) >= 2:
        if line.startswith("--"):
            line = line[2:len(line)].strip() + '\n'
            consecutive_comment = True
        elif line.strip().startswith("--"):
            line = line.strip()[2:len(line)].strip() + '\n'  
            consecutive_comment = True
        else:
            if consecutive_comment:
                output_file.write('\n')
                consecutive_comment = False
                
            line = '>' + line
            
    output_file.write(line)