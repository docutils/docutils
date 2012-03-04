# a simple script to fix bad fomatting forced by the formatting contraintes of
# the original
#
read_obj = open('method_of_least_squares.rst', 'r')
write_obj = open('method_of_least_squares_clean.rst', 'w')
line_to_read = 1
while line_to_read:
    line_to_read = read_obj.readline()
    line = line_to_read
    line = line.replace(' Miller [*]_', ' Miller') 
    line = line.replace('see equation (', 'see (')# makes the document consistent
    if '.. [*] E-mail:' in line:
        continue
    write_obj.write(line)
    if 'Providence,' in line:
        write_obj.write('          sjmiller@math.brown.edu\n') 
        # write_obj.write(':contact: sjmiller@math.brown.edu\n') 

read_obj.close()
write_obj.close()
