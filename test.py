s = "negr123"


# ecryption
h = 0
for c in s:
    d = ord(c)
    h = 256 * h + d

print(h)
s = ""
# decyption
while h > 0:
    s = chr(h % 256) + s
    h //= 256
    
print(s)

