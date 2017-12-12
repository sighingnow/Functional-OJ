#!/usr/bin/env python
# -*- coding: utf-8 -*-

def zero(c):
    return c == '0'

def one(c):
    return c == '1'

def space(c):
    return c == ' '

def nspace(c):
    return c != ' '

def dropWhile(pred, s):
    k = 0
    for c in s:
        if pred(c):
            k += 1
        else:
            break
    return s[k:]

def dropEndWhile(pred, s):
    k = 0
    for i in range(len(s)-1, 0, -1):
        if pred(s[i]):
            k += 1
        else:
            break
    return s[0:len(s)-k]

def takeDropWhile(pred, s):
    if s == None or len(s) == 0:
        return '', ''
    l, k = '', 0
    for c in s:
        if pred(c):
            l += c
            k += 1
        else:
            break
    return l, s[k:]


# "Dot" – is 1 time unit long.
# "Dash" – is 3 time units long.
# Pause between dots and dashes in a character – is 1 time unit long.
# Pause between characters inside a word – is 3 time units long.
# Pause between words – is 7 time units long.

def decodeBitsImpl(bits):
    if bits == None or len(bits) == 0:
        return ""
    else:
        if bits[0] == '0':
            l, r = takeDropWhile(zero, bits)
            ll = len(l)
            if ll == 1:
                return decodeBitsImpl(r)
            elif ll == 3:
                return ' ' + decodeBitsImpl(r)
            elif ll == 7:
                return '   ' + decodeBitsImpl(r)
        else:
            l, r = takeDropWhile(one, bits)
            ll = len(l)
            if ll == 1:
                return '.' + decodeBitsImpl(r)
            elif ll == 3:
                return '-' + decodeBitsImpl(r)

def detectFreq(bits):
    if bits == '':
        return 65535
    else:
        if bits[0] == '0':
            l, r = takeDropWhile(zero, bits)
            return min(len(l), detectFreq(r))
        else:
            l, r = takeDropWhile(one, bits)
            return min(len(l), detectFreq(r))

def decodeBits(bits):
    # ToDo: Accept 0's and 1's, return dots, dashes and spaces
    bits_ = ''
    bits = dropEndWhile(zero, dropWhile(zero, bits))
    freq = detectFreq(bits)
    if freq == 65535:
        freq = 1
    for i in range(0, len(bits), freq):
        bits_ += bits[i]
    return decodeBitsImpl(bits_)

def decodeMorse(morseCode):
    if morseCode == None or len(morseCode) == 0:
        return ''
    else:
        if space(morseCode[0]):
            l, r = takeDropWhile(space, morseCode)
            if len(l) == 3:
                return ' ' + decodeMorse(r)
            else:
                return decodeMorse(r)
        else:
            l, r = takeDropWhile(nspace, morseCode)
            return MORSE_CODE[l] + decodeMorse(r)



s = '1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011'

if __name__ == '__main__':
    x = decodeBits('000000011100000')
    print(x)
    print(decodeMorse(s))

