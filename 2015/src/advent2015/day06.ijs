NB. Parsing input

startswith =: [: (0&{) E. NB. does 'x' exist in 'y' with index 0?

ison     =: ('turn';'on') &startswith
isoff    =: ('turn';'off')&startswith
istoggle =: ('toggle')    &startswith


NB. Handling sub-arrays

NB. create a range from x to y by computing 1+y-x, create a list of
NB. integers with lengh 1+y-x, add x
r=:([ + [: i. 1: + -~)

NB. coordinates for sub-array fra x1,y1 til x2,y2
sub=: [: < (([: r/ 0&{) ; ([:r/1&{))

NB. c =: x1 y1 ,. x2 y2

NB. turn on
on=: 4: '1 (sub x) } y'

NB. turn off
off=: 4: '0 (sub x) } y'

NB. toggle
toggle=: 4: '(-. (sub x){y) (sub x) } y'

NB. read input
require 'files'
input=: freads 'input06.txt'


lights=: 1000 1000 $ 0   NB. lights er 1000x1000-tabell med lamper, initialisert til 0

lights=: 10 10 $ 0



c=. (1 2,.3 4)

1 (sub c) } lights

+/ +/ lights

lights

1 {"1 lights
(<0 2;1 3) { lights

(0 2;1 3)

' ' cutopen ',' cutopen 'dette er,en test'

NB. 100 doors
>: i. 10

~:/ (20 $ - {. 1:)"0 >: i.20

ascii =: a.&i.

ascii 'a'

;/ 1 2 3

NB. Return a range of integers from x to y, inclusive
range =: 4 : 'x + i. ((y-x)+1)'

NB. Toggle a sub-rectangle (x) of table (y): select x From y, compute 1-, amend in to y
toggle =: 4 : '(1-x{y) x } y'

NB. Count number of 1 by summing all values
count =: 3 : '+/+/(y)'

14 range 19

l2 =: 1 (< (2 range 55) ; (22 range 400)) } lights

space 'l2= (<(14 range 334);(49 range 73)) toggle l2'

l3 (<(1 range 3);(4 range 7)) } l2

NB. turn on 0,0 through 999,999
NB. toggle 0,0 through 999,0
NB. turn off 499,499 through 500,500

space=: 7!:2

space 1+2


ison ;: 'turn on 0,0 through 999,999'
;: 'toggle 0,0 through 999,0'
# ;: 'turn off 499,499 through 500,500'

(3 3,:4 5) (100&-);.0 i. 10 10

(< 1) E. 1; 2; 3


