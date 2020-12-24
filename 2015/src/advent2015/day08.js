// since the escaped strings from day-08 use the same escape-syntax as
// javascript, we can parse the easily using javascript eval()...

// this is not recommended unless you know what the input is!!

var rawcnt = 0;
var esccnt = 0;

var fs = require('fs'),
    readline = require('readline');

var rd = readline.createInterface({
    input: fs.createReadStream('input08.txt'),
    output: process.stdout,
    terminal: false
});


//rd.on('line', function(line) {
//    rawcnt += line.length;
//    esccnt += eval(line).length;
//});

rd.on('line', function(line) {
    esccnt += line.length;
    rawcnt += (line.replace(/\\/g, '\\\\').replace(/"/g,'\\"').length+2);
});

rd.on('close', function() {
    console.log('raw charcount: '+rawcnt);
    console.log('escaped count: '+esccnt);
    console.log('diff         : '+(rawcnt-esccnt));
});
