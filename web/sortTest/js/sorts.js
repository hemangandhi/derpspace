//JS - assumes sort.js

var moveThroughArrAnd = function(spars, ppars, exe){
        var tef = function(arr, args){return exe(arr, args.idx);};
        var tc2n = function(args){
                args.idx++;
                return {next: part(ppars.name, 
                                ppars.about, ppars.bigO, tef, tc2n),
                        nArgs: args};
        }

        var p = new part(ppars.name, 
                        ppars.about, ppars.bigO, tef, tc2n);

        return new sort(spars.name, spars.about, 
                        spars,bigO, p, {idx: 0});
};
