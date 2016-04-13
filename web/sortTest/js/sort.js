//JS

var arrEq = function(arrl, arrr){
        if(arrl.length !== arrr.length)
                return false;

        for(var i = 0; i < arrl.length; i++){
                if(arrl[i] !== arrr[i])
                        return false;
        }

        return true;
};

var bigO = function(best, average, worst){
        this.best = best;
        this.average = average;
        this.worst = worst;
};

var sort = function(name, about, thisBigO, firstPart, initArgs){
        this.name = name;
        this.about = about;
        this.bigO = thisBigO;
        this.part = firstPart;
        this.args = initArgs;

        this.checkExe = function(arrSt, arrEn){
                nObj = part.exe(arrSt, arrEn, args);
                if(nObj.valid){
                        this.part = nObj.next;
                        this.args = nObj.nArgs;
                }
                return nObj.valid;
        };

        this.done = function(arr){
                for(var i = 1; i < arr.length; i++){
                        if(arr[i - 1] > arr[i]) return false;
                }
                return true;
        };
};

var part = function(name, about, thisBigO, fExe, curr2next){
        this.name = name;
        this.about = about;
        this.bigO = thisBigO;

        this.exe = function(arrSt, arrEn, args){
                var en = fExe(arrSt.slice(), args);
                if(arrEq(en, arrEn)){
                        tn = curr2next(args);
                        tn.valid = true;
                        return tn;
                }else{
                        return {valid: false};
                }
        };
};
