//JS
var SQ_SIZE = 25;
var TOP_SET = 50;

var makeDiv = function(x, y, alive){
        var $nd = $("<div>");
        var $ndCls = (alive)?'cell-alive':'cell-dead';
        $nd.attr({
                'class': $ndCls,
                'id': 'l' + x + 'l' + y  
        });
        $nd.css({
                'position': 'absolute',
                'width': SQ_SIZE + 'px',
                'height': SQ_SIZE + 'px',
                'top': (TOP_SET + y*SQ_SIZE) + 'px',
                'left': x*SQ_SIZE + 'px'
        });
        return $nd;
};

var makeMat = function(initMat){
        var rv = [];
        for(var i = 0; i < initMat.length; i++){
                rv.push([]);
                for(var j = 0; j < initMat[i].length; j++){
                        rv[i].push(makeDiv(j,i, initMat[i][j]));
                }
        }
        return rv;
};

var render = function(target, initMat){
        target.html("");
        makeMat(initMat).forEach(function(row){
                row.forEach(function(tag){
                        target.append(tag);
                });
        });
};

var nbhrCount = function(mat, x, y){
        var dels = [-1, 0, -1];
        var ct = 0;
        for(var i = -1; i < 2; i++){
                for(var j = -1; j < 2; j++){
                        if(i == 0 && j == 0) continue;
                        if(mat[(y + i + mat.length) % mat.length][(x + j + mat[y].length) % mat[y].length]) ct++;
                };
        }
        return ct;
};

var nextState = function(currMat, rules){
        return currMat.map(function(row, ri){
                return row.map(function(val, ci){
                        var n = nbhrCount(currMat, ci, ri);
                        if(val)
                                return rules['live'].indexOf(n) >= 0;
                        else
                                return rules['spawn'].indexOf(n) >= 0;
                });
        });
};
