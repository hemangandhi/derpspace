var X_OFFSET = 75;
var Y_OFFSET = 86;

var makeDiv = function(x, y, imgSrc, attrs){
	var $nd = $("<div>");
	$nd.css({position: "absolute",
		 left: X_OFFSET*x + $(window).width()/2 + "px",
		 top: Y_OFFSET*y + $(window).height()/2 + Y_OFFSET*x/2 + "px",
		 width: "100px",
		 height: "100px",
		 margin: "0px"});
	if(attrs) $nd.attr(attrs);
	$("<img>", {src: imgSrc, width: "100px", height: "100px"}).appendTo($nd);
	return $nd;
};

var isUnoccupied = function(mat, x, y){
	return !mat.some(function(e, i, a){
		return e[0] == x && e[1] == y;
	});
};

var renderMat = function(mat, tag){
	tag.html("");

	mat.forEach(function(e){
		tag.append(makeDiv(e[0],e[1], ((e[2])?"img/red_hex.png":"img/black_hex.svg"), false));
	});
};

var renderMoves = function(mat, tag){
	var done = [];
	
	mat.forEach(function(e){
		var dels = [[1,0],[-1,0],[0,1],[0,-1],[1,-1],[-1,1]];
		dels.forEach(function(f){
			var c = [e[0] + f[0], e[1] + f[1]];
			if(isUnoccupied(mat,c[0],c[1]) && isUnoccupied(done, c[0],c[1])){
				tag.append(makeDiv(c[0],c[1], "img/hex_pos.png", {class: "pos"}));
				done.push(c);
			}
		});
	});
	return done;
};

var renderAll = function(mat,tag){
	renderMat(mat, tag);
	return renderMoves(mat, tag);
};

var renderStart = function(tag){
	var mat = [[0,0, true],[1,0, false]];
	renderMat(mat, tag);
	var p = [[1,1],[2,0],[2,-1]];
	p.forEach(function(e){
		tag.append(makeDiv(e[0],e[1], "img/hex_pos.png", {class: "pos"}));
	});

	return {mat: mat, moves: p};
};

var tagToMat = function(tag){
	var pos = tag.position();
	pos.left -= $(window).width()/2;
	pos.top -= $(window).height()/2;
	var x = pos.left/X_OFFSET;
	var y = pos.top - x*Y_OFFSET/2;
	y /= Y_OFFSET;

	return [x, y];
};

var getColor = function(mat, x, y){
	var i;
	for(i = 0; i < mat.length; i++)
		if(mat[i][0] == x && mat[i][1] == y)
			return (mat[i][2])?1:-1;
	return 0;
};

var checkWin = function(mat, x, y){
	var i;
	for(i = 0; i < mat.length; i++){
		if(mat[i][0] == x && mat[i][1] == y)
			break; 
	}

	if(i == mat.length)
		return false;

	var cInt = (mat[i][2])?1:-1;
	var dels = [[1,0],[1,-1],[0,-1],[-1,0],[-1,1],[0,1]];

	var lineLens = [];
	dels.forEach(function(e){
		var j;
		for(j = 0; getColor(mat, e[0]*(j + 1) + x, e[1]*(j + 1) + y) === cInt; j++);
		lineLens.push(j);
	});

	console.log('ll: ' + lineLens);
	//Lines - all cases.
	if(lineLens[0] + lineLens[3] >= 5 || lineLens[1] + lineLens[4] >= 5 || lineLens[2] + lineLens[5] >= 5)
		return true;

	//Vertex of triangle - all cases.
	for(var k = 0, j = 1; k < 6; k++, j = (k + 1) % 6){
		if(lineLens[k] >= 2 && lineLens[j] >= 2 && getColor(mat, x + dels[k][0] + dels[j][0], y + dels[k][1] + dels[j][1]) === cInt)
			return true;
	}

	//Ring - all cases.
	for(var k = 0; k < 6; k++){
		if(lineLens[k] >= 1){
			var xp = x + dels[k][0], yp = y + dels[k][1], j;
			for(j = 1; j < 6; j++){
				if(getColor(mat, xp, yp) === cInt){
					xp = xp + dels[(k + j) % 6][0];
					yp = yp + dels[(k + j) % 6][1];
				}else{
					break;
				}
			}

			if(j >= 6)
				return true;
		}
	}

	//Triangle edge - all cases.
	for(var k = 0; k < 6; k++){
		var j;
		for(j = 0; j < 4; j++){
			if(lineLens[(j + k) % 6] < 1)
				break;
		}

		if(j >= 4 && getColor(mat, x + dels[(k + 1) % 6][0] + dels[(k + 2) % 6][0], y + dels[(k + 1) % 6][1] + dels[(k + 2) % 6][1]) === cInt)
			return true;
	}

	return false;
};
