//JS

var genRand = function(h, w){
	var rv = [];
	for(var i = 0; i < h; i++){
		rv.push([]);
		for(var j = 0; j < w; j++){
			rv[i].push(Math.floor(Math.random() * 2) == 1);
		}
	}
	return rv;
};

var initialize = function(h, w, init){
        var rv = [];
        for(var i = 0; i < h; i++){
                rv.push([]);
                for(var j = 0; j < w; j++){
                        if(i < init.length && j < init[i].length)
                                rv[i].push(init[i][j]);
                        else
                                rv[i].push(false);
                }
        }
        return rv;
};


$(document).ready(function(){
        var rules;
        var matrix;
        var steps;
        var paused;
        var interID;

	$('#game').hide();

        var renderNext = function(isStep){
                if(paused && !isStep)return;
                steps['stps']++;
                matrix = nextState(matrix, rules);
                render($('#canvas'), matrix);
                $('#currSt').text("Kills: " + steps['kill'] + " Spawns: " + steps['gen'] + " Generations: " + steps['stps']);
                if(matrix.every(function(v){
                        return v.every(function(u){
                                return !u;
                        });
                })){
                        clearInterval(interID);
                        $('#game').hide();
                        $('#form').show();
                        $('#prev').text('Previous game results: ' + steps['gen'] + ' generated cells and ' + steps['kill'] + ' cells killed over ' + steps['stps'] + ' generations!');
                        $('#prev').show();
                }
        };

        $('#start-btn').click(function(){
                $('#form').hide();
                var w = parseInt($("#winp").val());
                var h = parseInt($("#hinp").val());
                var sps = [];
                $('#spwn > input').each(function(){
                        if($(this).is(':checked'))
                                sps.push(parseInt($(this).attr('value')));
                });
                var lvs = [];
                $('#live > input').each(function(){
                        if($(this).is(':checked'))
                                lvs.push(parseInt($(this).attr('value')));
                });
                rules = {'spawn': sps, 'live': lvs};
                steps = {'gen':0, 'kill':0, 'stps': 0};
                paused = false;

		if($('#init').val() !== "Rand")
			matrix = initialize(h, w, allTheInits[$('#init').val()]);
		else
			matrix = genRand(h, w);

                interID = setInterval(function(){renderNext(false);}, 100);
                $('#game').show();
                render($('#canvas'), matrix);
        });

        $('#canvas').on('click', '.cell-alive', function(){
                if(!paused)return;
                $(this).attr('class', 'cell-dead');
                var gd = $(this).attr('id').split('l').slice(1).map(Number);
                matrix[gd[1]][gd[0]] = false;
                steps['kill']++;
                //renderNext();
        });
        
        $('#canvas').on('click', '.cell-dead', function(){
                if(!paused)return;
                $(this).attr('class','cell-alive');
                var gd = $(this).attr('id').split('l').slice(1).map(Number);
                matrix[gd[1]][gd[0]] = true;
                steps['gen']++;
                //renderNext();
        });

        $('#pause').click(function(){
                paused = !paused;
                if(paused)
                        $(this).attr('value', 'Resume');
                else
                        $(this).attr('value', 'Pause');
        });

	$('#step').click(function(){
		if(!paused)return;
		renderNext(true);
	});
});
