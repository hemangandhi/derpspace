
var playerStates = {
        AWAITING_TURNS: "Waiting for other players to play",
        PLAYING: "Currently considering the next move",
        OUT_OF_TIME: "Ran out of time",
        DELAYING_NEXT: "Delaying the teammate's next move."
};

var player = function(idx, normalTime, byoCount, byoLen, delay, nextTeammate, opponent, uiUpdater){
        var isOpponentDone = false;
        var isTeammateDone = false;
        var currInt = null;
        var currByo = byoLen;
        var tryToDoTurn = function(){
                if(!isOpponentDone || !isTeammateDone) return;

                var playerFn = function(){
                        if(normalTime > 0){
                                uiUpdater(playerStates.PLAYING, idx, 1, normalTime);
                                normalTime--;
                        }else{
                                uiUpdater(playerStates.PLAYING, idx, byoCount, currByo);
                                if(currByo > 0)
                                        currByo--;
                                else{
                                        byoYomiCount--;
                                        currByo = byoLen;
                                }
                                if(byoYomiCount == 0)
                                        uiUpdater(playerStates.OUT_OF_TIME, idx, 0, 0);
                        }
                };
                currInt = setInterval(playerFn, 1000);
        };
        this.teammateDone = function(){
                isTeammateDone = true;
                tryToDoTurn();
        };
        this.opponentDone = function(){
                isOpponentDone = true;
                tryToDoTurn();
        };
        this.endTurn = function(){
                if(currInt !== null){
                        clearInterval(currInt);
                        currInt = null;
                        currByo = byoLen;
                        uiUpdater(playerStates.DELAYING_NEXT, idx, (normalTime > 0)? 1: byoCount, (normalTime > 0)? normalTime: byoLen);
                        setTimeout(this.nextTeammate.teammateDone, delay);
                        this.opponent.opponentDone();
                }
        };
};

$(document).ready(function(){
        $('#add-player').click(function(){
                var $tag = $('<div class="player-input">' + $('#player1').html() + "</div>");
                $('#players').append($tag);
        });

        $('#start-game').click(function(){
                $('#the-form').hide();
                var players = [];
                $('.player-input').each(function(pi){
                        var newPlayer = new player(players.length, pi.children('input[name=normalTime]').first().value(),
                                                   pi.children('input[name=byoCount]').first().value(),
                                                   pi.children('input[name=byoLen]').first().value(),
                                                   pi.children('input[name=delay]').first().value(),
                                                   null, null, );
                        players.push(newPlayer);
                });
        });
});
