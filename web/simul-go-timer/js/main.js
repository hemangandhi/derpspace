//JS

var player = function(idx, normalTime, byoYomiCount, byoYomiDurr, playerDelay, startNextPlayer){
        var currByo = byoYomiDurr;
        var currInt = null;
        this.next = nextPlayer;
        this.playTime = function(uiUpdater, doTimeout){
                var playerFn = function(){
                        if(normalTime > 0){
                                uiUpdater(idx, 1, normalTime);
                                normalTime--;
                        }else{
                                uiUpdater(idx, byoYomiCount, currByo);
                                if(currByo > 0)
                                        currByo--;
                                else{
                                        byoYomiCount--;
                                        currByo = byoYomiDurr;
                                }
                                if(byoYomiCount == 0)
                                        doTimeout(idx);
                        }
                };
                currInt = setInterval(playerFn, 1000);
        };
        this.resetByo = function(){ currByo = byoYomiDurr;};
        this.endTurn = function(){
                if(currInt !== null){
                        clearInterval(currInt);
                        this.resetByo();
                        setTimeout(this.next.playTime, playerDelay);
                }
        };
};

$(document).ready(function(){
        
});
