//JS

var player = function(normalTime, byoYomiCount, byoYomiDurr, playerDelay, nextPlayer){
        var currByo = byoYomiDurr;
        var currInt = null;
        this.next = nextPlayer;
        this.playTime = function(uiUpdater, doTimeout){
                var playerFn = function(){
                        if(normalTime > 0){
                                uiUpdater(1, normalTime);
                                normalTime--;
                        }else{
                                uiUpdater(byoYomiCount, currByo);
                                if(currByo > 0)
                                        currByo--;
                                else{
                                        byoYomiCount--;
                                        currByo = byoYomiDurr;
                                }
                                if(byoYomiCount == 0)
                                        doTimeout();
                        }
                };
                currInt = setInterval(playerFn, 1000);
        };
        this.resetByo = function(){ currByo = byoYomiDurr;};
        this.endTurn = function(){
                if(currInt !== null){
                        clearInterval(currInt);
                        this.resetByo();
                        return this.next;
                }
        };
};

var game = function(players, UIUpdater, doTimeOut){

};

$(document).ready(function(){
        
});
