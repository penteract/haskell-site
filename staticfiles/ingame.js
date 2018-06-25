

STARTED=4
AIP=2
GAMEOVER=8
DRAW=16
TURN=1
TIMEUP=32

function click(pos){
    $.post('makemove?pos='+pos+'&gameID='+gameID+'&playerID='+playerID,
        process)
}

function sendMsg(){
    $.post('msg',{"gameID":gameID,"msg":$("#msg").val()});
    $("#msg").val("");
}


function process(m){
    console.log(m)
    if(m.gameID == gameID){
        if (m.request=="update"){
            if((m.state&GAMEOVER)!=(lastj.state&GAMEOVER)){
                myalert("game over: "+(m.state&DRAW?"its a draw":
                    ((m.state&TURN)==player?"congratulations, you won":"sorry, you lost")))
            }
            $("#"+(m.state&TURN)).addClass("turn")
            $("#"+(1-m.state&TURN)).removeClass("turn")

            show(m.data);
            lastj=m
        }
        else if(m.request=="gameover"){
            if(m.reason=="timeup"){myalert(m.won?"congratulations, you won":"time up, sorry")}
            else myalert(m.won?"congratulations, you won":"sorry, you lost")
        }
        else if(m.request=="message"){
            myalert(m.content)
        }
    }
}

t=500
function poller(){
    $.get("getgame",{"gameID":gameID},updatet)
    POLLER=setTimeout(poller,t);t+=1000
}

function channeler(game){
    sock = createWebSocket("/"+game+"?gameID="+gameID+'&playerID='+playerID)
    sock.onmessage=(e=>{console.log(e);process(JSON.parse(e.data))})
    //$.get("getgame",{"gameID":gameID},updatet)
    //POLLER=setTimeout(poller,t);t+=1000
}

function createWebSocket(path) {
    var host = window.location.host;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}
