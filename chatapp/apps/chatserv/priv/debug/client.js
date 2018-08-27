var websocket;

$(document).ready(init);

function init() {
    $('#server').val("ws://" + window.location.host + "/ws");

    if(!("WebSocket" in window)) {
        $('#status').append('<span class="nav-link text-danger">websockets are not supported</span>');
        $("#navigation").hide();
    } else {
        $('#status').append('<span class="nav-link text-success">websockets are supported</span>');
        connect();
    }

    $("#connected").hide();
    $("#content").hide();
};

function connect()
{
    wsHost = $("#server").val();
    websocket = new WebSocket(wsHost);
    showScreen('info', 'Connecting to: ' +  wsHost);
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
};

function disconnect() {
    websocket.close();
};

function toggle_connection(){
    if(websocket.readyState == websocket.OPEN){
        disconnect();
    } else {
        connect();
    };
};

function sendRequest(requestString) {
    if(websocket.readyState == websocket.OPEN){
        showScreen('dark', 'SENDING: <pre><code>' + prettyjson(requestString) + '</pre></code>');
        websocket.send(requestString);
    } else {
        showScreen('danger', 'websocket is not connected');
    };
}

function getRooms() {
    req = "{\"type\":\"get_rooms\"}";

    sendRequest(req);
};

function joinRoom() {
    room = $("#req_room").val();
    req = "{\"type\":\"join_room\", \"room_id\":"+room+"}";

    sendRequest(req);
};

function setName() {
    room = $("#req_room").val();
    txt = $("#req_content").val();
    req = "{\"type\":\"set_name\", \"room_id\":"+room+", \"content\":\""+txt+"\"}";

    sendRequest(req);
};

function sendMessage() {
    room = $("#req_room").val();
    txt = $("#req_content").val();
    req = "{\"type\":\"send_message\", \"room_id\":"+room+", \"content\":\""+txt+"\"}";

    sendRequest(req);
};

function onOpen(evt) {
    showScreen('success', 'CONNECTED');

    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
};

function onClose(evt) {
    showScreen('warning', 'DISCONNECTED');
};

function onMessage(evt) {
    showScreen('info', 'RESPONSE: <pre><code>' + prettyjson(evt.data) + '</pre></code>');
};

function onError(evt) {
    showScreen('danger', 'ERROR: <pre><code>' + prettyjson(evt.data) + '</pre></code>');
};

function showScreen(type, message) {
    html = '<div class="alert alert-'+ type +'" role="'+ type +'">' +
        '<span class="text-muted"><small><em>' + new Date().toLocaleString() + '</em></small></span> ' +
        message + '</div>';

    $('#output').prepend(html);
};

function clearScreen()
{
    $('#output').html("");
};

function prettyjson(json) {
    return JSON.stringify(JSON.parse(json), null, 2);
}
