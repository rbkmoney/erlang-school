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
    wsHost = $("#server").val()
    websocket = new WebSocket(wsHost);
    showScreen('<div class="alert alert-info" role="alert">Connecting to: ' +  wsHost + '</div>');
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

function getRooms() {
    if(websocket.readyState == websocket.OPEN){
        req = "{\"type\":\"get_rooms\"}";
        websocket.send(req);
      showScreen('<div class="alert alert-dark" role="alert">SENDING: <pre><code>' + prettyjson(req) + '</pre></code></div>');
  } else {
       showScreen('<div class="alert alert-danger" role="alert">websocket is not connected</div>');
  };
};

function joinRoom() {
    if(websocket.readyState == websocket.OPEN){
        room = $("#req_room").val();
        req = "{\"type\":\"join_room\", \"room_id\":"+room+"}";
        websocket.send(req);
      showScreen('<div class="alert alert-dark" role="alert">SENDING: <pre><code>' + prettyjson(req) + '</pre></code></div>');
  } else {
       showScreen('<div class="alert alert-danger" role="alert">websocket is not connected</div>');
  };
};

function setName() {
  if(websocket.readyState == websocket.OPEN){
      room = $("#req_room").val();
      txt = $("#req_content").val();
      req = "{\"type\":\"set_name\", \"room_id\":"+room+", \"content\":\""+txt+"\"}";
      websocket.send(req);
      showScreen('<div class="alert alert-dark" role="alert">SENDING: <pre><code>' + prettyjson(req) + '</pre></code></div>');
  } else {
       showScreen('<div class="alert alert-danger" role="alert">websocket is not connected</div>');
  };
};

function sendMessage() {
  if(websocket.readyState == websocket.OPEN){
      room = $("#req_room").val();
      txt = $("#req_content").val();
      req = "{\"type\":\"send_message\", \"room_id\":"+room+", \"content\":\""+txt+"\"}";
      websocket.send(req);
      showScreen('<div class="alert alert-dark" role="alert">SENDING: <pre><code>' + prettyjson(req) + '</pre></code></div>');
  } else {
       showScreen('<div class="alert alert-danger" role="alert">websocket is not connected</div>');
  };
};

function onOpen(evt) {
  showScreen('<div class="alert alert-success" role="alert">CONNECTED</div>');

  $("#connected").fadeIn('slow');
  $("#content").fadeIn('slow');
};

function onClose(evt) {
  showScreen('<div class="alert alert-warning" role="alert">DISCONNECTED</div>');
};

function onMessage(evt) {
  showScreen('<div class="alert alert-info" role="alert">RESPONSE: <pre><code>' + prettyjson(evt.data) + '</pre></code></div>');
};

function onError(evt) {
  showScreen('<div class="alert alert-warning" role="danger">ERROR: <pre><code>' + prettyjson(evt.data) + '</pre></code></div>');
};

function showScreen(txt) {
  $('#output').prepend(txt);
};

function clearScreen()
{
  $('#output').html("");
};

function prettyjson(json) {
    return JSON.stringify(JSON.parse(json), null, 2);
}
