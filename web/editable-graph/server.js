var app = require('express')();
var httpLib = require('http')
var http = httpLib.Server(app);
var io = require('socket.io')(http);
var path = require('path');

// The base route: serve index.html from the document root.
app.get('/', function(req, res){
  var express=require('express');
  app.use(express.static(path.join(__dirname)));
  res.sendFile(path.join(__dirname, '../chat-app', 'index.html'));
});

var graph = {};

// Listen application request on port 3000
http.listen(3000, function(){
  console.log('listening on *:3000');
});
