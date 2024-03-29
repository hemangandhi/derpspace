#! /usr/bin/env python
from __future__ import print_function
from sys import version_info, exc_info
from traceback import print_exception
import bots
import json

if version_info.major == 2:
  import SimpleHTTPServer as srv
  from SocketServer import TCPServer

  server = TCPServer

else:
  import http.server as srv
  server = srv.HTTPServer

class sixHandler(srv.SimpleHTTPRequestHandler):
  def send_JSON(self, json_dict):
    v = json.dumps(json_dict)
    self.send_response(200)
    self.send_header('content_type','application/json')
    self.send_header('content_length',str(len(v)))
    self.end_headers()
    self.wfile.write(v.encode('utf-8'))

  def do_GET(self):
    if self.path == '/bots':
      val = dict()
      for i in bots.bots_dict:
        if bots.bots_dict[i].startswith('@'):
          val[i] = "http://localhost:8080/move/" + bots.bots_dict[i][1:]
        else:
          val[i] = bots.bots_dict[i]
      
      self.send_JSON(val)
      return None
    else:
      srv.SimpleHTTPRequestHandler.do_GET(self)

  def do_POST(self):
    if self.path.startswith('/move'):
      l = int(self.headers['Content-Length'])
      gsd = json.loads(self.rfile.read(l).decode('utf-8'))
      try:
        mv = getattr(bots,self.path[len('/move/'):])(gsd['mat'], gsd['moves'], gsd['turn'])
        self.send_JSON({'x': mv[0], 'y': mv[1]})
      except Exception as e:
        print("Error in bot:")
        print_exception(*exc_info())
        self.send_JSON({'err': " : ".join(*exc_info())})
      finally:
        return None

s = server(("localhost", 8080), sixHandler)
s.serve_forever()
