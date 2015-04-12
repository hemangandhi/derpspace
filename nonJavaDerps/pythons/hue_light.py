import http.client as cli
import json

modifiable_attrs = {"on":[(lambda x: x == "True" or x == "False"),"Must be a boolean.", lambda x: x == "True"],
                    "hue":[(lambda x: x.isdigit() and int(x) <= 65535 and int(x) >= 0),"Must be an integer x, 0 <= x <= 65535.", lambda x: int(x)],
                    "bri":[(lambda x: x.isdigit() and int(x) <= 254 and int(x) >= 1),"Must be an integer x, 1 <= x <= 254.", lambda x: int(x)],
                    "sat":[(lambda x: x.isdigit() and int(x) <= 254 and int(x) >= 0),"Must be an integer x, 1 <= x <= 254.", lambda x: int(x)]}

class Light(object):
    def __init__(self, conn, iden, user_id):
        self.conn = conn
        self.api_url = "/api/" + user_id + "/"
        self.id = iden
        conn.request("GET",self.api_url + "lights/" + iden)
        self.state_dict = json.loads(conn.getresponse().read().decode('utf-8'))['state']
    def is_on(self):
        return self.state_dict['on']
    def get_hue(self):
        return self.state_dict['hue']
    def get_brightness(self):
        return self.state_dict['bri']
    def set_state(self, attrib, val):
        self.conn.request("PUT",self.api_url + "lights/" + self.id + "/state",
                     json.dumps({attrib:val}))
        self.conn.getresponse()
        self.update_state()
    def set_state_multiple(self,attrib_dict):
        self.conn.request("PUT",self.api_url + "lights/" + self.id + "/state",
                     json.dumps(attrib_dict))
        self.conn.getresponse()
        self.update_state()
    def update_state(self):
        self.conn.request("GET",self.api_url + "lights/" + self.id)
        self.state_dict = json.loads(self.conn.getresponse().read().decode('utf-8'))['state']
        return self.state_dict
    def turn_on(self):
        self.set_state("on",True)
    def turn_off(self):
        self.set_state("on",False)
    def toggle_on(self):
        self.set_state("on",not self.state_dict['on'])    

def run():
    print("Welcome to hue cmdln. Type in the light id.")
    conn = cli.HTTPConnection("10.0.0.26")
    l_id = input("id > ")
    conn.request("GET","/api/newdeveloper/lights")
    while l_id not in json.loads(conn.getresponse().read().decode('utf-8')):
        l_id = input("id (" + l_id + " was invalid)> ")
        conn.request("GET","/api/newdeveloper/lights")
    light_to_use = Light(conn, l_id, "newdeveloper")    
    print('load success!')
    print("The light's current state:")
    print(light_to_use.state_dict)
    init_state = light_to_use.state_dict
    exi = input("Type exit to exit >")
    while exi != "exit":
        to_set = get_modification_dict()
        if to_set != {}:
            light_to_use.set_state_multiple(to_set)
        exi = input("Type exit to exit or read to see the light's current state >")
        if exi == "read":
            print(light_to_use.state_dict)
    if input("Type r to revert the light to it's initial state >") == "r":        
        light_to_use.set_state_multiple(init_state)
    conn.close()
    input("Bye!")

def get_modification_dict():
    print("You can now modify the light's state. These are the attributes you can modify.")
    for i in modifiable_attrs:
        print(i, " where values ", modifiable_attrs[i][1])
    to_ret = {}    
    s = input('type in "y" to keep adding values to modify >')
    while s == "y":
        key = input("type in the attribute you'd like to alter. >")
        while key not in modifiable_attrs:
            key = input("type in the attribute you'd like to alter or 'p' for a list of valid attributes. >")
            if key=='p':
                for i in modifiable_attrs:
                    print(i, " where values ", modifiable_attrs[i][1])
        val = input("Type in the value you'd like to set " + key + " to. >")
        while not modifiable_attrs[key][0](val):
            print("Sorry that's not an acceptable value.")
            print(key + " " + modifiable_attrs[key][1])
            val = input("Type in the value you'd like to set " + key + " to. >")
        to_ret[key] = modifiable_attrs[key][2](val)
        print("Attributes you've added:")
        print(to_ret)
        s = input('type in "y" to keep adding values to modify >')
    return to_ret    

if __name__ == "__main__":
    run()
