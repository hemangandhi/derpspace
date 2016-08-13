#! /usr/bin/env python3
from curses import *
import os.path as pth

class FileNode:
    def __init__(self, path, parent, sibling = None, child = None):
        self.pth = path
        self.parent = parent
        self.sibling = sibling
        self.child = child

        #update the "compound pointers"
        if self.child != None:
            self.next = self.child
        elif self.sibling != None:
            self.next = self.sibling
        elif self.parent != None:
            self.next = self.parent.sibling
        else:
            self.next = None

        if self.next != None:
            self.prev = self.next.prev
            self.next.prev = self
            if self.prev != None:
                self.prev.next = self

    def add_child(self, newPath):
        self.child = FileNode(newPath, self, self.child, None)
    def add_sibling(self, sibPath):
        self.siblin = FileNode(sibPath, self.parent, self.sibling, None)
    def __abs__(self):
        if self.parent == None:
            return self.pth
        else:
            return pth.join(abs(self.parent), self.pth)

