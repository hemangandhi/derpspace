"""
Vague idea: function and variable nodes that are made into a graph with edges we are trying to learn.
The learning will be Hebbian, so if two values change, edge weights between them change with the
same sign as their correlation.
"""

from dataclass import dataclass
from enum import Enum

class VariableMode(Enum):
    """
    A flagging enum to record how a variable is being accessed, to change the mutability.
    """
    LOADING = 0
    EXECUTING = 1

class BoundedQueue:
    """
    A bounded queue with a dynamically growing bound when in the LOADING mode.
    """
    def __init__(self, bound=0):
        self.mode = VariableMode.LOADING
        self.queue = []
        self.bound = bound
    def __iadd__(self, other):
        if self.mode != VariableMode.EXECUTING:
            raise ValueError("Cannot add to unloaded queue")
        self.queue.append(other)
        if len(self.queue) > bound:
            self.queue = self.queue[1:]
    def increase_bound(self, bound):
        if self.mode != VariableMode.LOADING:
            raise ValueError("Cannot increase the bound on a loaded queue")
        self.bound = max(bound, self.bound)
    def peek(self):
        if self.mode != VariableMode.EXECUTING:
            raise ValueError("Cannot peek on an unloaded queue")
        return self.queue[0]
    def __getitem__(self, idx):
        if self.mode != VariableMode.EXECUTING:
            raise ValueError("Cannot peek on an unloaded queue")
        return self.queue[idx]
    def start_executing(self):
        self.mode = VariableMode.EXECUTING

class Edgetype(Enum):
    """
    Whether the relation between variables is additive or multiplicative.
    """
    SUM = 0
    PRODUCT = 1

class VariableNode:
    """
    A term with a variable in a differential equation.
    """
    def __init__(self):
        self.neighbors = dict()
        self.delay_values = []
    def add_dependency(self, other: VariableNode, relation: EdgeType, new_delay=0):
        
