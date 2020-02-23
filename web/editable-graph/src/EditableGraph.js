import React from 'react';
import Graph from 'vis-react';

class EditableGraph extends React.Component {
    constructor(props) {
        super(props);
	this.state = {
	    graph: {
		nodes: [{id: 1, label: 'root'}],
		edges: [{from: 1, to: 1}]
	    },
	    editor: {}
	};
    }

    renderEditor () {
        var getNodeInfo = function(_this, nodeId){
            var labelOfNode = (nodeId) => {
		return _this.state.graph.nodes.filter((node) => node.id === nodeId)[0].label
	    };
            var edgeAlterer = function(edge) {
		return function(){
                    var newGraph = {
			nodes: _this.state.graph.nodes,
			edges: _this.state.graph.edges.filter((e) => e !== edge)
		    };
		    _this.setState({graph: newGraph, editor: _this.state.editor});
		};
	    };

	    var addEdge = function(nodeId){
                return function () {
                    var destId = parseInt(document.getElementById('editor-selector').value.replace('-node'));
		    var newEdge = {from: nodeId, to: destId};
		    for(var i = 0; i < _this.state.graph.edges.length; i++){
                        if(_this.state.graph.edges[i].from === newEdge.from &&
			   _this.state.graph.edges[i].to === newEdge.to) {
			    alert('Enschuldigung! Es ist kiene multigraph!');
			    return;
			}
		    }
		    var newEdges = _this.state.graph.edges.concat([{from: nodeId, to: destId}]);
		    _this.setState({
			editor: _this.state.editor,
			graph: {nodes: _this.state.graph.nodes, edges: newEdges}
		    });
		};
	    }

	    var removeNode = function(nodeId) {
                return function () {
                    var newNodes = _this.state.graph.nodes.filter(n => n.id !== nodeId);
		    var newEdges = _this.state.graph.edges.filter(e => e.from !== nodeId && e.to !== nodeId);
		    _this.setState({
			editor: {},
			graph: {nodes: newNodes, edges: newEdges}
		    });
		};
	    }

            return (
	            <div>
		    You clicked on {labelOfNode(nodeId)}.
		    <button onClick={removeNode(nodeId)}>Remove this node</button>
		    It has edges into:
		    <ul>
                    {  _this.state.graph.edges &&
		       _this.state.graph.edges.filter((edge) => edge.from === nodeId)
		                              .map((edge) => (<li
				                              key={edge.from + '-' + edge.to}
							      alt="click to remove edge"
							      onClick={edgeAlterer(edge)}>
							      {labelOfNode(edge.to)}
							      </li>))
		    }
		    </ul>
                    <button onClick={addEdge(nodeId)}>Add edge</button> to
		    <select id="editor-selector">
                    {_this.state.graph.nodes.map((node) => (<option
							    key={node.id + "-edge-option"}
							    value={ node.id + "-node"}>{node.label}</option>))}
		    </select>
		    </div>);
	};
	
	if (!this.state.editor || !this.state.editor.selection) return;
        return getNodeInfo(this, this.state.editor.selection.nodes[0]);
    }

    render () {
        var _this = this;
	var setEditor = {
	    select: function (e) {
	        _this.setState({graph: _this.state.graph, editor: {selection: e}});
	    }
	};

	var addNode = function(){
            var maxId = _this.state.graph.nodes.map(n => n.id).reduce(Math.max, -1);
	    var newNode = {id: maxId + 1, label: document.getElementById('new-node-name').value};
	    var newGraph = {
		nodes: _this.state.graph.nodes.concat([newNode]),
		edges: _this.state.graph.edges
	    };
	    _this.setState({editor: _this.state.editor, graph: newGraph});
	}

        return (
	  <div>
	    <Graph
              graph={this.state.graph}
              events={setEditor} />
	    {this.renderEditor()}
	   <button onClick={addNode}>Add node</button> called <input type="text" id="new-node-name" />
	   </div>
	);
    }
}

export default EditableGraph;
