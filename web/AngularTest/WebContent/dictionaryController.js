var app = angular.module('dictionaryApp',[]);

app.controller('dictionaryCtrl', function($scope, $http){
	$http.get('https://montanaflynn-dictionary.p.mashape.com/define?word=' + $scope.word).success(
		function(data, status, headers, config){
			$scope.defns = data;
	}).error(function(data, status, headers, config){
		$scope.defns = [];
	});
	
	
});
//Fails - lack of API key!