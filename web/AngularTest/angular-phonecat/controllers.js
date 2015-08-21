var phonecatApp = angular.module('phonecatApp',[]);

phonecatApp.controller('PhoneListCtrl', ['$scope','$http',function($scope, $http){
	$http.get('phones.json').success(function(data){
		$scope.phones = data;
	});
	
	$scope.orderProp = 'age';
}]);
//https://github.com/angular/angular-phonecat/app/phones/phones.json