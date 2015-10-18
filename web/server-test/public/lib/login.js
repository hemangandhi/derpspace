var app = angular.module('login',[]);

app.controller('lgCtrl',function($scope, $http){
	$scope.username = "goldfish";
	$scope.password = "******";
	$scope.lgIn = false;
	$scope.lgErr = false;
	
	$scope.login = function(){
		$http.post('/nm', {'userName': $scope.userName,
			               'password': $scope.password}).success(function(data){
			            	   if(data['stat'] == "success"){
			            		   lgIn = true;
			            	   }else if(data['stat'] == "error"){
			            		   lgErr = false;
			            	   }
			            	   
			               });
	}
	
});