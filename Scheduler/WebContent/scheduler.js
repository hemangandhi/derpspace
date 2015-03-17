var mainApp = angular.module('scheduleApp',[]);

mainApp.controller('CoursesCtrl',['$http','$scope',function($http, $scope){
	
	$http.get('courses.json').success(function(data){
		$scope.data = data;
	});
	
	angular.forEach($scope.data, function(val){
		val.yearTaken = 13;
	});
	
	$scope.types = [{'name':'LA',
					 'requirement':4,
					 'taken':0},
					{'name':'Math',
				     'requirement':4,
					 'taken':0},
					{'name':'History',
					 'requirement':3,
					 'taken':0},
					{'name':'Foreign lang',
					 'requirement':2,
					 'taken':0},
					{'name':'Elective',
					 'requirement':4,
					 'taken':0},
					{'name':'science',
					 'requirement':3,
					 'taken':0}];
	
	$scope.getMaxGrade = function(){
		var tempMax = 8;
		angular.forEach($scope.data,function(val){
			if(val.yearTaken != 13 && val.yearTaken > tempMax)
				tempMax = val.yearTaken;
		});
		return tempMax;
	};
	
	$scope.getCourseByName = function(name){
		var toRet = null;
		angular.forEach($scope.data,function(val){
			if(val.name == name)
				toRet = val;
		});
		return toRet;
	};
	
	$scope.prerecsMet = function(val){
		if(val.prerecs == null)
			return true;
		else{
			var toRet = true;
			angular.forEach(val.prerecs,function(rec){
				if(getCourseByName(rec).yearTaken == 13)
					toRet = false;
			});
			return toRet;
		}
	};
	
	$scope.fillCell = function(grade,type){
		if(grade != $scope.getMaxGrade() + 1){
			return;
		}
		var toRet = [];
		angular.forEach($scope.data, function(val){
			if((val.type == type.name || type.taken == type.requirement) 
					&& val.yearTaken == 13 && prerecsMet(val))
				toRet.push(val);
		});
		return toRet;
	};
}]);