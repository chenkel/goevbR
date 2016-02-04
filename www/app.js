(function(){	
	$(document).ready(function() {
		var mapA = $("#mapOrigin");
		var mapB = $("#mapDestination");
		
		console.log(mapA, 'maaaap'); 
	
	
		mapDestination.sync(mapOrigin);
		mapB.sync(mapA);
    });
	
})()