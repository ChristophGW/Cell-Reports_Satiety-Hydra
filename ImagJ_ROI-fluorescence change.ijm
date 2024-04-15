//Sript for extracting population dynamics of neurons by taking a ROI of the whole animal/Frame and extracting the mean grey value// 
//select the input directory where the folder with the exported tiff files are
inputDir = getDirectory('Choose Input Directory');
//select the output directory where the tables should be saved
outputDir = getDirectory('Choose Output Directory'); 

//extract list of all files in the input directory
listFiles(inputDir); 
//retrieves the list of files in the specified directory
list = getFileList(inputDir);

//iterates through the list of files/directories
function listFiles(dir) { 
        list = getFileList(dir); 
        for (i=0; i<list.length; i++) { 

                setBatchMode(true);

for (i=0; i<list.length; i++) {
	file = dir + list[i];
	//opens folder as virtual stack
    open(file, "virtual");
    // Duplicate the virtual stack
	run("Duplicate...", "duplicate");
	//renames duplicated stack
	rename("Duplicate_" + i);
    
	//Image processing
	//gamma correction
	run("Gamma...", "value=1.5 stack");
	//Extract value for adjusting the whole stack later
	//Z-Projection of the stack: Maximum intensity
	run("Z Project...", "projection=[Max Intensity]");
	//select maximum projection image
	selectImage("MAX_"+ "Duplicate_" + i);
	//measure the mean value of the stack
	run("Set Measurements...", "mean min stack redirect=None decimal=3");
	run("Measure");
	//select Result window
	selectWindow("Results");
	//extract the mean value
	meanVal = getResult("Mean");
	//close result window
	close("Results");
	//close maximum projection
	close("MAX_"+ "Duplicate_" + i);
	
	//select the duplicated stack
	selectImage("Duplicate_" + i);
	
	//set multiplication factor based on the extracted mean value
	multiply_factor = (255/meanVal);
	
	//multiply images with extracted value
	run("Multiply...", "value=" + multiply_factor + " stack");
	run("Apply LUT", "stack");
	//adjust intensity levels into a range between 100 and 255 to remove background fluorescence
	maxVal = 255;
    minVal = 255 - 155;
    setMinAndMax(minVal, maxVal);
    run("Apply LUT", "stack");

    // Create a rectangle selection covering the entire image
	makeRectangle(0, 0, getWidth(), getHeight());
	
	// Add the selection to the ROI Manager
	makeRectangle(0, 0, getWidth(), getHeight());
    roiManager("Add");
    
    //set measurement and measure mean value of the whole stack
    run("Set Measurements...", "mean redirect=None decimal=3");
	roiManager("Multi Measure");
    selectWindow("Results");
    
    //select and save result window as csv file
    selectWindow("Results");
    measurementsPath = outputDir + File.getNameWithoutExtension(list[i]) + ".csv";
    saveAs("Results", measurementsPath);    
    
    close();
    // Delete the ROI from the ROI Manager
    roiManager("Delete");
	

//close();
    }
setBatchMode(false); 

        } 
} 