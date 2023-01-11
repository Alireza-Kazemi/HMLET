% FeatureEx
function [featList,fIdx] = FeatureList(fixationNames)


%% feature extraction
fIdx = 1;
featList = ["NGP_T";"NGP_L";"NGP_C"];
fIdx = fIdx+3;
featList = cat(1,featList,["SW_T";"SW_L";"SW_C"]); 
fIdx = fIdx+3;

for ind = 1:length(fixationNames)
    featList = cat(1,featList,"RFixtime_"+string(fixationNames(ind))); 
    fIdx = fIdx+1;
end

for ind = 1:length(fixationNames)
    featList = cat(1,featList,"AverageDistance_"+string(fixationNames(ind))); 
    fIdx = fIdx+1;
end

for ind = 1:length(fixationNames)
    featList = cat(1,featList,"stdDistance_"+string(fixationNames(ind))); 
    fIdx = fIdx+1;
end

for ind = 1:length(fixationNames)
    featList = cat(1,featList,"TotalDistance_"+string(fixationNames(ind))); 
    fIdx = fIdx+1;
end

featList = cat(1,featList,"TotalDistance");
fIdx = fIdx+1;
featList = cat(1,featList,"AverageDistance");
fIdx = fIdx+1;
featList = cat(1,featList,"stdDistance");
fIdx = fIdx+1;
featList = cat(1,featList,"Totalswitches");
fIdx = fIdx+1;


for ind = 1:length(fixationNames)
    featList = cat(1,featList,"GazeProportions_"+string(fixationNames(ind))); 
    fIdx = fIdx+1;
end



featList = cat(1,featList,"NGPUnique");
fIdx = fIdx+1;
featList = cat(1,featList,"RevisitingGP");  
fIdx = fIdx+1;


featList = cat(1,featList,"MeanInfBorder");      % Mean informativeness to Border
fIdx = fIdx+1;
featList = cat(1,featList,"MeanInfItem");        % Mean informativeness to item
fIdx = fIdx+1;
featList = cat(1,featList,"MeanInfLure");        % Mean informativeness to lure
fIdx = fIdx+1;

featList = cat(1,featList,"MinInfBorder");       % Min informativeness to Border
fIdx = fIdx+1;
featList = cat(1,featList,"MinInfItem");         % Min informativeness to item
fIdx = fIdx+1;
featList = cat(1,featList,"MinInfLure");         % Min informativeness to lure
fIdx = fIdx+1;

featList = cat(1,featList,"stdInfBorder");       % std informativeness to Border
fIdx = fIdx+1;
featList = cat(1,featList,"stdInfItem");         % std informativeness to item
fIdx = fIdx+1;
featList = cat(1,featList,"stdInfLure");        % std informativeness to lure



end

