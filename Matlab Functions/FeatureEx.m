% FeatureEx
function feats = FeatureEx(gaze,fixationInfo,timePoints,centerDat,fixationNames,fNum)


nanIndex = ~(fixationInfo.fixation=="NA");
gaze = gaze(nanIndex,:);
fixation = fixationInfo.fixation(nanIndex,:);
response = fixationInfo.response(nanIndex,:);
timePoints = timePoints(nanIndex);
interpolated = fixationInfo.Interpolated(nanIndex);


sameGazePointThreshold = 5; %minimum distance in pixel to count as new gazepoint

feats = zeros(1,fNum);

fixatedOn = zeros(size(gaze,1),length(fixationNames));
for ind = 1:length(fixationNames)
    fixatedOn(:,ind) = double(fixation==fixationNames(ind));
end

gazeIdx = zeros(size(fixatedOn));
fixationIdx = zeros(size(fixatedOn));
for ind = 1:length(fixationNames)
    fIndex = 1;
    a = fixatedOn(:,ind);
    b = diff([0;a;0]);
    b(b>0)=0;
    b = abs(b);
    [~, c] = findpeaks(b);
    c = c-1;
    b = a;
    g = zeros(size(a));
    ci0 = 1;
    for ci = 1:(length(c))
        b(ci0:c(ci)) = cumsum(b(ci0:c(ci)));   
        gt = b(ci0:c(ci));
        if(max(gt)>0)
            gt(gt>0) = fIndex;
            g(ci0:c(ci)) = gt;
            fIndex = fIndex+1;
        end
        ci0 = c(ci)+1;
    end
    b(ci0:end) = cumsum(b(ci0:end));
    gt = b(ci0:end);
    if(max(gt)>0)
        gt(gt>0) = fIndex;
        g(ci0:end) = gt;
        fIndex = fIndex+1;
    end
    gazeIdx(:,ind) = b;
    fixationIdx(:,ind) = g;
end

%% Indexing the gaze points based on fixation point
diffGaze = diff(gaze(:,1:2));
diffGaze(:,3) = sqrt(diffGaze(:,1).^2+diffGaze(:,2).^2);
% diffGaze(diffGaze(:,3)<=sameGazePointThreshold,3)=0;


gaze(2:end,3) = diffGaze(:,3); 
gaze(1,3)=0;%100; % dummy number to not be confused with zero space

% 


%% feature extraction
% Featurelist = ["NGP_T","NGP_L","NGP_C","SW_T","SW_L","SW_C",... 
%               "RFixtime_T","RFixtime_L","RFixtime_C",...
%               "AvgDistance_T","AvgDistance_L","AvgDistance_C",...
%               "StdDistance_T","StdDistance_L","StdDistance_C",...
%               "TotalDistance_T","TotalDistance_L","TotalDistance_C",...
%               "TotalDistance","AvgDistance","StdDistance","TotalSW",...
%               "GazeProportion_Target","GazeProportion_Lure",...
%               "NGP_Unique", "Revisiting Same GP","Blinks per Sample",...
% ]';
fIdx = 1;
feats(fIdx:(fIdx+2)) = sum(fixatedOn); %"NGP_T","NGP_L","NGP_C"
fIdx = fIdx+3;
feats(fIdx:(fIdx+2)) = max(fixationIdx); % "SW_T","SW_L","SW_C"
fIdx = fIdx+3;

for ind = 1:length(fixationNames)
    temp = fixationIdx(:,ind);
    temp = temp(temp~=0);
    if(isempty(temp))
        feats(fIdx) = 0; % "RFixtime_AOI"
    else
        temp = countcats(categorical(temp));
        feats(fIdx) = mean(temp); % "RFixtime_AOI"
    end
    fIdx = fIdx+1;
end


for ind = 1:length(fixationNames)
    temp = gaze(fixation==fixationNames(ind),3);
    if (isempty(temp))
        feats(fIdx) = 0;% "Average Distance_AOI"
    else
        feats(fIdx) = mean(temp);% "Average Distance_AOI"
    end
    fIdx = fIdx+1;
end

for ind = 1:length(fixationNames)
    temp = gaze(fixation==fixationNames(ind),3);
    temp = temp(2:end);
    if (isempty(temp))
        feats(fIdx) = 0;            % "std Distance_AOI"
    else
        feats(fIdx) = std(temp);   % "std Distance_AOI"
    end
    fIdx = fIdx+1;
end

for ind = 1:length(fixationNames)
    temp = gaze(fixation==fixationNames(ind),3);
    if (isempty(temp))
        feats(fIdx) = 0;           % "Total Distance_AOI"
    else
        feats(fIdx) = sum(temp);   % "Total Distance_AOI"
    end
    fIdx = fIdx+1;
end


feats(fIdx) = sum(gaze(2:end,3)); % "Total Distance"
fIdx = fIdx+1;
feats(fIdx) = mean(gaze(2:end,3)); % "Average Distance"
fIdx = fIdx+1;
feats(fIdx) = std(gaze(2:end,3)); % "std Distance"
fIdx = fIdx+1;
feats(fIdx) = sum(max(fixationIdx)); % "Total switches"
fIdx = fIdx+1;

SS = sum(fixatedOn,'all');
for ind = 1:length(fixationNames)
    feats(fIdx) = sum(fixatedOn(:,ind))./SS; % "GazeProportions"
    fIdx = fIdx+1;
end


clustersCentroids = clusterXYpoints(gaze(:,1:2),sameGazePointThreshold/2);
feats(fIdx) = size(clustersCentroids,1);  % "Number of Unique Gaze points"
fIdx = fIdx+1;
feats(fIdx) = size(gaze,1)-size(clustersCentroids,1);  %"Revisiting the same point"
fIdx = fIdx+1;



clustersCentroids = clusterXYpoints(gaze(:,1:2),25);
border = clustersCentroids(:,1) - sqrt((clustersCentroids(:,1)-1280).^2);
item = sqrt((clustersCentroids(:,1) - centerDat.X_CenterTarget).^2+(clustersCentroids(:,2) - centerDat.Y_Center).^2);
lure = sqrt((clustersCentroids(:,1) - centerDat.X_CenterLure).^2+(clustersCentroids(:,2) - centerDat.Y_Center).^2);

feats(fIdx) = mean(1./border);      % Mean informativeness to Border
fIdx = fIdx+1;
feats(fIdx) = mean(1./item);        % Mean informativeness to item
fIdx = fIdx+1;
feats(fIdx) = mean(1./lure);        % Mean informativeness to lure
fIdx = fIdx+1;

feats(fIdx) = min(1./border);       % Min informativeness to Border
fIdx = fIdx+1;
feats(fIdx) = min(1./item);         % Min informativeness to Border
fIdx = fIdx+1;
feats(fIdx) = min(1./lure);         % Min informativeness to Border
fIdx = fIdx+1;

feats(fIdx) = std(1./border);       % std informativeness to Border
fIdx = fIdx+1;
feats(fIdx) = std(1./item);         % std informativeness to Border
fIdx = fIdx+1;
feats(fIdx) = std(1./lure);         % std informativeness to Border

% feats(27) = length(findpeaks(diff([0;interpolated])))/length(interpolated); % "Blinks per Sample"
feats(isnan(feats))=0;

end

