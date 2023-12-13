close all
clear
%% Import data from text file

opts = delimitedTextImportOptions("NumVariables", 16);

% Specify range and delimiter
opts.DataLines = [2, Inf];
opts.Delimiter = ",";

% Specify column names and types
opts.VariableNames = ["ParticipantName", "StudioTestName", "trialIdx", "PostDec", "CounterBalance", "MediaName", "trialName", "RecordingTimestamp", "responseME", "GazeEventType", "GazeX", "GazeY", "EventName", "timeStamp", "GazeX_Interp", "GazeY_Interp"];
opts.VariableTypes = ["double", "double", "double", "double", "double", "categorical", "categorical", "double", "categorical", "categorical", "double", "double", "categorical", "double", "double", "double"];

% Specify file level properties
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Specify variable properties
opts = setvaropts(opts, ["MediaName", "trialName", "responseME", "GazeEventType", "EventName"], "EmptyFieldRule", "auto");
opts = setvaropts(opts, ["ParticipantName", "StudioTestName"], "TrimNonNumeric", true);
opts = setvaropts(opts, ["ParticipantName", "StudioTestName"], "ThousandsSeparator", ",");

% Import the data
d = readtable("D:\Projects\HMLET\DataFiles\Debug\interpolatedModified.csv", opts);

clear opts

%%
xlimits = [0,1024];
ylimits = [200,800];

d = d(d.timeStamp<5000,:);
d = d(d.GazeY_Interp<=ylimits(2),:);
d = d(d.GazeY_Interp>ylimits(1),:);
d = d(d.GazeX_Interp<=xlimits(2),:);
d = d(d.GazeX_Interp>xlimits(1),:);
% t = d.RecordingTimestamp;
t = d.timeStamp;

%%
for i=10 %:max(d.trialIdx)
    figure
    % subplot(3,4,i-4)
    t = d.timeStamp(d.trialIdx==i);
    % t = t+tOffset;
    % tOffset = max(t);

    X = d.GazeX_Interp(d.trialIdx==i);
    Y = d.GazeY_Interp(d.trialIdx==i);
    plot3(X,Y,t,'Color',"#77AC30",'LineWidth',2)
    hold on
    X = d.GazeX(d.trialIdx==i);
    Y = d.GazeY(d.trialIdx==i);
    plot3(X,Y,t,'Color',"#D95319",'LineWidth',2)

    % plot3(repmat(max(t),length(X),1),X,Y,'LineWidth',1)
    plot3(X,Y,zeros(length(X),1),'LineWidth',1)

    map = zeros(1280,1024);
    IndstNA = ~(isnan(X) | isnan(Y));
    inds = round(sort(X(IndstNA)+1280.*(Y(IndstNA)-1)));
    % inds = categorical(inds);
    S = histcounts(categorical(inds))';
    map(unique(inds)) = S;
    map = imgaussfilt(map,5);
    mesh(map');
    % imagesc(map)
    xlim(xlimits)
    ylim(ylimits)
    xlabel('X')
    ylabel('Y')
    zlabel('time')
    title("Trial Number = "+i)
    colormap("hot")
    % box on
end