function dat = LoadData(path, fileName)
    dat = readtable(path + fileName);
    for colNames = (convertCharsToStrings(dat.Properties.VariableNames))
        if(iscell(dat.(colNames)))
            dat.(colNames) = convertCharsToStrings(dat.(colNames));
        end
    end
    if(isstring(dat.gazeX))
        dat.gazeX = str2double(dat.gazeX);
    end
    if(isstring(dat.gazeY))
        dat.gazeY = str2double(dat.gazeY);
    end
    if(isstring(dat.gazeXRelative))
        dat.gazeXRelative = str2double(dat.gazeXRelative);
    end
    if(isstring(dat.gazeYRelative))
        dat.gazeYRelative = str2double(dat.gazeYRelative);
    end

end
% 
% dat = readtable(RD + "MATLAB_Encoding_EMS.csv");
% for colNames = (convertCharsToStrings(dat.Properties.VariableNames))
%     if(iscell(dat.(colNames)))
%         dat.(colNames) = convertCharsToStrings(dat.(colNames));
%     end
% end
% if(isstring(dat.gazeX))
%     dat.gazeX = str2double(dat.gazeX);
% end
% if(isstring(dat.gazeY))
%     dat.gazeY = str2double(dat.gazeY);
% end
% if(isstring(dat.gazeXRelative))
%     dat.gazeXRelative = str2double(dat.gazeXRelative);
% end
% if(isstring(dat.gazeYRelative))
%     dat.gazeYRelative = str2double(dat.gazeYRelative);
% end
% 
% datEncoding = dat(dat.timePoint>=StartEnc & (dat.PostDec==0 | dat.timePoint<=EndEnc),:);
% 
% 
% mapEncRet = datEncoding(:,["ID","RetrievalDPID"]);
% mapEncRet.EncodingDPID = datEncoding.DataPointID;