classdef DataLoader
    % This Class is loading and containing data
    
	% HMLET 
	% Ver. 2.0 Jan 2024
	% Alireza Kazemi kazemi@ucdavis.edu
    
    properties (Access = public)
        path = pwd; % Directory to the data files
        encoding;  % Encoding data
        retrieval;  % Retrieval data
    end
    properties (Access = private)
        dat; 
    end
    methods (Access = public)
        function Obj = DataLoader
            uiwait(questdlg("Please select the Encoding csv file            ",...
                "Encoding","Browse","Cancel"));
            Obj.encoding = uiimport("-file");
            uiwait(msgbox("Please select the Retrieval csv file           ","Retrieval","modal"));
            Obj.retrieval = uiimport("-file");
        end
    end
end
answer = questdlg(quest,dlgtitle,defbtn)
