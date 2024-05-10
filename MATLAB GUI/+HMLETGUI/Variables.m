classdef Variables
    % This Class keeps variables
    
	% HMLET 
	% Ver. 2.0 Jan 2024
	% Alireza Kazemi kazemi@ucdavis.edu
    
    properties
        HMLETversion;
        versionInfo = 'Ver. 2.00 Jan 2024';
    end
    methods 
        function Obj = Variables
            Obj.HMLETversion = string(Obj.versionInfo(6:9));
            Obj.versionInfo = string(Obj.versionInfo);
        end
    end
end

