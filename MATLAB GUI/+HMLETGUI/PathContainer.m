classdef PathContainer
    %This class saves and return the directory of multimedia files.
    
	
	% Help Seeking Experiment 
	% Ver. 5.12 July 17 2018  
	% Alireza Kazemi kazemi@ucdavis.edu
	
	
    properties
        Audio
        Graphics
        Practice
        Stimuli
        Output
        Data
    end
    
    methods
        %% Constructor of the class to initialize Path
        function Obj = PathContainer
            Obj.Audio = [pwd,'\Files\Audio\'];
            Obj.Graphics = [pwd,'\Files\Graphics\'];
            Obj.Practice = [pwd,'\Files\Practice_Stimuli\'];
            Obj.Stimuli = [pwd,'\Files\Stimuli\'];
            Obj.Output = [pwd,'\Output\'];
            Obj.Data = [pwd,'\Files\Data\'];
        end
    end
    
end

