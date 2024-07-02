classdef DataCollector
    % This Class is going to collect and save reports and results
    
	% Help Seeking Experiment
	% Ver. 5.12 July 17 2018
	% Alireza Kazemi kazemi@ucdavis.edu
	
	
    properties        
        Data   % The table that stores all of the data and results
        Lists   % Contains List of Stimulies
        Index   % Contains the indexes of old/new and valid/invalid
        Index_NonCued % Contains the indexes for non cued Stimuli
        Index_Valid % Contains the index for invalid cues
        Index_Invalid % Contains the index for invalid cues
        List_Number % contains the original list number
        Stimuli % Contains Stimuli for each part
        Path % stores the Path
        Participant_Code % the Participant's Code
        Codes = Code; % Contains the codes for reponses
        Encoding_Practice_items
        Testing_Practice_Training_items 
        Testing_Practice_Training_isold
        Testing_Practice_Trials_items 
        Testing_Practice_Trials_isold 
        Testing_Practice_Trials_iscued
        Testing_Practice_Trials_isvalid

    end
    
    
    methods
        %% Constructor of the Class
        function Obj = DataCollector(Participant_Code,Age,Number,ListOrder,Confidence_Order,Group,...
                                       t_fixation,t_encoding,t_mute,Block_order)
            %% Create Data Table
            Trials = 221; % Total Items
            Vars = Variables;
            Ver = Vars.Version;
            Obj.Participant_Code = Participant_Code;
            
            VarNames = {'code',...
                        'age',...
                        'number',...
                        'version',...
                        'list_order',...
                        'block_order',...
                        'confidence_order',...
                        'group',...
                        'fixation',...
                        'encoding',...
                        'mute',...
                        'list_number',...
                        'phase',...
                        'subphase',...
                        'old_new',...
                        'hint',...
                        'hint_type',...
                        'stimuli',...
                        'passed',...
                        'choice',...
                        'repeat',...
                        'coordinates_choice',...
                        'coordinates_conf',...
                        'assess',...
                        'hint_status',...
                        'confidence',...
                        't_trial',...
                        't_choice',...
                        't_recall',...
                        't_hint',...
                        't_confidence',...
                        'date_time'};  
            code = repmat(Participant_Code,Trials,1);
			age = ones(Trials,1)*Age;
            number = ones(Trials,1)*Number;
            version = string(repmat(Ver,Trials,1));
            list_order = ones(Trials,1)*ListOrder;
            block_order = ones(Trials,1)*Block_order;
            confidence_order = ones(Trials,1)*Confidence_Order;
            group = ones(Trials,1)*Group;
            fixation = ones(Trials,1)*t_fixation;
            encoding = ones(Trials,1)*t_encoding;
            mute = ones(Trials,1)*t_mute;
            list_number = zeros(Trials,1);
            phase = string(repmat('NULL',Trials,1));
            subphase = string(repmat('NULL',Trials,1));
            old_new = false(Trials,1);
            hint = false(Trials,1);
            hint_type = false(Trials,1);
            stimuli = string(repmat('NULL',Trials,1));
            passed = false(Trials,1);
            choice = ones(Trials,1)*-1;
            repeat = zeros(Trials,1);
            coordinates_choice = zeros(Trials,2);
            coordinates_conf = zeros(Trials,2);
            assess = ones(Trials,1)*-1;
            hint_status = string(repmat('NULL',Trials,1));
            confidence = zeros(Trials,1);
            t_trial = zeros(Trials,1);
            t_choice = zeros(Trials,1);
            t_recall = zeros(Trials,1);
            t_hint = zeros(Trials,1);
            t_confidence = zeros(Trials,1);
            date_time = string(repmat('NULL',Trials,1));
            
            Obj.Data = table(code,...							 
                             age,...
                             number,...
                             version,...
                             list_order,...
                             block_order,...
                             confidence_order,...
                             group,...
                             fixation,...
                             encoding,...
                             mute,...
                             list_number,...
                             phase,...
                             subphase,...
                             old_new,...
                             hint,...
                             hint_type,...
                             stimuli,...
                             passed,...
                             choice,...
                             repeat,...
                             coordinates_choice,...
                             coordinates_conf,...
                             assess,...
                             hint_status,...
                             confidence,...
                             t_trial,...
                             t_choice,...
                             t_recall,...
                             t_hint,...
                             t_confidence,...
                             date_time,...
                             'VariableNames',VarNames);

            %% ===================== Create Stimuli List
            Obj.Path = PathContainer;
            List_Index = [1,2,3,4;...
                          2,1,4,3;...
                          3,4,1,2;...
                          4,3,2,1];
            Lists = load([Obj.Path.Data,'Lists.mat']);
            Lists = Lists.Lists;
%             Lists = circshift(Lists.Lists,-(ListOrder-1),2);   % ================> Check            
            List_Index = List_Index(ListOrder,:);
            Obj.Lists = Lists(:,List_Index);
            Lists =  Lists(:,List_Index);
                        
            for i=1:4
                Ind_Pool = randperm(32);
                Obj.Index_Valid.(['L',num2str(i),'_P1']) = Ind_Pool(1:9); % Index Valid for List i Phase 1
                Obj.Index_Valid.(['L',num2str(i),'_P2']) = Ind_Pool(10:18); % Index Valid for List i Phase 2
                Obj.Index_Invalid.(['L',num2str(i),'_P1']) = Ind_Pool(19:21); % Index Invalid for List i Phase 1
                Obj.Index_Invalid.(['L',num2str(i),'_P2']) = Ind_Pool(22:24); % Index Invalid for List i Phase 2
                Obj.Index_NonCued.(['L',num2str(i),'_P1']) = Ind_Pool(25:28); % Index NonCued for List i Phase 1
                Obj.Index_NonCued.(['L',num2str(i),'_P2']) = Ind_Pool(29:32); % Index NonCued for List i Phase 2
            end            
            
            % Study:
            Perm_Ind = randperm(64);
            Obj.Stimuli.Study = [Lists(:,1);Lists(:,3)];
            Obj.List_Number.Study = [ones(32,1)*List_Index(1);ones(32,1)*List_Index(3)];
            Obj.Stimuli.Study = Obj.Stimuli.Study(Perm_Ind);
            Obj.List_Number.Study = Obj.List_Number.Study(Perm_Ind);
            
            % Step 1:
            Perm_Ind = randperm(8);
            Obj.Stimuli.NonCuedS1 = [Lists(Obj.Index_NonCued.L1_P1,1);Lists(Obj.Index_NonCued.L2_P1,2)];
            Obj.List_Number.NonCuedS1 = [ones(4,1)*List_Index(1);ones(4,1)*List_Index(2)];
            Obj.Index.IsOldNonCued.S1 = [true(4,1);false(4,1)];
            Obj.Stimuli.NonCuedS1 = Obj.Stimuli.NonCuedS1(Perm_Ind);
            Obj.Index.IsOldNonCued.S1 = Obj.Index.IsOldNonCued.S1(Perm_Ind);
            Obj.List_Number.NonCuedS1 = Obj.List_Number.NonCuedS1(Perm_Ind);
            
            Perm_Ind = randperm(24);
            Obj.Stimuli.CuedS1 = [Lists(Obj.Index_Valid.L1_P1,1);Lists(Obj.Index_Invalid.L1_P1,1);...
                                  Lists(Obj.Index_Valid.L2_P1,2);Lists(Obj.Index_Invalid.L2_P1,2)];
            Obj.List_Number.CuedS1 = [ones(12,1)*List_Index(1);ones(12,1)*List_Index(2)];
            Obj.Index.IsOldCued.S1 = [true(9,1);true(3,1);...
                                      false(9,1);false(3,1)];
            Obj.Index.IsValid.S1   = [true(9,1);false(3,1);...
                                      true(9,1);false(3,1)];
                                  
            Obj.Stimuli.CuedS1 = Obj.Stimuli.CuedS1(Perm_Ind);
            Obj.Index.IsOldCued.S1 = Obj.Index.IsOldCued.S1(Perm_Ind);
            Obj.Index.IsValid.S1 = Obj.Index.IsValid.S1(Perm_Ind);
            Obj.List_Number.CuedS1 = Obj.List_Number.CuedS1(Perm_Ind);
            
            % Step 2:
            Perm_Ind = randperm(8);
            Obj.Stimuli.NonCuedS2 = [Lists(Obj.Index_NonCued.L1_P2,1);Lists(Obj.Index_NonCued.L2_P2,2)];
            Obj.List_Number.NonCuedS2 = [ones(4,1)*List_Index(1);ones(4,1)*List_Index(2)];
            Obj.Index.IsOldNonCued.S2 = [true(4,1);false(4,1)];
            Obj.Stimuli.NonCuedS2 = Obj.Stimuli.NonCuedS2(Perm_Ind);
            Obj.Index.IsOldNonCued.S2 = Obj.Index.IsOldNonCued.S2(Perm_Ind);
            Obj.List_Number.NonCuedS2 = Obj.List_Number.NonCuedS2(Perm_Ind);
            
            Perm_Ind = randperm(24);
            Obj.Stimuli.CuedS2 = [Lists(Obj.Index_Valid.L1_P2,1);Lists(Obj.Index_Invalid.L1_P2,1);...
                                  Lists(Obj.Index_Valid.L2_P2,2);Lists(Obj.Index_Invalid.L2_P2,2)];
            Obj.List_Number.CuedS2 = [ones(12,1)*List_Index(1);ones(12,1)*List_Index(2)];
            Obj.Index.IsOldCued.S2 = [true(9,1);true(3,1);...
                                      false(9,1);false(3,1)];
            Obj.Index.IsValid.S2   = [true(9,1);false(3,1);...
                                      true(9,1);false(3,1)];
                                  
            Obj.Stimuli.CuedS2 = Obj.Stimuli.CuedS2(Perm_Ind);
            Obj.Index.IsOldCued.S2 = Obj.Index.IsOldCued.S2(Perm_Ind);
            Obj.Index.IsValid.S2 = Obj.Index.IsValid.S2(Perm_Ind);
            Obj.List_Number.CuedS2 = Obj.List_Number.CuedS2(Perm_Ind);
            
            % Step 3:
            Perm_Ind = randperm(8);
            Obj.Stimuli.NonCuedS3 = [Lists(Obj.Index_NonCued.L3_P1,3);Lists(Obj.Index_NonCued.L4_P1,4)];
            Obj.List_Number.NonCuedS3 = [ones(4,1)*List_Index(3);ones(4,1)*List_Index(4)];
            Obj.Index.IsOldNonCued.S3 = [true(4,1);false(4,1)];
            Obj.Stimuli.NonCuedS3 = Obj.Stimuli.NonCuedS3(Perm_Ind);
            Obj.Index.IsOldNonCued.S3 = Obj.Index.IsOldNonCued.S3(Perm_Ind);
            Obj.List_Number.NonCuedS3 = Obj.List_Number.NonCuedS3(Perm_Ind);
            
            Perm_Ind = randperm(24);
            Obj.Stimuli.CuedS3 = [Lists(Obj.Index_Valid.L3_P1,3);Lists(Obj.Index_Invalid.L3_P1,3);...
                                  Lists(Obj.Index_Valid.L4_P1,4);Lists(Obj.Index_Invalid.L4_P1,4)];
            Obj.List_Number.CuedS3 = [ones(12,1)*List_Index(3);ones(12,1)*List_Index(4)];
            Obj.Index.IsOldCued.S3 = [true(9,1);true(3,1);...
                                      false(9,1);false(3,1)];
            Obj.Index.IsValid.S3   = [true(9,1);false(3,1);...
                                      true(9,1);false(3,1)];
                                  
            Obj.Stimuli.CuedS3 = Obj.Stimuli.CuedS3(Perm_Ind);
            Obj.Index.IsOldCued.S3 = Obj.Index.IsOldCued.S3(Perm_Ind);
            Obj.Index.IsValid.S3 = Obj.Index.IsValid.S3(Perm_Ind);
            Obj.List_Number.CuedS3 = Obj.List_Number.CuedS3(Perm_Ind);
            
            % Step 4:
            Perm_Ind = randperm(8);
            Obj.Stimuli.NonCuedS4 = [Lists(Obj.Index_NonCued.L3_P2,3);Lists(Obj.Index_NonCued.L4_P2,4)];
            Obj.List_Number.NonCuedS4 = [ones(4,1)*List_Index(3);ones(4,1)*List_Index(4)];
            Obj.Index.IsOldNonCued.S4 = [true(4,1);false(4,1)];
            Obj.Stimuli.NonCuedS4 = Obj.Stimuli.NonCuedS4(Perm_Ind);
            Obj.Index.IsOldNonCued.S4 = Obj.Index.IsOldNonCued.S4(Perm_Ind);
            Obj.List_Number.NonCuedS4 = Obj.List_Number.NonCuedS4(Perm_Ind);
            
            Perm_Ind = randperm(24);
            Obj.Stimuli.CuedS4 = [Lists(Obj.Index_Valid.L3_P2,3);Lists(Obj.Index_Invalid.L3_P2,3);...
                                  Lists(Obj.Index_Valid.L4_P2,4);Lists(Obj.Index_Invalid.L4_P2,4)];
            Obj.List_Number.CuedS4 = [ones(12,1)*List_Index(3);ones(12,1)*List_Index(4)];
            Obj.Index.IsOldCued.S4 = [true(9,1);true(3,1);...
                                      false(9,1);false(3,1)];
            Obj.Index.IsValid.S4   = [true(9,1);false(3,1);...
                                      true(9,1);false(3,1)];
                                  
            Obj.Stimuli.CuedS4 = Obj.Stimuli.CuedS4(Perm_Ind);
            Obj.Index.IsOldCued.S4 = Obj.Index.IsOldCued.S4(Perm_Ind);
            Obj.Index.IsValid.S4 = Obj.Index.IsValid.S4(Perm_Ind);
            Obj.List_Number.CuedS4 = Obj.List_Number.CuedS4(Perm_Ind);
            
            
            
            Codes = Obj.Codes;
            Obj.Encoding_Practice_items = {'watch','tree','wheel','whistle','zebra',...            
                                           'turtle','glasses','cake','cheese'}';
        
            Obj.Testing_Practice_Training_items = {'watch','wheel','turkey',...
                                                   'tree','watermelon'}';
            Obj.Testing_Practice_Training_isold = [Codes.Old, Codes.Old, Codes.New,...
                                                   Codes.Old, Codes.New]';
            Obj.Testing_Practice_Trials_items = {'whistle','stove','glasses','zebra',...
                                                 'wagon','truck','cheese','flower',...
                                                 'turtle','windmill','elephant','cake'}';
            Obj.Testing_Practice_Trials_isold =  [Codes.Old, Codes.New, Codes.Old, Codes.Old,...
                                                  Codes.New, Codes.New, Codes.Old, Codes.New,...
                                                  Codes.Old, Codes.New, Codes.New, Codes.Old]'; % Old: True
            Obj.Testing_Practice_Trials_iscued = [true,true,true,true,...
                                                  true,true,true,true,...
                                                  false,false,false,false]'; % Cued: True
            Obj.Testing_Practice_Trials_isvalid =  [true,true,true,false,...
                                                    true,false,true,true,...
                                                    false,false,false,false]'; % Valid: True
        end
        %% Fill data with stimuli names and types
        function Obj = FillData(Obj)
            
            Encoding_Practice = 9; % items
            Encoding_Study = 64; % items
            Testing_PracticeTraining = 5; % items
            Testing_PracticeTrials = 12; % items
            Testing_Test1_Cued1 = 24; % items
            Testing_Test1_Noncued1 = 8; % items
            Testing_Test1_Cued2 = 24; % items
            Testing_Test1_Noncued2 = 8; % items
%             Testing_Test1 = Testing_Test1_Cued1 +...
%                             Testing_Test1_Noncued1 +...
%                             Testing_Test1_Cued2 +...
%                             Testing_Test1_Noncued2;
            
            Testing_Test2_Cued1 = 24; % items
            Testing_Test2_Noncued1 = 8; % items
            Testing_Test2_Cued2 = 24; % items
            Testing_Test2_Noncued2 = 8; % items
%             Testing_Test2 = Testing_Test2_Cued1 +...
%                             Testing_Test2_Noncued1 +...
%                             Testing_Test2_Cued2 +...
%                             Testing_Test2_Noncued2;
            Cue_Assessment1 = 1; % items
            Cue_Assessment2 = 1; % items
			Cue_Assessment3 = 1;
            
            Block_order = Obj.Data.block_order(1);
            
            Encoding_Num = [Encoding_Practice,Encoding_Study];
            
            if (Block_order == 1)
            Nums = [Encoding_Practice,...       % 1
                    Encoding_Study,...          % 2
                    Testing_PracticeTraining,...% 3
                    Cue_Assessment1,...         % 4
                    Testing_PracticeTrials,...  % 5
                    Testing_Test1_Cued1,...     %6  5
                    Testing_Test1_Noncued1,...  %7  5
                    Testing_Test1_Cued2,...     %8  5
                    Testing_Test1_Noncued2,...  %9  5
                    Cue_Assessment2,...         %10  6
                    Testing_Test2_Cued1,...     %11  7 
                    Testing_Test2_Noncued1,...  %12  7
                    Testing_Test2_Cued2,...     %13  7
                    Testing_Test2_Noncued2,...  %14  7
                    Cue_Assessment3];           %15  8

            Obj.Data.phase(1:sum(Encoding_Num)) = 'Encoding_Phase';
            Obj.Data.phase(sum(Encoding_Num)+1:end) = 'Testing_Phase';
            Obj.Data.subphase(1:Nums(1)) = 'Practice';
            Obj.Data.stimuli(1:Nums(1)) = Obj.Encoding_Practice_items;            
            Obj.Data.subphase(1+Nums(1):sum(Nums(1:2))) = 'Study';
            Obj.Data.stimuli(1+Nums(1):sum(Nums(1:2))) = Obj.Stimuli.Study;
            Obj.Data.list_number(1+Nums(1):sum(Nums(1:2))) = Obj.List_Number.Study;
            Obj.Data.subphase(1+sum(Nums(1:2)):sum(Nums(1:3))) = 'Practice_Trainings';
            Obj.Data.stimuli(1+sum(Nums(1:2)):sum(Nums(1:3))) = Obj.Testing_Practice_Training_items;
            Obj.Data.old_new(1+sum(Nums(1:2)):sum(Nums(1:3))) = Obj.Testing_Practice_Training_isold;
            Obj.Data.subphase(1+sum(Nums(1:3)):sum(Nums(1:4))) = 'Cue_Assessment1';
            
            Obj.Data.subphase(1+sum(Nums(1:4)):sum(Nums(1:5))) = 'Practice_Trials';
            Obj.Data.stimuli(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_items;
            Obj.Data.old_new(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_isold;
            Obj.Data.hint(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_iscued;
            Obj.Data.hint_type(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_isvalid;
            Obj.Data.subphase(1+sum(Nums(1:5)):sum(Nums(1:6))) = 'Test1_Cued1';
            Obj.Data.subphase(1+sum(Nums(1:6)):sum(Nums(1:7))) = 'Test1_NonCued1';
            Obj.Data.subphase(1+sum(Nums(1:7)):sum(Nums(1:8))) = 'Test1_Cued2';
            Obj.Data.subphase(1+sum(Nums(1:8)):sum(Nums(1:9))) = 'Test1_NonCued2';
            Obj.Data.stimuli(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.Stimuli.CuedS1;...
                                                                 Obj.Stimuli.NonCuedS1;...
                                                                 Obj.Stimuli.CuedS2;...
                                                                 Obj.Stimuli.NonCuedS2];
            Obj.Data.list_number(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.List_Number.CuedS1;...
                                                                     Obj.List_Number.NonCuedS1;...
                                                                     Obj.List_Number.CuedS2;...
                                                                     Obj.List_Number.NonCuedS2];
            Obj.Data.old_new(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.Index.IsOldCued.S1;...
                                                                Obj.Index.IsOldNonCued.S1;...
                                                                Obj.Index.IsOldCued.S2;...
                                                                Obj.Index.IsOldNonCued.S2];
            Obj.Data.hint(1+sum(Nums(1:5)):sum(Nums(1:9))) = [true(24,1);...
                                                                false(8,1);...
                                                                true(24,1);...
                                                                false(8,1)];
            Obj.Data.hint_type(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.Index.IsValid.S1;...
                                                                   false(8,1);...
                                                                   Obj.Index.IsValid.S2;...
                                                                   false(8,1)];
                                                            
            Obj.Data.subphase(1+sum(Nums(1:9)):sum(Nums(1:10))) = 'Cue_Assessment2';
            Obj.Data.subphase(1+sum(Nums(1:10)):sum(Nums(1:11))) = 'Test2_Cued1';
            Obj.Data.subphase(1+sum(Nums(1:11)):sum(Nums(1:12))) = 'Test2_NonCued1';
            Obj.Data.subphase(1+sum(Nums(1:12)):sum(Nums(1:13))) = 'Test2_Cued2';
            Obj.Data.subphase(1+sum(Nums(1:13)):sum(Nums(1:14))) = 'Test2_NonCued2';
            Obj.Data.stimuli(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.Stimuli.CuedS3;...
                                                                 Obj.Stimuli.NonCuedS3;...
                                                                 Obj.Stimuli.CuedS4;...
                                                                 Obj.Stimuli.NonCuedS4];
            Obj.Data.list_number(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.List_Number.CuedS3;...
                                                                     Obj.List_Number.NonCuedS3;...
                                                                     Obj.List_Number.CuedS4;...
                                                                     Obj.List_Number.NonCuedS4];
            Obj.Data.old_new(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.Index.IsOldCued.S3;...
                                                                Obj.Index.IsOldNonCued.S3;...
                                                                Obj.Index.IsOldCued.S4;...
                                                                Obj.Index.IsOldNonCued.S4];
            Obj.Data.hint(1+sum(Nums(1:10)):sum(Nums(1:14))) = [true(24,1);...
                                                              false(8,1);...
                                                              true(24,1);...
                                                              false(8,1)];
            Obj.Data.hint_type(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.Index.IsValid.S3;...
                                                                   false(8,1);...
                                                                   Obj.Index.IsValid.S4;...
                                                                   false(8,1)];
            Obj.Data.subphase(1+sum(Nums(1:14)):sum(Nums(1:15))) = 'Cue_Assessment3';
            else
                
            Nums = [Encoding_Practice,...       % 1
                    Encoding_Study,...          % 2
                    Testing_PracticeTraining,...% 3
                    Cue_Assessment1,...         % 4
                    Testing_PracticeTrials,...  % 5
                    Testing_Test1_Noncued1,...  %6  5
                    Testing_Test1_Cued1,...     %7  5
                    Testing_Test1_Noncued2,...  %8  5
                    Testing_Test1_Cued2,...     %9  5
                    Cue_Assessment2,...         %10  6 
                    Testing_Test2_Noncued1,...  %11  7
                    Testing_Test2_Cued1,...     %12  7
                    Testing_Test2_Noncued2,...  %13  7
                    Testing_Test2_Cued2,...     %14  7
                    Cue_Assessment3];           %15  8

            Obj.Data.phase(1:sum(Encoding_Num)) = 'Encoding_Phase';
            Obj.Data.phase(sum(Encoding_Num)+1:end) = 'Testing_Phase';
            Obj.Data.subphase(1:Nums(1)) = 'Practice';
            Obj.Data.stimuli(1:Nums(1)) = Obj.Encoding_Practice_items;            
            Obj.Data.subphase(1+Nums(1):sum(Nums(1:2))) = 'Study';
            Obj.Data.stimuli(1+Nums(1):sum(Nums(1:2))) = Obj.Stimuli.Study;
            Obj.Data.list_number(1+Nums(1):sum(Nums(1:2))) = Obj.List_Number.Study;
            Obj.Data.subphase(1+sum(Nums(1:2)):sum(Nums(1:3))) = 'Practice_Trainings';
            Obj.Data.stimuli(1+sum(Nums(1:2)):sum(Nums(1:3))) = Obj.Testing_Practice_Training_items;
            Obj.Data.old_new(1+sum(Nums(1:2)):sum(Nums(1:3))) = Obj.Testing_Practice_Training_isold;
            Obj.Data.subphase(1+sum(Nums(1:3)):sum(Nums(1:4))) = 'Cue_Assessment1';
            
            Obj.Data.subphase(1+sum(Nums(1:4)):sum(Nums(1:5))) = 'Practice_Trials';
            Obj.Data.stimuli(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_items;
            Obj.Data.old_new(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_isold;
            Obj.Data.hint(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_iscued;
            Obj.Data.hint_type(1+sum(Nums(1:4)):sum(Nums(1:5))) = Obj.Testing_Practice_Trials_isvalid;
            Obj.Data.subphase(1+sum(Nums(1:5)):sum(Nums(1:6))) = 'Test1_NonCued1';
            Obj.Data.subphase(1+sum(Nums(1:6)):sum(Nums(1:7))) = 'Test1_Cued1';
            Obj.Data.subphase(1+sum(Nums(1:7)):sum(Nums(1:8))) = 'Test1_NonCued2';
            Obj.Data.subphase(1+sum(Nums(1:8)):sum(Nums(1:9))) = 'Test1_Cued2';
            Obj.Data.stimuli(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.Stimuli.NonCuedS1;...
                                                                 Obj.Stimuli.CuedS1;...
                                                                 Obj.Stimuli.NonCuedS2;...
                                                                 Obj.Stimuli.CuedS2];
            Obj.Data.list_number(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.List_Number.NonCuedS1;...
                                                                     Obj.List_Number.CuedS1;...
                                                                     Obj.List_Number.NonCuedS2;...
                                                                     Obj.List_Number.CuedS2];
            Obj.Data.old_new(1+sum(Nums(1:5)):sum(Nums(1:9))) = [Obj.Index.IsOldNonCued.S1;...
                                                                 Obj.Index.IsOldCued.S1;...
                                                                 Obj.Index.IsOldNonCued.S2;...
                                                                 Obj.Index.IsOldCued.S2];
            Obj.Data.hint(1+sum(Nums(1:5)):sum(Nums(1:9))) = [false(8,1);...
                                                              true(24,1);...
                                                              false(8,1);...
                                                              true(24,1)];
            Obj.Data.hint_type(1+sum(Nums(1:5)):sum(Nums(1:9))) = [false(8,1);...
                                                                   Obj.Index.IsValid.S1;...
                                                                   false(8,1);...
                                                                   Obj.Index.IsValid.S2];
                                                            
            Obj.Data.subphase(1+sum(Nums(1:9)):sum(Nums(1:10))) = 'Cue_Assessment2';
            Obj.Data.subphase(1+sum(Nums(1:10)):sum(Nums(1:11))) = 'Test2_NonCued1';
            Obj.Data.subphase(1+sum(Nums(1:11)):sum(Nums(1:12))) = 'Test2_Cued1';
            Obj.Data.subphase(1+sum(Nums(1:12)):sum(Nums(1:13))) = 'Test2_NonCued2';
            Obj.Data.subphase(1+sum(Nums(1:13)):sum(Nums(1:14))) = 'Test2_Cued2';
            Obj.Data.stimuli(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.Stimuli.NonCuedS3;...
                                                                   Obj.Stimuli.CuedS3;...
                                                                   Obj.Stimuli.NonCuedS4;...
                                                                   Obj.Stimuli.CuedS4];
            Obj.Data.list_number(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.List_Number.NonCuedS3;...
                                                                       Obj.List_Number.CuedS3;...
                                                                       Obj.List_Number.NonCuedS4;...
                                                                       Obj.List_Number.CuedS4];
            Obj.Data.old_new(1+sum(Nums(1:10)):sum(Nums(1:14))) = [Obj.Index.IsOldNonCued.S3;...
                                                                   Obj.Index.IsOldCued.S3;...
                                                                   Obj.Index.IsOldNonCued.S4;...
                                                                   Obj.Index.IsOldCued.S4];
            Obj.Data.hint(1+sum(Nums(1:10)):sum(Nums(1:14))) = [false(8,1);...
                                                                true(24,1);...
                                                                false(8,1);...
                                                                true(24,1)];
            Obj.Data.hint_type(1+sum(Nums(1:10)):sum(Nums(1:14))) = [false(8,1);...
                                                                     Obj.Index.IsValid.S3;...
                                                                     false(8,1);...
                                                                     Obj.Index.IsValid.S4];
            Obj.Data.subphase(1+sum(Nums(1:14)):sum(Nums(1:15))) = 'Cue_Assessment3';
            end
        end                
        
        %% Save Data Table
        function SaveData(Data,Step,FileCheck) %#ok<INUSL>
            if(~exist('FileCheck','var'))
                FileCheck=0;
            end
            SData = struct(Data); %#ok<NASGU>
            FileFlag = exist([Data.Path.Output,'Participant_',num2str(Data.Participant_Code),'.mat'],'file');
            Overwrite='Yes';
            if(FileFlag==2 && FileCheck==1)
                Overwrite = questdlg('This Participant''s file already exists. Do you want to overwrite?', ...
                         'Warning', ...
                         'Yes','No','No');
            end
            if(strcmp(Overwrite,'Yes'))
                save([Data.Path.Output,'Participant_',num2str(Data.Participant_Code),'.mat'],'Data','Step');
                save([Data.Path.Output,'Participant_',num2str(Data.Participant_Code),'_Struct.mat'],'SData','Step');
                writetable(Data.Data,...
                           [Data.Path.Output,'Participant_',num2str(Data.Participant_Code),'.csv'],...
                           'QuoteStrings',true);
            else
                error('(HONEYBEE)-> Program is terminated by user ')
            end
        end
        %% Quick Save Table
        function QuickSave(Data,Step) %#ok<INUSD>            
            save([Data.Path.Output,'QSave_Participant_',num2str(Data.Participant_Code),'.mat'],'Data','Step');
%             save([Obj.Path.Output,'QSave_Participant_',num2str(Obj.Participant_Code),'.mat'],'Obj','Step');
%             save([Obj.Path.Output,'Participant_',num2str(Obj.Participant_Code),'_Workspace.mat']);
        end
    end
end

