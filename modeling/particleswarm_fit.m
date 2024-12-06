clear;
clc;


% read in data files
observed_data = readmatrix("empirical\resp_2cat_lower.csv");


init_swarm = [];

fun = @(x) calc_error(observed_data,x); % x is a row vector of parameters
lb = [.001, 1, .001, .001,.001,.001]; %add bound values for bias parameter
ub = [10,1,1,1,1,1];
hybridopts = optimoptions('patternsearch','TolMesh',1e-3);
options = optimoptions('particleswarm','FunctionTolerance',1e-4,'MaxStallIterations',30,...
                       'InitialSwarmMatrix',init_swarm,'SwarmSize',50,...
                       'Display','iter','DisplayInterval',1,...
                       'HybridFcn',{'patternsearch',hybridopts},...
                       'OutputFcn',@show_params,...
                       'UseParallel',true);
nvars = length(lb);
[param,fit,exitflag,output] = particleswarm(fun,nvars,lb,ub,options)  

function stop = show_params(optimValues,state)
stop = false; % This function does not stop the solver
global FinalSwarmMatrix
best_params = optimValues.bestx;
text = {'c','gamma','w_color','w_vertline';
        best_params(1),best_params(2),best_params(3),best_params(4)};

switch state
    case 'init'
        fprintf('Starting parameters:\n') 
        fprintf('%s = %4.3f;\n',text{:})        
    case 'iter'
        fprintf('Best parameters:\n')       
        fprintf('%s = %4.3f;\n',text{:})  
    case 'done' 
        FinalSwarmMatrix = optimValues.swarm;
        fprintf('Final particle positions:\n')
        disp(FinalSwarmMatrix)   
        fprintf('\n')
end
end

                   



