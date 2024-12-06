function [Pr_resp_4cat,Pr_resp_2cat] = GCM_CSA(param)

%% set parameters
% free parameters
c = param.sensitivity;
gamma = param.response_scaling;
w_color = param.weight_color;
w_vertline_red = param.weight_vertline_red;
w_height_blue = param.weight_height_blue;
b = param.bias_A; %bias term for category A/C

% experimental parameters
n_item = 32;
n_dim = 3;
n_trainblock = 10;


%% construct stimuli
item_dim = NaN(n_item,n_dim);
item_cat2 = NaN(1,n_item);
item_cat4 = NaN(1,n_item);
for i = 1:n_item
    i_vertline = mod(i,4);%vertline
    i_height = mod(floor((i-1)/4) + 1,4);%height
    i_color = floor((i-1)/16) + 1;%color; set color spacing to be 3
    if i_vertline == 0
        i_vertline = 4;
    end
    if i_height == 0
        i_height = 4;
    end
    if i_color == 2
        i_color = 4;
    end

    if i_color == 1 && i_vertline <= 2
        item_cat2(i) = 1;
        item_cat4(i) = 1;
    elseif i_color == 1 && i_vertline >= 3
        item_cat2(i) = 2;
        item_cat4(i) = 2;
    elseif i_color == 4 && i_height <= 2
        item_cat2(i) = 1;
        item_cat4(i) = 3;
    else
        item_cat2(i) = 2;
        item_cat4(i) = 4;
    end
    item_dim(i,1) = i_vertline;
    item_dim(i,2) = i_height;
    item_dim(i,3) = i_color;
end

train_index = [2,6,9,3,7,12,15,19,21,22,25,26,28,31];
crit_index = [14,24];



%% compute similarity matrix
item_sim_cat2 = NaN(4,n_item);
item_sim_cat4 = NaN(4,n_item);
for ipat = 1:n_item
    sum_sim_cat2 = zeros(1,4);
    sum_sim_cat4 = zeros(1,4);
    for jpat = train_index
        cat2_idx = item_cat2(jpat);
        cat4_idx = item_cat4(jpat);
        if item_dim(ipat,3) == 1
            w = [w_vertline_red, 1-w_color-w_vertline_red, w_color];
        elseif item_dim(ipat,3) == 4
            w = [1-w_color-w_height_blue, w_height_blue, w_color];
        end
        dist = sum(w.*abs(item_dim(ipat,:) - item_dim(jpat,:)));
        sim = exp(-c*dist);
        sum_sim_cat2(cat2_idx) = sum_sim_cat2(cat2_idx) + sim;
        sum_sim_cat4(cat4_idx) = sum_sim_cat4(cat4_idx) + sim;
    end
    item_sim_cat2(:,ipat) = sum_sim_cat2*n_trainblock.*[b 1-b 0 0];
    item_sim_cat4(:,ipat) = sum_sim_cat4*n_trainblock.*[.5*b .5*(1-b) .5*b .5*(1-b)];
    item_sim_cat2(:,ipat) = sum_sim_cat2*n_trainblock;
    item_sim_cat4(:,ipat) = sum_sim_cat4*n_trainblock;
end

%% compute category response probabilities
Pr_resp_2cat = bsxfun(@rdivide,item_sim_cat2.^gamma,sum(item_sim_cat2.^gamma));
Pr_resp_4cat = bsxfun(@rdivide,item_sim_cat4.^gamma,sum(item_sim_cat4.^gamma));










