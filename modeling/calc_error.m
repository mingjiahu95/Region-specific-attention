
function [LL,Pr_resp_profile] = calc_error(observed_data, x)
epsilon = .000001;
Nresp_2cat_lower = observed_data;

param = struct; 

param.sensitivity = x(1);
param.response_scaling = x(2);
param.weight_color = x(3);
% param.weight_vertline = min(x(4),1-x(3));
param.weight_vertline_red = min(x(4),1-x(3));
param.weight_height_blue = min(x(5),1-x(3));
param.bias_A = x(6);

[~,Pr_resp_2cat] = GCM_CSA(param);
Pr_resp_profile = Pr_resp_2cat; %bsxfun(@times,Pr_resp_2cat,sum(Nresp_4cat_reduced_close))
Pr_resp_2cat(Pr_resp_2cat == 0) = epsilon;

%MLE 
LL = -sum(Nresp_2cat_lower(:).*log(Pr_resp_2cat(:)));
%-∑f_i ln(p_i) 
%-2∑f_i ln(p_i)   +  kln(N)

end
