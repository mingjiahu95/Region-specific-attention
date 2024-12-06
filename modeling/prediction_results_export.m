%export category response matrix to text file
[~,Pr_resp_profile] = calc_error(observed_data, param);
writematrix(round(Pr_resp_profile,3),'prediction\resp_2cat_lower.txt','WriteMode','append','Delimiter','tab');
%writematrix(round(Pr_resp_profile,3),'prediction\resp_2cat_xxxxx.txt','Delimiter','tab');