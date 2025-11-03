function [A]=difdiv(s,f)
n=length(s);
A=zeros(n, n);
A(1:n,1)=f;
for J=2:n
 J1=J-1;
 for K=1:(n-J1)
 A(K,J)=(A(K+1,J1)-A(K,J1))/(s(K+J1)-s(K));
 end
end
end