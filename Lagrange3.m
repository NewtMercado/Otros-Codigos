function d=Lagrange3(n,x,y)
d=zeros(n);
d(:,1)=y(:);
for j=2:n
for i=1:n-j+1
d(i,j)=(d(i+1,j-1)-d(i,j-1))/(x(i+j-1)-x(i))
end
end
d