function Lagrange2(x,y,xi);
p=0;
t = sym('t');
n=length(x);
yi=zeros(size(xi));
for i=1:n
z=ones(size(xi));
L=1;
for j=1:n
if i~=j
L=L*(t-x(j))/(x(i)-x(j));
z=z.*(xi-x(j))/(x(i)-x(j));
end
end
yi=yi+z*y(i);
p=p+L*y(i);
p=simplify(p);
end
polinomio=p
yi