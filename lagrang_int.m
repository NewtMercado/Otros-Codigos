function [l,L]=lagrang_int(x,y)
% Calcula el polinomio de interpolación de Lagrange
% x e y vectores de datos; l, coeficientes polinomio resultante,
% L polinomio base
N=length(x)-1; l=0;
for m=1:N+1
P=1;
for k=1:N+1
if k~=m, P=conv(P,poly(x(k)))/(x(m)-x(k)); end
end
L(m,:)=P; % Polinomios de Lagrange base
l=l+y(m)*P; % Coeficientes el polinomio resultante
end