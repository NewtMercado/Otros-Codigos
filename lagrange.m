function L = lagrange(xpuntos,k,x)
% Argumentos:
% xpuntos: vector fila con las coordenadas en x
% k: valor entero correspondiente Lk
% x: valor real en el que se eval´ua el polinomio
% Salida:
% L: valor obtenido al evaluar el polinomio de
% Lagrange en x
L = 1.0;
n = length(xpuntos);
for i=1:k
L = L * (x - xpuntos(i))/(xpuntos(k+1)-xpuntos(i));
end
for i=k+2:n
L = L * (x - xpuntos(i))/(xpuntos(k+1)-xpuntos(i));
end
end