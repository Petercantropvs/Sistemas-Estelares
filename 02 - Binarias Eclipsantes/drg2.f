
      Program delta_Rg 
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
C     Este programa calcula una tabla de valores 
c     (alfa , delta/R_g)
C     segun definiciones Merrill 1950 (1950CoPri..24) p. 6. 
C     k=R_s/R_g se obtiene de la curva de luz.
C     los valores de alfa se leen de tabla 'entrada.dat'
C     Dados alfa y k, se busca la raiz de la funcion g(phi_2,alfa,k)
C     suponiendo 0 < phi_2 < pi.
C     Hallado phi_2, se calculan phi_1 y delta/R_g.
C     dx es la precision deseada en phi_2.
c     se busca raÃ­z de g(x) por biseccion.
c     xl,xr: extremos izq. y der. del intervalo de valores de x
c     donde se busca phi2.
c     xm: punto medio del mismo intervalo
C     gferrero@fcaglp.unlp.edu.ar
c
      Implicit none
      Integer i,j
      Real *8 k,phi1,phi2,g,x,dx,xl,xr,xm,p
      Real *8 alfa(0:1000),drg(0:1000),pi
      Write(*,*)'Ingrese el valor de k'
      Read(*,*)k
      Open(unit=25,file='salida.dat')
      Open(unit=15,file='entrada.dat')
C     se define pi
      pi=4.d0*datan(1.d0)
c     se titula archivo salida.dat
      Write(25,*)"#alfa     delta/R_g"
      dx=0.00001
C     se lee valor de alfa en fila i de entrada.dat
      Do i=0,1000
         Read(15,*,End=90,Err=80)alfa(i)
c        se busca phi2 para ese alfa por biseccion
c        se aprovecha que dg/dphi2 > 0
         xl=0.d0
         xr=pi
10       if((abs(xl-xr)).ge.dx) then
           xm=(xl+xr)/2
           x=xm
           p=g(x,k,alfa(i))
           if (p.lt.0.) then
                             xl=xm
                        else
                             xr=xm
           endif
         goto 10
20       endif
         phi2=x
         phi1=asin(k*sin(phi2))
         drg(i)=cos(phi1)+k*cos(phi2) 
C       se guarda el valor de alfa y su correspondiente delta/R_g
        Write(25,100)alfa(i),drg(i)
        j=i
        Enddo
80    Stop "Error en archivo de entrada"
90    Continue
95    Write(*,*)"datos de alfa leidos: ",j
100   Format(1x,f7.3,3x,f7.4)
      Close(25)
      End

      function g(phi2,k,alfa) result(y)
      Real *8 phi2,k,alfa,pi 
      Real *8 y    
      pi=4.d0*datan(1.d0)
      y=phi2*k**2+asin(k*sin(phi2))-k*sin(phi2)*k*cos(phi2)-k*sin(phi2)*
     xsqrt(1.d0-(k*sin(phi2))**2)-alfa*pi*k**2
      end function g 
