C      ######################################################################
C      #################      CAE Assistant Company          ################
C      ##############         CAEassistant-com              #############
C      ###########   Copy right by CAE Assistant Company    ###############
C      ######################################################################

C      ######################################################################
C      ######################################################################
C      CAE Assisitant Services: 
C      Toturial Packages,Consultancy,Articles,Q&A,Video Gallery,Online Course
C      ######################################################################
C      Need help with your project? 
C      You can get initial free consultation from (caeassistant-com)
C      ###################################################################### 	
      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4 JSTEP(4)
      REAL*8 E1,E2,V12,V21,G12,D11,D22,D66,CFULL(3,3),Y,X,Z,R,F11,F22,
     1F12,F66,F1,F2,F6,XT,XC,YT,YC,SU12
      
     
C=================================================================================
C                                 GIVE PROPS
C=================================================================================

      E1=PROPS(1)
      E2=PROPS(2)
      V12=PROPS(3)
      G12=PROPS(4)
      XT=PROPS(5)
      XC=PROPS(6)
      YT=PROPS(7)
      YC=PROPS(8)
      SU12=PROPS(9)
      PRINT*,' E2', E2
      V21=(E2/E1)*V12 
      R=1.0/(1.0-(V12*V21))
      D11=E1*R
      D22=E2*R
      D66=G12
      D12=E2*V12*R
C=================================================================================
C                     FILL DEGRADED STIFNESS MATRIX
C=================================================================================       

      DO K1=1,3
        DO K2=1,3
            DDSDDE(K2,K1)=0.0
        END DO
      END DO
      DDSDDE(1,1)=D11
      DDSDDE(1,2)=D12
      DDSDDE(2,2)=D22   
      DDSDDE(1,3)=0.0  
      DDSDDE(2,3)=0.0
      DDSDDE(3,3)=D66
      DDSDDE(2,1)=D12
      DDSDDE(3,1)=0.0
      DDSDDE(3,2)=0.0

C=================================================================================
C                             CALCULATE STRESS
C================================================================================= 
         DO I=1,3
          Z=0.0
           DO J=1,3
            !Hidden Content: You can get the complete code from our website for free. CAE Assistant com
         END DO     
      
C=================================================================================
C                          TSAI-HILL CRITERIA
C=================================================================================    
      
        IF (STRESS(1).GE. 0.0)THEN
            X=XT
        ELSE
            X=XC
        END IF
        IF (STRESS(2).GE. 0.0)THEN
            Y=YT
        ELSE
            Y=YC
        END IF

        F11=1.0/(X**2.0)
        F1=0.0
        F12=-1.0/(2.0*(X**2.0))
        F22=1.0/(Y**2.0)
        F2=0.0  
        F66=1.0/(SU12**2.0)
        F6=0.0
C       FAILURE SURFACE
        !Hidden Content: You can get the complete code from our website for free. CAE Assistant com                 

      RETURN
      END
