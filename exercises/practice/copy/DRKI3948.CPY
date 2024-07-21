      *****************************************************************
      *    COPY-MEMBER: DRKI3948                                      * 
      *                                                               *
      *    CURSOR FULL SELECT ZXKI0558 G48_PRIVATKREDIT                 
      *                                                              
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          
      *    FÜR DIE FUNKTION DRKI3948                                  
      *    AUSSER: DEM FUNKTIONSNAMEN,                                
      *            DEM ERR-ERROR-BEREICH UND                          
      *            DEM STANDARD-STATUS-BEREICH                        
      *            (JEWEILS SEPARATE COPYS)                           
      *                                                               
      *    FUNKTION: LIST          
      *                                                               *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-8788   !05.05.22! Neuerstellung                ! u131774  * 
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  PI-DRKI3948.                                                 
           05 PI-DRKI3948-UFKT-CODE             PIC  9(0002).           
             88 PI-DRKI3948-UFKT-FIRST              VALUE 1.            
             88 PI-DRKI3948-UFKT-NEXT               VALUE 2.            
             88 PI-DRKI3948-UFKT-QUIT               VALUE 3.            
                                                               
           05 PI-DRKI3948-KEY.                                          
             10 PI-DRKI3948-G48MANDANT          PIC  S9(009)    COMP.   
      
       01  PO-DRKI3948.         
           05 PO-DRKI3948-RC                    PIC  X(0003).           
             88 PO-DRKI3948-RC-OK              VALUE 'OK '.             
             88 PO-DRKI3948-RC-LEER            VALUE 'LER'.             
             88 PO-DRKI3948-RC-EOF             VALUE 'EOF'.    
       
           05 PO-DRKI3882-KEY.
             10 PO-DRKI3948-G48PKVNR            PIC  X(0015).           
             10 PO-DRKI3948-G48PKUKTONR         PIC  X(0002). 
             10 PO-DRKI3948-G48PKWAEHRUNG       PIC  X(0003).
                                                       
          05  PO-DRKI3948-DATEN.                      
             10 PO-DRKI3948-G48PKANNUITAET      PIC S9(12)V9(2) COMP-3. 
             10 PO-DRKI3948-G48PKBICBELASTKTO   PIC  X(0011).
             10 PO-DRKI3948-G48PKDARLBTRG       PIC S9(12)V9(2) COMP-3. 
             10 PO-DRKI3948-G48PKDELKZN         PIC  X(0001).
             10 PO-DRKI3948-G48PKFAELLIGDAT     PIC  X(0008).
             10 PO-DRKI3948-G48PKGSMLAUFZEIT    PIC S9(9)       COMP.   
             10 PO-DRKI3948-G48PKIBANBELASTKTO  PIC  X(0034).
             10 PO-DRKI3948-G48PKIBANPK         PIC  X(0034).
             10 PO-DRKI3948-G48PKKREDITART      PIC  X(0006).
             10 PO-DRKI3948-G48PKKREDITAUSZDAT  PIC  X(0008).
             10 PO-DRKI3948-G48PKKREDITZUSDAT   PIC  X(0008).
             10 PO-DRKI3948-G48PKMANDREF        PIC  X(0035).
             10 PO-DRKI3948-G48PKNAMEBELASTKTO  PIC  X(0028).
             10 PO-DRKI3948-G48PKOEFFDAT        PIC  X(0008).
             10 PO-DRKI3948-G48PKPIN            PIC S9(9)       COMP.   
             10 PO-DRKI3948-G48PKRESTLAUFZEIT   PIC S9(9)       COMP.   
             10 PO-DRKI3948-G48PKSALDO          PIC S9(12)V9(2) COMP-3. 
             10 PO-DRKI3948-G48PKSCHLDAT        PIC  X(0008).
             10 PO-DRKI3948-G48PKSOLLZINS       PIC S9(2)V9(4)  COMP-3. 
             10 PO-DRKI3948-G48PKVERSUMFANGKN1  PIC  X(1).  
             10 PO-DRKI3948-G48PKVERSUMFANGKN2  PIC  X(1).  
             10 PO-DRKI3948-LETZTERAENDERER     PIC  X(0010).
      *****************************************************************
      *---------------------- ENDE DRKI3948 --------------------------* 
      *****************************************************************
