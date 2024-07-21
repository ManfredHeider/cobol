      *****************************************************************
      *    COPY-MEMBER: DRKI3911                                     
      *                                                              
      *    CURSOR FULL SELECT ZXKI0554 G48_BAUFI                             
      *                                                              
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          
      *    FÜR DIE FUNKTION DRKI3911                                  
      *    AUSSER: DEM FUNKTIONSNAMEN,                                
      *            DEM ERR-ERROR-BEREICH UND                          
      *            DEM STANDARD-STATUS-BEREICH                        
      *            (JEWEILS SEPARATE COPYS)                           
      *                                                               
      *    FUNKTION: LIST                                    
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-8787   !11.10.21! Neuerstellung                ! u130685  *
      * -----------!--------!------------------------------!----------*
      * DBO-9707   !08.02.22! NEUES FELD G48BFFAELLIGDAT   ! u130685  *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  PI-DRKI3911.                                                
           05 PI-DRKI3911-UFKT-CODE             PIC  9(0002).          
             88 PI-DRKI3911-UFKT-FIRST              VALUE 1.          
             88 PI-DRKI3911-UFKT-NEXT               VALUE 2.          
             88 PI-DRKI3911-UFKT-QUIT               VALUE 3.          
                                                               
           05 PI-DRKI3911-KEY.                                   
             10 PI-DRKI3911-G48MANDANT          PIC  S9(009) COMP.   
      
       01  PO-DRKI3911.         
           05 PO-DRKI3911-RC                    PIC  X(0003).          
             88 PO-DRKI3911-RC-OK              VALUE 'OK '.           
             88 PO-DRKI3911-RC-LEER            VALUE 'LER'.           
             88 PO-DRKI3911-RC-EOF             VALUE 'EOF'.    
       
           05 PO-DRKI3882-KEY.
             10 PO-DRKI3911-G48BFVNR          PIC  X(0015).             
             10 PO-DRKI3911-G48BFUKTONR       PIC  X(0002). 
             10 PO-DRKI3911-G48BFWAEHRUNG     PIC  X(0003).
                                                       
          05  PO-DRKI3911-DATEN.                      
             10 PO-DRKI3911-G48BFAKTUSALDO    PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3911-G48BFANNUITAET    PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3911-G48BFBIC          PIC  X(0011).
             10 PO-DRKI3911-G48BFDARLEHEN     PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3911-G48BFDELKZ        PIC  X(0001).
             10 PO-DRKI3911-G48BFENDEDAT      PIC  X(0008).
             10 PO-DRKI3911-G48BFENDEZINSDAT  PIC  X(0008).
             10 PO-DRKI3911-G48BFENDEZRFREI   PIC  X(0008).
             10 PO-DRKI3911-G48BFFAELLIGDAT   PIC  X(0008).
             10 PO-DRKI3911-G48BFIBAN         PIC  X(0034).
             10 PO-DRKI3911-G48BFIBANBF       PIC  X(0034).
             10 PO-DRKI3911-G48BFKREDITAUSDAT PIC  X(0008).
             10 PO-DRKI3911-G48BFKREDITZUSDAT PIC  X(0008).
             10 PO-DRKI3911-G48BFMANDREF      PIC  X(0035).
             10 PO-DRKI3911-G48BFNNAMEBZ      PIC  X(0028).
             10 PO-DRKI3911-G48BFOEFDAT       PIC  X(0008).
             10 PO-DRKI3911-G48BFORWORDKZ     PIC  X(0001).
             10 PO-DRKI3911-G48BFPIN          PIC S9(9) COMP.
             10 PO-DRKI3911-G48BFRATEANT      PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3911-G48BFSOLLZINS     PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3911-G48BFTILGUNGBEITR PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3911-G48BFTILGUNGPROD  PIC  X(0010).
             10 PO-DRKI3911-G48BFTILGUNGPROZ  PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3911-G48BFTILGUNGSART  PIC  X(0001).
             10 PO-DRKI3911-G48BFUEBERZINS    PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3911-G48BFVOLLAUSZDAT  PIC  X(0008).
             10 PO-DRKI3911-G48BFZAHLUNGSW    PIC  X(0001).
             10 PO-DRKI3911-G48BFZINSANT      PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3911-G48BFZINSART      PIC  X(0002).
             10 PO-DRKI3911-LETZTERAENDERER   PIC  X(0010).           

      *****************************************************************
      *---------------------- ENDE DRKI3911 --------------------------* 
      *****************************************************************
