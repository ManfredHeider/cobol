      *****************************************************************
      *    COPY-MEMBER: DRKI3893                                     
      *                                                              
      *    CURSOR FULL SELECT ZXKI0552 G40_GELDKONTO                  *
      *    --------------------------------------------------------   *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3893
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DB=-8784   !22.07.21! NEU                          ! T13EX39  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ****************************************************************
      *
       01  PI-DRKI3893.
           05 PI-DRKI3893-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3893-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3893-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3893-UFKT-QUIT               VALUE 3.
 
           05 PI-DRKI3893-KEY.
              10 PI-DRKI3893-G40MANDANT         PIC  S9(9) COMP.


       01  PO-DRKI3893.
           05 PO-DRKI3893-KEY.
              10 PO-DRKI3893-G40GKOVNR          PIC  X(0015).
 
           05 PO-DRKI3893-RC                    PIC  X(0003).
              88 PO-DRKI3893-RC-OK              VALUE 'OK '.
              88 PO-DRKI3893-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3893-RC-EOF             VALUE 'EOF'.
      *
           05 PO-DRKI3893-DATEN.
             10 PO-DRKI3893-G40GKOBILANZSCHL   PIC  X(0004).                    
             10 PO-DRKI3893-G40GKODATOEFF   PIC  X(0008).                       
             10 PO-DRKI3893-G40GKODELKZ   PIC  X(0001).                         
             10 PO-DRKI3893-G40GKOEXTLIMIT   PIC S9(12)V9(2) COMP-3.            
             10 PO-DRKI3893-G40GKOHABENZINS   PIC S9(2)V9(4) COMP-3.            
             10 PO-DRKI3893-G40GKOKONDSCHL   PIC  X(0002).                      
             10 PO-DRKI3893-G40GKOKUMHABENAM   PIC S9(12)V9(2) COMP-3.          
             10 PO-DRKI3893-G40GKOKUMHABENVM   PIC S9(12)V9(2) COMP-3.          
             10 PO-DRKI3893-G40GKOKUMSOLLAM   PIC S9(12)V9(2) COMP-3.           
             10 PO-DRKI3893-G40GKOKUMSOLLVM   PIC S9(12)V9(2) COMP-3.           
             10 PO-DRKI3893-G40GKOSALDO   PIC S9(12)V9(2) COMP-3.               
             10 PO-DRKI3893-G40GKOSCHLDAT   PIC  X(0008).                       
             10 PO-DRKI3893-G40GKOSOLLZINS   PIC S9(2)V9(4) COMP-3.             
             10 PO-DRKI3893-G40GKOUEBERZINS   PIC S9(2)V9(4) COMP-3.            
             10 PO-DRKI3893-G40GKOZRAM   PIC  X(0005).                          
             10 PO-DRKI3893-G40GKOZRVM   PIC  X(0005).                          
             10 PO-DRKI3893-G40GKVORZEICHEN      PIC X(1).                      
      *****************************************************************
      *---------------------- ENDE DRKI3893 --------------------------* 
      *****************************************************************
