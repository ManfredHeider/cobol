      *****************************************************************
      *    COPY-MEMBER: DRVB1343                                     
      *                                                              
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0005                    
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1343
      *                                                              
      *                                                              
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DAISY-4437 !27.04.22! NEUERSTELLUNG                ! U131764  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                                  
       01 PI-DRVB1343.
          05 PI-DRVB1343-UFKT-CODE          PIC  9(002).     
          05 PI-DRVB1343-ZEITPUNKT          PIC  X(010). 
      *                                                                  
       01 PO-DRVB1343.
                                                                       
          05 PO-DRVB1343-RC                 PIC  X(003).                  
             88 PO-DRVB1343-RC-OK                        VALUE 'OK '.   
             88 PO-DRVB1343-RC-LEER                      VALUE 'LER'.   
             88 PO-DRVB1343-RC-EOF                       VALUE 'EOF'.   
             88 PO-DRVB1343-RC-NOT-FOUND                 VALUE 'NFD'.   
                                                                       
          05 PO-DRVB1343-DATEN.                                           
             10 PO-DRVB1343-RFT-ST-NR       PIC S9(009)  BINARY.   
             10 PO-DRVB1343-MA-ST-NR        PIC S9(009)  BINARY. 
             10 PO-DRVB1343-RFT-DAT-VON     PIC X(010).                 
             10 PO-DRVB1343-RFT-DAT-BIS     PIC X(010).                      

      *****************************************************************
      *---------------------- ENDE DRVB1343 --------------------------* 
      *****************************************************************
