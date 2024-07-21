      *****************************************************************
      *    COPY-MEMBER: DRPA1979
      *
      *    PI-PO-BEREICH FUER DEN ZUGRIFF AUF DIE PMPR(H) ZUR
      *    ERMITTLUNG DES SALDOS AN PROVISIONEN EINES
      *    PRODUKTS INNERHALB EINES VERTRAGS ZU EINEM
      *    VORGEGEBENEN STICHTAG
      *
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                       
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME      
      * -------------------------------------------------------------- 
      * DSY04569   !12.08.22! NEU                          ! SCHEIDEL  
      *            !        !                              !           
      *            !        !                              !           
      *                                                                
      * ENDE-LOGBUCH                                                   
      *****************************************************************
       01 PI-DRPA1979.
          05 PI-DRPA1979-DATEN.
             10 PI-DRPA1979-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1979-PRINTE     PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1979-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1979-STICHTAG   PIC  X(010).
       
       01 PO-DRPA1979.
          05 PO-DRPA1979-RC            PIC  X(003).
             88 PO-DRPA1979-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1979-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1979-RC-FAELLIG-N-F               VALUE 'FNF'.
             88 PO-DRPA1979-RC-UNZULAESSIG               VALUE 'UNZ'.
          05 PO-DRPA1979-DATEN.
             10 PO-DRPA1979-SUMME-PROV PIC S9(009)V9(02) BINARY. 
      *****************************************************************
      *---------------------- ENDE DRPA1979 --------------------------* 
      *****************************************************************
