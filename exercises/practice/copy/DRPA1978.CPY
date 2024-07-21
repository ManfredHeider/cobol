      *****************************************************************
      *    COPY-MEMBER: DRPA1978
      *
      *    PI-PO-BEREICH FUER DEN ZUGRIFF AUF DIE PMPR(H) ZUR
      *    ERMITTLUNG DES SALDOS AN PROVISIONEN EINES
      *    PRODUKTS INNERHALB EINES VERTRAGS INNERHALB EINES
      *    VORGEGEBENEN ZEITRAUMS
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
       01 PI-DRPA1978.
          05 PI-DRPA1978-DATEN.
             10 PI-DRPA1978-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1978-PRINTE     PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1978-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PI-DRPA1978-STICHTAG-VON
                                       PIC  X(010).
             10 PI-DRPA1978-STICHTAG-BIS
                                       PIC  X(010).
       
       01 PO-DRPA1978.
          05 PO-DRPA1978-RC            PIC  X(003).
             88 PO-DRPA1978-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1978-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1978-RC-FAELLIG-N-F               VALUE 'FNF'.
             88 PO-DRPA1978-RC-UNZULAESSIG               VALUE 'UNZ'.
          05 PO-DRPA1978-DATEN.
             10 PO-DRPA1978-SUMME-PROV PIC S9(009)V9(02) BINARY. 
      *****************************************************************
      *---------------------- ENDE DRPA1978 --------------------------* 
      *****************************************************************
