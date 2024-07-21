      *****************************************************************
      *    COPY-MEMBER: DRPA1977                                      *
      *                                                               *
      *    PI-PO-BEREICH FUER DEN ZUGRIFF AUF DIE PMPR ZUR            *
      *    ERMITTLUNG DES HOECHSTEN VERTRAGSTEILS ZU EINEM PRODUKT    *
      *    INNERHALB EINES VERTARGS                                   *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY04569   !11.08.22! NEU                          ! SCHEIDEL *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01 PI-DRPA1977.
          05 PI-DRPA1977-DATEN.
             10 PI-DRPA1977-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1977-PRINTE     PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1977-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
       
       01 PO-DRPA1977.
          05 PO-DRPA1977-RC            PIC  X(003).
             88 PO-DRPA1977-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1977-RC-NOT-FOUND                 VALUE 'NFD'.
             88 PO-DRPA1977-RC-UNZULAESSIG               VALUE 'UNZ'.
          05 PO-DRPA1977-DATEN.
             10 PO-DRPA1977-VERTRAGSTEIL
                                       PIC S9(004)       BINARY.
      *****************************************************************
      *---------------------- ENDE DRPA1977 --------------------------* 
      *****************************************************************
