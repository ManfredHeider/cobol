      *****************************************************************
      *    COPY-MEMBER: DRPA1961                                     
      *                                                              
      *    UPDATE TBPA0019               
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY03823   !16.09.21! NEU                          ! SCHEIDEL *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
        01 PI-DRPA1961.
           05 PI-DRPA1961-PAAA-SCHLUESSEL.
              10 PI-DRPA1961-PAAA-NAME      PIC  X(030).
              10 PI-DRPA1961-PAAA-PRIO      PIC S9(004) BINARY.
              10 PI-DRPA1961-PAAA-KEY       PIC  X(1000).  
              10 PI-DRPA1961-PAAA-LFDNR     PIC S9(004) BINARY.
              10 PI-DRPA1961-PAAA-ZUSTAND   PIC S9(004) BINARY.
              10 PI-DRPA1961-PAAA-ZUSATZ-INFO
                                            PIC  X(1400).
              10 PI-DRPA1961-PAAA-LAST-UPDATE
                                            PIC  X(026).
              10 PI-DRPA1961-PAAA-KENNUNG   PIC  X(008).
      *
        01 PO-DRPA1961.
           05 PO-DRPA1961-RC                PIC  X(003).
              88 PO-DRPA1961-RC-OK               VALUE 'OK '.
              88 PO-DRPA1961-RC-NOT-FOUND        VALUE 'NFD'.
              88 PO-DRPA1961-RC-CHANGED          VALUE 'CHA'.
      *
      *****************************************************************
      *---------------------- ENDE DRPA1961 --------------------------* 
      *****************************************************************
