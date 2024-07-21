      *****************************************************************
      *    COPY-MEMBER: DRPA1958                                     
      *                                                              
      *    SELECT ALL TBPA0019                                       
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
        01 PI-DRPA1958.
           05 PI-DRPA1958-SCHLUESSEL.
              10 PI-DRPA1958-PAAA-NAME      PIC  X(030).
              10 PI-DRPA1958-PAAA-KEY       PIC  X(1000).
              10 PI-DRPA1958-PAAA-LFDNR     PIC S9(004) BINARY.
      *
        01 PO-DRPA1958.
           05 PO-DRPA1958-RC                PIC  X(003).
              88 PO-DRPA1958-RC-OK               VALUE 'OK '.
              88 PO-DRPA1958-RC-NOT-FOUND        VALUE 'NFD'.
           05 PO-DRPA1958-PAAA-DATEN.
              10 PO-DRPA1958-PAAA-NAME      PIC  X(030).
              10 PO-DRPA1958-PAAA-PRIO      PIC S9(004) BINARY.
              10 PO-DRPA1958-PAAA-KEY.
                 15 PO-DRPA1958-PAAA-KEY-LEN 
                                            PIC S9(004) BINARY.
                 15 PO-DRPA1958-PAAA-KEY-TEXT  
                                            PIC  X(1000).
              10 PO-DRPA1958-PAAA-LFDNR     PIC S9(004) BINARY.
              10 PO-DRPA1958-PAAA-ZUSTAND   PIC S9(004) BINARY.
              10 PO-DRPA1958-PAAA-ZUSATZ-INFO.
                 15 PO-DRPA1958-PAAA-Z-INFO-LEN  
                                            PIC S9(004) BINARY.
                 15 PO-DRPA1958-PAAA-Z-INFO-TEXT 
                                            PIC  X(1400).
              10 PO-DRPA1958-PAAA-LAST-UPDATE   
                                            PIC  X(026).
              10 PO-DRPA1958-PAAA-KENNUNG   PIC  X(008).
      *
      *****************************************************************
      *---------------------- ENDE DRPA1958 --------------------------* 
      *****************************************************************
