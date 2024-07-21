      *****************************************************************
      *    COPY-MEMBER: DRPA1959                                     
      *                                                              
      *    INSERT IN TBPA0019                                        
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
        01 PI-DRPA1959.
           05 PI-DRPA1959-DATEN.
              10 PI-DRPA1959-PAAA-NAME      PIC  X(030).
              10 PI-DRPA1959-PAAA-PRIO      PIC S9(004) BINARY.
              10 PI-DRPA1959-PAAA-KEY       PIC  X(1000).
              10 PI-DRPA1959-PAAA-LFDNR     PIC S9(004) BINARY.
              10 PI-DRPA1959-PAAA-ZUSTAND   PIC S9(004) BINARY.
              10 PI-DRPA1959-PAAA-ZUSATZ-INFO
                                            PIC  X(1400).
              10 PI-DRPA1959-PAAA-LAST-UPDATE
                                            PIC  X(026).
              10 PI-DRPA1959-PAAA-KENNUNG   PIC  X(008).
      *
        01 PO-DRPA1959.
           05 PO-DRPA1959-RC                PIC  X(003).
              88 PO-DRPA1959-RC-OK               VALUE 'OK '.
              88 PO-DRPA1959-RC-DUPLICATE-VALUE  VALUE 'DVA'.
      *
      *****************************************************************
      *---------------------- ENDE DRPA1959 --------------------------* 
      *****************************************************************
