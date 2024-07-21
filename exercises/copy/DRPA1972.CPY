      *****************************************************************
      *    COPY-MEMBER: DRPA1972                                     
      *                                                              
      *   FUNKTION GEVO VERTRAGSNEUORDNUNG                      
      *                                                              
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY04569   !12.05.22! NEU                          ! SCHEIDEL *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
        01 PI-DRPA1972.
           05 PI-DRPA1972-KNR-MANDANT  PIC S9(004) BINARY.
           05 PI-DRPA1972-PVER-ZUSTAND PIC S9(004) BINARY.
           05 PI-DRPA1972-ANTA-ZUSTAND PIC S9(004) BINARY.
           05 PI-DRPA1972-PIN-VNR-AKZ  PIC S9(004) BINARY.
           
        01 PO-DRPA1972.
           05 PO-DRPA1972-RC           PIC  X(003).
              88 PO-DRPA1972-RC-OK                    VALUE 'OK '.
              88 PO-DRPA1972-RC-MELDUNGSFEHLER        VALUE 'MFE'.
           05 PO-DRPA1972-STRUK-KDBETR PIC  X(001).
           05 PO-DRPA1972-KDBETR       PIC S9(009) BINARY.

      *---------------------- ENDE DRPA1972 --------------------------* 
      *****************************************************************
