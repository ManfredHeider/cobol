      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY03823   !16.09.21! NEU                          ! SCHEIDEL *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRPA0576.
           05 PI-DRPA0576-PAAA-NAME           PIC  X(030).
      * 
       01  PO-DRPA0576.
           05 PO-DRPA0576-RC                PIC  X(003).
              88 PO-DRPA0576-RC-OK               VALUE 'OK '.
              88 PO-DRPA0576-RC-NFD              VALUE 'NFD'.
           05 PO-DRPA0576-DATEN.
              10 PO-DRPA0576-PAAA-KEY.
                 15 PO-DRPA0576-PAAA-KEY-LEN
                                            PIC S9(004) BINARY.
                 15 PO-DRPA0576-PAAA-KEY-TEXT
                                            PIC  X(1000).
              10 PO-DRPA0576-PAAA-ZUSTAND   PIC S9(004) BINARY.
              10 PO-DRPA0576-PAAA-LFDNR     PIC S9(004) BINARY.
              10 PO-DRPA0576-PAAA-ZUSATZ-INFO. 
                 15 PO-DRPA0576-PAAA-Z-INFO-LEN 
                                            PIC S9(004) BINARY.
                 15 PO-DRPA0576-PAAA-Z-INFO-TEXT
                                            PIC  X(1400).
              10 PO-DRPA0576-PAAA-KENNUNG   PIC  X(008).
      *
      *
      *****************************************************************
      *---------------------- ENDE DRPA0576 --------------------------* 
      *****************************************************************
      