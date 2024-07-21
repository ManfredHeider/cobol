      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1031.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1031.CPv  $
      *
      *   Rev 2.2   Nov 04 2011 11:40:50   t13ae14
      *FVB00765: E-Mail von früher erneut erhalten
      *
      *   Rev 2.1   Nov 04 2011 10:35:02   t13ae14
      *FVB00765: E-Mail von früher erneut erhalten
      *
      *   Rev 2.0   Nov 03 2011 16:13:42   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      **    COPY-MEMBER: DRVB1031, ALLGEMEINE SCHNITTSTELLE           *
      **                 FÜR CTV-BATCH UPRO-CALL                      *
      **    --------------------------------------------------------  *
      **    ERSTELLT: K. OBER                                         *
      **    DATUM:    03.11.2011                                      *
      *****************************************************************
      **    VERSION   1
      *****************************************************************
      ** AENDERUNGEN:                                                 *
      ** AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      ** -------------------------------------------------------------*
      ** AVB00587!03.11.11!    ! NEU                       ! K.OBER   *
      **         !        !    !                           !          *
      **                                                              *
      ** ENDE-LOGBUCH                                                 *
      *****************************************************************
      *****************************************************************
      **                  PARAMETER-INPUT                             *
      *****************************************************************
        01  PI-DRVB1031.
            05  PI-DRVB1031-MEMZ-EMAIL         PIC  X(070).
            05  PI-DRVB1031-MEMZ-DAT-BIS       PIC  X(010).
            05  PI-DRVB1031-MA-ST-NR           PIC  S9(009) COMP.

      *****************************************************************
      **                  PARAMETER-OUTPUT                            *
      *****************************************************************
        01  PO-DRVB1031.
            05  PO-DRVB1031-RC                 PIC  X(003).
                88  PO-DRVB1031-RC-OK                      VALUE 'OK '.
                88  PO-DRVB1031-RC-NOK                     VALUE 'NOK'.
                88  PO-DRVB1031-RC-NOT-FOUND               VALUE 'NFD'.

            05  PO-DRVB1031-MA-ST-NR           PIC  S9(009) COMP.
            05  PO-DRVB1031-MEMZ-DAT-VON       PIC  X(026).

            05 PO-DRVB1031-DATEN.
               10 PO-DRVB1031-ANZAHL           PIC  9(004).
      *********** ENDE COPY-MEMBER ************************************

