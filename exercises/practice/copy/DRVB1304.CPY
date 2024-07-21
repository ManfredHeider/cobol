      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.01.2021
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1304.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1304.CPv  $
      *
      *   Rev 2.1   Feb 24 2021 11:33:38   t13ae56
      *ACV00188: PGM - Vereinfachung jährliches VSW-Zusageschreiben
      *
      *   Rev 2.0   Jan 12 2021 15:12:52   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VLA-431 !13.01.21!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            COPY-MEMBER FÜR CTV-BRIEFE VSP-ZUSAGEANSCHREIBEN   *
      *            DATEINUMMER: CV01230_ VSAM                         *
      *            COPYMEMBER:  DRVB1304                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRVB1304-SATZ.
           02 DRVB1304-VBNR                     PIC 9(007).
           02 DRVB1304-JAHR                     PIC 9(004).
           02 DRVB1304-GV-KZ                    PIC X(001).
           02 DRVB1304-GV-BETRAG                PIC 9(006),99.
           02 DRVB1304-GV-VSNR-LV               PIC 9(010).
           02 DRVB1304-GV-VSNR-RV               PIC 9(010).
           02 DRVB1304-WG-KZ                    PIC X(001).
           02 DRVB1304-WG-BETRAG                PIC 9(006),99.
           02 DRVB1304-WG-VSNR-LV               PIC 9(010).
           02 DRVB1304-WG-VSNR-RV               PIC 9(010).
           02 DRVB1304-AV-KZ                    PIC X(001).
           02 DRVB1304-AV-BETRAG                PIC 9(006),99.
           02 DRVB1304-AV-KONTO                 PIC X(010).
           02 DRVB1304-ED-KZ                    PIC X(001).
           02 DRVB1304-ED-BETRAG                PIC 9(006),99.
           02 DRVB1304-VSP-SUMME                PIC 9(006),99.
      *
      **** ENDE DRVB1304
      *

