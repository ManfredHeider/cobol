      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0928.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0928.CPv  $
      *
      *   Rev 2.2   Mar 25 2009 10:08:40   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.1   Feb 20 2009 13:55:42   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Feb 20 2009 11:30:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0928                                      *
      *    --------------------------------------------------------   *
250309*    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0116 --> ZWVB0300       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.02.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00812!20.02.09!2.1 ! NEU                       ! KLEEMEIER *
250309* AAS00812!25.03.09!2.2 ! ZWVB0300 STATT ZWVB0116   ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0928 - ERMITTELN CPROF-SATZ                   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0928.
           05  PI-DRVB0928-DATEN.
               10  PI-DRVB0928-MA-ST-NR         PIC S9(09)       BINARY.
               10  PI-DRVB0928-CPROF-MONAT      PIC  X(10).
               10  PI-DRVB0928-KTY-DISZIPLIN    PIC S9(04)       BINARY.
               10  PI-DRVB0928-KNR-DISZIPLIN    PIC S9(04)       BINARY.
      *
       01  PO-DRVB0928.
         03  PO-DRVB0928-RC                     PIC  X(03).
               88  PO-DRVB0928-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0928-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB0928-DATEN.
250309**** 05  PO-DRVB0928-NI-VIVB0116.
250309     05  PO-DRVB0928-NI-VIVB0300.
               10  NI-DRVB0928-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-MONAT      PIC S9(04)       BINARY.
               10  NI-DRVB0928-KTY-DISZIPLIN    PIC S9(04)       BINARY.
               10  NI-DRVB0928-KNR-DISZIPLIN    PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-STERN      PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-ART        PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-LAST-UPD   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-U-EH-GES   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-U-EH-EGFR  PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-U-EH-WERTG PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EQF-GES    PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EQF-EGFR   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EQF-WERTG  PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EGFR-AB    PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EGFR-BIS   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-EH1      PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-EH2      PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-EH3      PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-EH4      PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-STEIG1   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-STEIG2   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-STEIG3   PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-W-STEIGGES PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-ANZ-ASTRUK PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EH-NP      PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-EH-GES     PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-ANTEIL-NP  PIC S9(04)       BINARY.
               10  NI-DRVB0928-CPROF-ED-ANZ     PIC S9(04)       BINARY.
250309**** 05  PO-DRVB0928-DCL-VIVB0116.
250309     05  PO-DRVB0928-DCL-VIVB0300.
               10  PO-DRVB0928-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB0928-CPROF-MONAT      PIC  X(10).
               10  PO-DRVB0928-KTY-DISZIPLIN    PIC S9(04)       BINARY.
               10  PO-DRVB0928-KNR-DISZIPLIN    PIC S9(04)       BINARY.
               10  PO-DRVB0928-CPROF-STERN      PIC S9(04)       BINARY.
               10  PO-DRVB0928-CPROF-ART        PIC  X(01).
               10  PO-DRVB0928-CPROF-LAST-UPD   PIC  X(26).
               10  PO-DRVB0928-CPROF-U-EH-GES   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-U-EH-EGFR  PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-U-EH-WERTG PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-EQF-GES    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-EQF-EGFR   PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-EQF-WERTG  PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-EGFR-AB    PIC  X(10).
               10  PO-DRVB0928-CPROF-EGFR-BIS   PIC  X(10).
               10  PO-DRVB0928-CPROF-W-EH1      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-W-EH2      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-W-EH3      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-W-EH4      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-W-STEIG1   PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-W-STEIG2   PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-W-STEIG3   PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-W-STEIGGES PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-ANZ-ASTRUK PIC S9(04)       BINARY.
               10  PO-DRVB0928-CPROF-EH-NP      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-EH-GES     PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0928-CPROF-ANTEIL-NP  PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0928-CPROF-ED-ANZ     PIC S9(04)       BINARY.
      ****
      **** ENDE COPY DRVB0928 ****************************************
      ****
