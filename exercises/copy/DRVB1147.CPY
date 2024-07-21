      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1147.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1147.CPv  $
      *
      *   Rev 2.1   Sep 09 2016 11:36:22   t13ae13
      *AVB01400: Projekt Lucy2 - ZWVB0004, ZWVB0015 (SWL)
      *
      *   Rev 2.0   Aug 30 2016 12:46:42   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1147                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0004                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    30.08.2016                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!30.08.16! 01 ! Neuerstellung             !S. Walb   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1147                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB1147.
           05  PI-DRVB1147-DATEN.
               10 PI-DRVB1147-MA-ST-NR           PIC S9(009) COMP.
      *
       01  PO-DRVB1147.
           05  PO-DRVB1147-RC                    PIC  X(003).
               88 PO-DRVB1147-RC-OK                      VALUE 'OK '.
               88 PO-DRVB1147-RC-NOT-FOUND               VALUE 'NFD'.
           05  PO-DRVB1147-NI-VIVBO004.
               10 NI-DRVB1147-MA-ST-NR           PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-LAST-UPDATE     PIC S9(04)  COMP.
               10 NI-DRVB1147-KTY-MA-TYP         PIC S9(04)  COMP.
               10 NI-DRVB1147-KNR-MA-TYP         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-BDV-MITGLIED    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-STRAFE          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-VERSANDSTOP     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-ZVII-ABL     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-RCK-STZ         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-RCK-STZ-DAT     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-FUEH-ZEUG-DAT   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-SCHUFA-DAT      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34C-DAT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-AFO             PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-DARLEHEN        PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-AVAG-DVAG       PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-KOMPASS-AVAG    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-AUTOTELEFON     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-TELETEXT        PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-NR           PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-DAT-BIS-R    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-DAT-BIS      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-DAT-VON      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-KUE-DAT      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ED-ZUSATZ       PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-VERL         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-FG-DAT          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-127-DAT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-VA-DAT          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ANWERBER        PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-BEFBED-NEU      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-STEUER-NR       PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-GEWERBE-DAT     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-GRUNDSTUECKE    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-127-UNBES-DAT   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-BEFBED-WIEN-07  PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-REGISTER-NR     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-SCHULDNER-VERZ  PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-GEW-ZENTR-REG   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-INSOLVENZ-VERZ  PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-AM-AGENT-NR     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-EINTR-DAT-AM    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-HRD-ORT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-HRD-EINTR       PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-HROE-EINTR-VA   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-HROE-EINTR-VB   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-BEFBED-DVAG-07  PIC S9(04)  COMP.
               10 NI-DRVB1147-KTY-NOTFALL-TYP    PIC S9(04)  COMP.
               10 NI-DRVB1147-KNR-NOTFALL-TYP    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34C-AMT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34C-STRASSE     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34C-PLZ         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34C-ORT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ZV-ABSICHERG    PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-FG-VOM          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-127-VOM         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-VA-VOM          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-FUEH-ZEUG-VOM   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-SCHUFA-VOM      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-GEW-Z-REG-VOM   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-ANLAGE-BER      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-HROE-EINTR-FG   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-VSH-POLNR       PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-VSH-DAT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-VSH-VOM         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-HROE-EINTR-WG   PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-WG-DAT          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-WG-VOM          PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-BEFBED-2011     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-DAT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-ANT-SCH     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-AFO         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-ANL-BER     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-AMT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-STRASSE     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-PLZ         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-ORT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34C-ANT-SCH     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34F-REG-NR      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34I-DAT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34I-AMT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34I-STRASSE     PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34I-PLZ         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34I-ORT         PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-34I-REG-NR      PIC S9(04)  COMP.
               10 NI-DRVB1147-MA-SOZ-VSNR-WIEN   PIC S9(04)  COMP.
           05  PO-DRVB1147-DCL-VIVBO004.
               10 PO-DRVB1147-MA-ST-NR           PIC S9(009) COMP.
               10 PO-DRVB1147-MA-LAST-UPDATE     PIC  X(026).
               10 PO-DRVB1147-KTY-MA-TYP         PIC S9(004) COMP.
               10 PO-DRVB1147-KNR-MA-TYP         PIC S9(004) COMP.
               10 PO-DRVB1147-MA-BDV-MITGLIED    PIC  X(001).
               10 PO-DRVB1147-MA-STRAFE          PIC  X(001).
               10 PO-DRVB1147-MA-VERSANDSTOP     PIC  X(001).
               10 PO-DRVB1147-MA-ZV-ZVII-ABL     PIC  X(001).
               10 PO-DRVB1147-MA-RCK-STZ         PIC S999V99 COMP-3.
               10 PO-DRVB1147-MA-RCK-STZ-DAT     PIC  X(010).
               10 PO-DRVB1147-MA-FUEH-ZEUG-DAT   PIC  X(010).
               10 PO-DRVB1147-MA-SCHUFA-DAT      PIC  X(010).
               10 PO-DRVB1147-MA-34C-DAT         PIC  X(010).
               10 PO-DRVB1147-MA-AFO             PIC  X(001).
               10 PO-DRVB1147-MA-DARLEHEN        PIC  X(001).
               10 PO-DRVB1147-MA-AVAG-DVAG       PIC  X(010).
               10 PO-DRVB1147-MA-KOMPASS-AVAG    PIC  X(010).
               10 PO-DRVB1147-MA-AUTOTELEFON     PIC  X(026).
               10 PO-DRVB1147-MA-TELETEXT        PIC  X(026).
               10 PO-DRVB1147-MA-ZV-NR           PIC  X(001).
               10 PO-DRVB1147-MA-ZV-DAT-BIS-R    PIC  X(010).
               10 PO-DRVB1147-MA-ZV-DAT-BIS      PIC  X(010).
               10 PO-DRVB1147-MA-ZV-DAT-VON      PIC  X(010).
               10 PO-DRVB1147-MA-ZV-KUE-DAT      PIC  X(010).
               10 PO-DRVB1147-MA-ED-ZUSATZ       PIC S9(004) COMP.
               10 PO-DRVB1147-MA-ZV-VERL         PIC  X(001).
               10 PO-DRVB1147-MA-FG-DAT          PIC  X(010).
               10 PO-DRVB1147-MA-127-DAT         PIC  X(010).
               10 PO-DRVB1147-MA-VA-DAT          PIC  X(010).
               10 PO-DRVB1147-MA-ANWERBER        PIC S9(009) COMP.
               10 PO-DRVB1147-MA-BEFBED-NEU      PIC  X(010).
               10 PO-DRVB1147-MA-STEUER-NR       PIC  X(015).
               10 PO-DRVB1147-MA-GEWERBE-DAT     PIC  X(10).
               10 PO-DRVB1147-MA-GRUNDSTUECKE    PIC  X(1).
               10 PO-DRVB1147-MA-127-UNBES-DAT   PIC  X(10).
               10 PO-DRVB1147-MA-BEFBED-WIEN-07  PIC  X(10).
               10 PO-DRVB1147-MA-REGISTER-NR     PIC  X(30).
               10 PO-DRVB1147-MA-SCHULDNER-VERZ  PIC  X(10).
               10 PO-DRVB1147-MA-GEW-ZENTR-REG   PIC  X(10).
               10 PO-DRVB1147-MA-INSOLVENZ-VERZ  PIC  X(10).
               10 PO-DRVB1147-MA-AM-AGENT-NR     PIC S9(9) USAGE COMP.
               10 PO-DRVB1147-MA-EINTR-DAT-AM    PIC  X(10).
               10 PO-DRVB1147-MA-HRD-ORT         PIC  X(40).
               10 PO-DRVB1147-MA-HRD-EINTR       PIC  X(8).
               10 PO-DRVB1147-MA-HROE-EINTR-VA   PIC  X(25).
               10 PO-DRVB1147-MA-HROE-EINTR-VB   PIC  X(25).
               10 PO-DRVB1147-MA-BEFBED-DVAG-07  PIC  X(10).
               10 PO-DRVB1147-KTY-NOTFALL-TYP    PIC S9(4) USAGE COMP.
               10 PO-DRVB1147-KNR-NOTFALL-TYP    PIC S9(4) USAGE COMP.
               10 PO-DRVB1147-MA-34C-AMT         PIC  X(40).
               10 PO-DRVB1147-MA-34C-STRASSE     PIC  X(40).
               10 PO-DRVB1147-MA-34C-PLZ         PIC  X(09).
               10 PO-DRVB1147-MA-34C-ORT         PIC  X(40).
               10 PO-DRVB1147-MA-ZV-ABSICHERG   PIC S9(09)V9(02) COMP-3.
               10 PO-DRVB1147-MA-FG-VOM          PIC  X(10).
               10 PO-DRVB1147-MA-127-VOM         PIC  X(10).
               10 PO-DRVB1147-MA-VA-VOM          PIC  X(10).
               10 PO-DRVB1147-MA-FUEH-ZEUG-VOM   PIC  X(10).
               10 PO-DRVB1147-MA-SCHUFA-VOM      PIC  X(10).
               10 PO-DRVB1147-MA-GEW-Z-REG-VOM   PIC  X(10).
               10 PO-DRVB1147-MA-ANLAGE-BER      PIC  X(01).
               10 PO-DRVB1147-MA-HROE-EINTR-FG   PIC  X(25).
               10 PO-DRVB1147-MA-VSH-POLNR       PIC  X(25).
               10 PO-DRVB1147-MA-VSH-DAT         PIC  X(10).
               10 PO-DRVB1147-MA-VSH-VOM         PIC  X(10).
               10 PO-DRVB1147-MA-HROE-EINTR-WG   PIC  X(25).
               10 PO-DRVB1147-MA-WG-DAT          PIC  X(10).
               10 PO-DRVB1147-MA-WG-VOM          PIC  X(10).
               10 PO-DRVB1147-MA-BEFBED-2011     PIC  X(10).
               10 PO-DRVB1147-MA-34F-DAT         PIC  X(10).
               10 PO-DRVB1147-MA-34F-ANT-SCH     PIC  X(01).
               10 PO-DRVB1147-MA-34F-AFO         PIC  X(01).
               10 PO-DRVB1147-MA-34F-ANL-BER     PIC  X(01).
               10 PO-DRVB1147-MA-34F-AMT         PIC  X(40).
               10 PO-DRVB1147-MA-34F-STRASSE     PIC  X(40).
               10 PO-DRVB1147-MA-34F-PLZ         PIC  X(09).
               10 PO-DRVB1147-MA-34F-ORT         PIC  X(40).
               10 PO-DRVB1147-MA-34C-ANT-SCH     PIC  X(01).
               10 PO-DRVB1147-MA-34F-REG-NR      PIC  X(15).
               10 PO-DRVB1147-MA-34I-DAT         PIC  X(10).
               10 PO-DRVB1147-MA-34I-AMT         PIC  X(40).
               10 PO-DRVB1147-MA-34I-STRASSE     PIC  X(40).
               10 PO-DRVB1147-MA-34I-PLZ         PIC  X(09).
               10 PO-DRVB1147-MA-34I-ORT         PIC  X(40).
               10 PO-DRVB1147-MA-34I-REG-NR      PIC  X(15).
               10 PO-DRVB1147-MA-SOZ-VSNR-WIEN   PIC  X(15).
      ****
      **** ENDE COPY DRVB1147 ****************************************
      ****

