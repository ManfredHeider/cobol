      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0901.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0901.CPv  $
      *
      *   Rev 2.1   May 20 2008 12:13:30   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.0   Apr 29 2008 14:39:10   t13ae48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0901                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0004                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MAESS                                            *
      *    DATUM:    29.04.2008                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00875!29.04.08! 01 ! Neuerstellung             !J.MAESS   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0901                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0901.
           05  PI-DRVB0901-DATEN.
               10 PI-DRVB0901-MA-ST-NR           PIC S9(009) COMP.
      *
       01  PO-DRVB0901.
           05  PO-DRVB0901-RC                    PIC  X(003).
               88 PO-DRVB0901-RC-OK                      VALUE 'OK '.
               88 PO-DRVB0901-RC-NOT-FOUND               VALUE 'NFD'.
           05  PO-DRVB0901-NI-VIVBJ004.
               10 NI-DRVB0901-MA-ST-NR          PIC S9(004) COMP.
               10 NI-DRVB0901-MA-LAST-UPDATE    PIC S9(04)  COMP.
               10 NI-DRVB0901-KTY-MA-TYP        PIC S9(04)  COMP.
               10 NI-DRVB0901-KNR-MA-TYP        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-BDV-MITGLIED   PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-STRAFE         PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-VERSANDSTOP    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-ZVII-ABL    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-RCK-STZ        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-RCK-STZ-DAT    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-FUEH-ZEUG-DAT  PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-SCHUFA-DAT     PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-34C-DAT        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-AFO            PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-DARLEHEN       PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-AVAG-DVAG      PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-KOMPASS-AVAG   PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-AUTOTELEFON    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-TELETEXT       PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-NR          PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-DAT-BIS-R   PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-DAT-BIS     PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-DAT-VON     PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-KUE-DAT     PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ED-ZUSATZ      PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ZV-VERL        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-FG-DAT         PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-127-DAT        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-VA-DAT         PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-ANWERBER       PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-BEFBED-NEU     PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-STEUER-NR      PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-GEWERBE-DAT    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-GRUNDSTUECKE   PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-127-UNBES-DAT  PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-BEFBED-WIEN-07 PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-REGISTER-NR    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-SCHULDNER-VERZ PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-GEW-ZENTR-REG  PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-INSOLVENZ-VERZ PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-AM-AGENT-NR    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-EINTR-DAT-AM   PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-HRD-ORT        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-HRD-EINTR      PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-HROE-EINTR-VA  PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-HROE-EINTR-VB  PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-BEFBED-DVAG-07 PIC S9(04)  COMP.
               10 NI-DRVB0901-KTY-NOTFALL-TYP   PIC S9(04)  COMP.
               10 NI-DRVB0901-KNR-NOTFALL-TYP   PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-34C-AMT        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-34C-STRASSE    PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-34C-PLZ        PIC S9(04)  COMP.
               10 NI-DRVB0901-MA-34C-ORT        PIC S9(04)  COMP.
           05  PO-DRVB0901-DCL-VIVBJ004.
               10 PO-DRVB0901-MA-ST-NR           PIC S9(009) COMP.
               10 PO-DRVB0901-MA-LAST-UPDATE     PIC  X(026).
               10 PO-DRVB0901-KTY-MA-TYP         PIC S9(004) COMP.
               10 PO-DRVB0901-KNR-MA-TYP         PIC S9(004) COMP.
               10 PO-DRVB0901-MA-BDV-MITGLIED    PIC  X(001).
               10 PO-DRVB0901-MA-STRAFE          PIC  X(001).
               10 PO-DRVB0901-MA-VERSANDSTOP     PIC  X(001).
               10 PO-DRVB0901-MA-ZV-ZVII-ABL     PIC  X(001).
               10 PO-DRVB0901-MA-RCK-STZ         PIC S999V99 COMP-3.
               10 PO-DRVB0901-MA-RCK-STZ-DAT     PIC  X(010).
               10 PO-DRVB0901-MA-FUEH-ZEUG-DAT   PIC  X(010).
               10 PO-DRVB0901-MA-SCHUFA-DAT      PIC  X(010).
               10 PO-DRVB0901-MA-34C-DAT         PIC  X(010).
               10 PO-DRVB0901-MA-AFO             PIC  X(001).
               10 PO-DRVB0901-MA-DARLEHEN        PIC  X(001).
               10 PO-DRVB0901-MA-AVAG-DVAG       PIC  X(010).
               10 PO-DRVB0901-MA-KOMPASS-AVAG    PIC  X(010).
               10 PO-DRVB0901-MA-AUTOTELEFON     PIC  X(026).
               10 PO-DRVB0901-MA-TELETEXT        PIC  X(026).
               10 PO-DRVB0901-MA-ZV-NR           PIC  X(001).
               10 PO-DRVB0901-MA-ZV-DAT-BIS-R    PIC  X(010).
               10 PO-DRVB0901-MA-ZV-DAT-BIS      PIC  X(010).
               10 PO-DRVB0901-MA-ZV-DAT-VON      PIC  X(010).
               10 PO-DRVB0901-MA-ZV-KUE-DAT      PIC  X(010).
               10 PO-DRVB0901-MA-ED-ZUSATZ       PIC S9(004) COMP.
               10 PO-DRVB0901-MA-ZV-VERL         PIC  X(001).
               10 PO-DRVB0901-MA-FG-DAT          PIC  X(010).
               10 PO-DRVB0901-MA-127-DAT         PIC  X(010).
               10 PO-DRVB0901-MA-VA-DAT          PIC  X(010).
               10 PO-DRVB0901-MA-ANWERBER        PIC S9(009) COMP.
               10 PO-DRVB0901-MA-BEFBED-NEU      PIC  X(010).
               10 PO-DRVB0901-MA-STEUER-NR       PIC  X(015).
               10 PO-DRVB0901-MA-GEWERBE-DAT     PIC  X(10).
               10 PO-DRVB0901-MA-GRUNDSTUECKE    PIC  X(1).
               10 PO-DRVB0901-MA-127-UNBES-DAT   PIC  X(10).
               10 PO-DRVB0901-MA-BEFBED-WIEN-07  PIC  X(10).
               10 PO-DRVB0901-MA-REGISTER-NR     PIC  X(30).
               10 PO-DRVB0901-MA-SCHULDNER-VERZ  PIC  X(10).
               10 PO-DRVB0901-MA-GEW-ZENTR-REG   PIC  X(10).
               10 PO-DRVB0901-MA-INSOLVENZ-VERZ  PIC  X(10).
               10 PO-DRVB0901-MA-AM-AGENT-NR     PIC S9(9) USAGE COMP.
               10 PO-DRVB0901-MA-EINTR-DAT-AM    PIC  X(10).
               10 PO-DRVB0901-MA-HRD-ORT         PIC  X(40).
               10 PO-DRVB0901-MA-HRD-EINTR       PIC  X(8).
               10 PO-DRVB0901-MA-HROE-EINTR-VA   PIC  X(25).
               10 PO-DRVB0901-MA-HROE-EINTR-VB   PIC  X(25).
               10 PO-DRVB0901-MA-BEFBED-DVAG-07  PIC  X(10).
               10 PO-DRVB0901-KTY-NOTFALL-TYP    PIC S9(4) USAGE COMP.
               10 PO-DRVB0901-KNR-NOTFALL-TYP    PIC S9(4) USAGE COMP.
               10 PO-DRVB0901-MA-34C-AMT         PIC  X(40).
               10 PO-DRVB0901-MA-34C-STRASSE     PIC  X(40).
               10 PO-DRVB0901-MA-34C-PLZ         PIC  X(09).
               10 PO-DRVB0901-MA-34C-ORT         PIC  X(40).
      ****
      **** ENDE COPY DRVB0901 ****************************************
      ****
