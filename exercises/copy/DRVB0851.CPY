      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0851.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0851.CPv  $
      *
      *   Rev 2.1   16 May 2007 11:21:30   t13ae13
      *AVB00908: Neuer VB-Vertrag 2007 - UPVB0128
      *
      *   Rev 2.0   30 Mar 2007 15:54:52   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0851                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0004                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    10.04.2007                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00906!10.04.07!2.1 ! NEU                       !GUTFLEISCH*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB0851.
           05  PI-DRVB0851-DATEN.
               10 PI-DRVB0851-MA-ST-NR         PIC S9(009) COMP.
      *
       01  PO-DRVB0851.
           05  PO-DRVB0851-RC                  PIC  X(003).
               88 PO-DRVB0851-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0851-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0851-NI-VIVBH004.
               10 NI-DRVB0851-MA-ST-NR         PIC S9(004) COMP.
               10 NI-DRVB0851-MA-LAST-UPDATE   PIC S9(004) COMP.
               10 NI-DRVB0851-KTY-MA-TYP       PIC S9(004) COMP.
               10 NI-DRVB0851-KNR-MA-TYP       PIC S9(004) COMP.
               10 NI-DRVB0851-MA-BDV-MITGLIED  PIC S9(004) COMP.
               10 NI-DRVB0851-MA-STRAFE        PIC S9(004) COMP.
               10 NI-DRVB0851-MA-VERSANDSTOP   PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-ZVII-ABL   PIC S9(004) COMP.
               10 NI-DRVB0851-MA-RCK-STZ       PIC S9(004) COMP.
               10 NI-DRVB0851-MA-RCK-STZ-DAT   PIC S9(004) COMP.
               10 NI-DRVB0851-MA-FUEH-ZEUG-DAT PIC S9(004) COMP.
               10 NI-DRVB0851-MA-SCHUFA-DAT    PIC S9(004) COMP.
               10 NI-DRVB0851-MA-34C-DAT       PIC S9(004) COMP.
               10 NI-DRVB0851-MA-AFO           PIC S9(004) COMP.
               10 NI-DRVB0851-MA-DARLEHEN      PIC S9(004) COMP.
               10 NI-DRVB0851-MA-AVAG-DVAG     PIC S9(004) COMP.
               10 NI-DRVB0851-MA-KOMPASS-AVAG  PIC S9(004) COMP.
               10 NI-DRVB0851-MA-AUTOTELEFON   PIC S9(004) COMP.
               10 NI-DRVB0851-MA-TELETEXT      PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-NR         PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-DAT-BIS-R  PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-DAT-BIS    PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-DAT-VON    PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-KUE-DAT    PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ED-ZUSATZ     PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ZV-VERL       PIC S9(004) COMP.
               10 NI-DRVB0851-MA-FG-DAT        PIC S9(004) COMP.
               10 NI-DRVB0851-MA-127-DAT       PIC S9(004) COMP.
               10 NI-DRVB0851-MA-VA-DAT        PIC S9(004) COMP.
               10 NI-DRVB0851-MA-ANWERBER      PIC S9(004) COMP.
               10 NI-DRVB0851-MA-BEFBED-NEU    PIC S9(004) COMP.
               10 NI-DRVB0851-MA-STEUER-NR     PIC S9(004) COMP.
               10 NI-DRVB0851-MA-GEWERBE-DAT    PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-GRUNDSTUECKE   PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-127-UNBES-DAT  PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-BEFBED-WIEN-07 PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-REGISTER-NR    PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-SCHULDNER-VERZ PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-GEW-ZENTR-REG  PIC S9(04)  COMP.
               10 NI-DRVB0851-MA-INSOLVENZ-VERZ PIC S9(04)  COMP.

           05  PO-DRVB0851-DCL-VIVBH004.
               10 PO-DRVB0851-MA-ST-NR         PIC S9(009) COMP.
               10 PO-DRVB0851-MA-LAST-UPDATE   PIC  X(026).
               10 PO-DRVB0851-KTY-MA-TYP       PIC S9(004) COMP.
               10 PO-DRVB0851-KNR-MA-TYP       PIC S9(004) COMP.
               10 PO-DRVB0851-MA-BDV-MITGLIED  PIC  X(001).
               10 PO-DRVB0851-MA-STRAFE        PIC  X(001).
               10 PO-DRVB0851-MA-VERSANDSTOP   PIC  X(001).
               10 PO-DRVB0851-MA-ZV-ZVII-ABL   PIC  X(001).
               10 PO-DRVB0851-MA-RCK-STZ       PIC S999V99 COMP-3.
               10 PO-DRVB0851-MA-RCK-STZ-DAT   PIC  X(010).
               10 PO-DRVB0851-MA-FUEH-ZEUG-DAT PIC  X(010).
               10 PO-DRVB0851-MA-SCHUFA-DAT    PIC  X(010).
               10 PO-DRVB0851-MA-34C-DAT       PIC  X(010).
               10 PO-DRVB0851-MA-AFO           PIC  X(001).
               10 PO-DRVB0851-MA-DARLEHEN      PIC  X(001).
               10 PO-DRVB0851-MA-AVAG-DVAG     PIC  X(010).
               10 PO-DRVB0851-MA-KOMPASS-AVAG  PIC  X(010).
               10 PO-DRVB0851-MA-AUTOTELEFON   PIC  X(026).
               10 PO-DRVB0851-MA-TELETEXT      PIC  X(026).
               10 PO-DRVB0851-MA-ZV-NR         PIC  X(001).
               10 PO-DRVB0851-MA-ZV-DAT-BIS-R  PIC  X(010).
               10 PO-DRVB0851-MA-ZV-DAT-BIS    PIC  X(010).
               10 PO-DRVB0851-MA-ZV-DAT-VON    PIC  X(010).
               10 PO-DRVB0851-MA-ZV-KUE-DAT    PIC  X(010).
               10 PO-DRVB0851-MA-ED-ZUSATZ     PIC S9(004) COMP.
               10 PO-DRVB0851-MA-ZV-VERL       PIC  X(001).
               10 PO-DRVB0851-MA-FG-DAT        PIC  X(010).
               10 PO-DRVB0851-MA-127-DAT       PIC  X(010).
               10 PO-DRVB0851-MA-VA-DAT        PIC  X(010).
               10 PO-DRVB0851-MA-ANWERBER      PIC S9(009) COMP.
               10 PO-DRVB0851-MA-BEFBED-NEU    PIC  X(010).
               10 PO-DRVB0851-MA-STEUER-NR     PIC  X(015).
               10 PO-DRVB0851-MA-GEWERBE-DAT    PIC X(010).
               10 PO-DRVB0851-MA-GRUNDSTUECKE   PIC X(001).
               10 PO-DRVB0851-MA-127-UNBES-DAT  PIC X(010).
               10 PO-DRVB0851-MA-BEFBED-WIEN-07 PIC X(010).
               10 PO-DRVB0851-MA-REGISTER-NR    PIC X(030).
               10 PO-DRVB0851-MA-SCHULDNER-VERZ PIC X(010).
               10 PO-DRVB0851-MA-GEW-ZENTR-REG  PIC X(010).
               10 PO-DRVB0851-MA-INSOLVENZ-VERZ PIC X(010).
      ****
      **** ENDE COPY DRVB0851 ****************************************
      ****
