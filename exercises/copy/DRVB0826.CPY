      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0826.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0826.CPv  $
      *
      *   Rev 2.2   May 20 2008 12:12:44   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.1   Jun 08 2006 14:49:06   t13ex43
      *AVB00850: PGM - Autom. Rückstufung von VBA-K zu VBA-S
      *
      *   Rev 2.0   Jun 02 2006 13:58:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0826                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0004                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.06.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00850!02.06.06!2.1 ! NEU                       ! KLEEMEIER*
      * AVB00875!11.03.08!2.2 ! Autotel.+Teletext 14 -> 26! MAESS    *
      *         !        !    !                           !          *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB0826.
           05  PI-DRVB0826-DATEN.
               10 PI-DRVB0826-MA-ST-NR         PIC S9(009) COMP.
      *
       01  PO-DRVB0826.
           05  PO-DRVB0826-RC                  PIC  X(003).
               88 PO-DRVB0826-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0826-RC-NOT-FOUND                 VALUE 'NFD'.
110308*    05  PO-DRVB0826-NI-VIVBE004.
110308     05  PO-DRVB0826-NI-VIVBJ004.
               10 NI-DRVB0826-MA-ST-NR         PIC S9(004) COMP.
               10 NI-DRVB0826-MA-LAST-UPDATE   PIC S9(004) COMP.
               10 NI-DRVB0826-KTY-MA-TYP       PIC S9(004) COMP.
               10 NI-DRVB0826-KNR-MA-TYP       PIC S9(004) COMP.
               10 NI-DRVB0826-MA-BDV-MITGLIED  PIC S9(004) COMP.
               10 NI-DRVB0826-MA-STRAFE        PIC S9(004) COMP.
               10 NI-DRVB0826-MA-VERSANDSTOP   PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-ZVII-ABL   PIC S9(004) COMP.
               10 NI-DRVB0826-MA-RCK-STZ       PIC S9(004) COMP.
               10 NI-DRVB0826-MA-RCK-STZ-DAT   PIC S9(004) COMP.
               10 NI-DRVB0826-MA-FUEH-ZEUG-DAT PIC S9(004) COMP.
               10 NI-DRVB0826-MA-SCHUFA-DAT    PIC S9(004) COMP.
               10 NI-DRVB0826-MA-34C-DAT       PIC S9(004) COMP.
               10 NI-DRVB0826-MA-AFO           PIC S9(004) COMP.
               10 NI-DRVB0826-MA-DARLEHEN      PIC S9(004) COMP.
               10 NI-DRVB0826-MA-AVAG-DVAG     PIC S9(004) COMP.
               10 NI-DRVB0826-MA-KOMPASS-AVAG  PIC S9(004) COMP.
               10 NI-DRVB0826-MA-AUTOTELEFON   PIC S9(004) COMP.
               10 NI-DRVB0826-MA-TELETEXT      PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-NR         PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-DAT-BIS-R  PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-DAT-BIS    PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-DAT-VON    PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-KUE-DAT    PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ED-ZUSATZ     PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ZV-VERL       PIC S9(004) COMP.
               10 NI-DRVB0826-MA-FG-DAT        PIC S9(004) COMP.
               10 NI-DRVB0826-MA-127-DAT       PIC S9(004) COMP.
               10 NI-DRVB0826-MA-VA-DAT        PIC S9(004) COMP.
               10 NI-DRVB0826-MA-ANWERBER      PIC S9(004) COMP.
               10 NI-DRVB0826-MA-BEFBED-NEU    PIC S9(004) COMP.
               10 NI-DRVB0826-MA-STEUER-NR     PIC S9(004) COMP.
110308*    05  PO-DRVB0826-DCL-VIVBE004.
110308     05  PO-DRVB0826-DCL-VIVBJ004.
               10 PO-DRVB0826-MA-ST-NR         PIC S9(009) COMP.
               10 PO-DRVB0826-MA-LAST-UPDATE   PIC  X(026).
               10 PO-DRVB0826-KTY-MA-TYP       PIC S9(004) COMP.
               10 PO-DRVB0826-KNR-MA-TYP       PIC S9(004) COMP.
               10 PO-DRVB0826-MA-BDV-MITGLIED  PIC  X(001).
               10 PO-DRVB0826-MA-STRAFE        PIC  X(001).
               10 PO-DRVB0826-MA-VERSANDSTOP   PIC  X(001).
               10 PO-DRVB0826-MA-ZV-ZVII-ABL   PIC  X(001).
               10 PO-DRVB0826-MA-RCK-STZ       PIC S999V99 COMP-3.
               10 PO-DRVB0826-MA-RCK-STZ-DAT   PIC  X(010).
               10 PO-DRVB0826-MA-FUEH-ZEUG-DAT PIC  X(010).
               10 PO-DRVB0826-MA-SCHUFA-DAT    PIC  X(010).
               10 PO-DRVB0826-MA-34C-DAT       PIC  X(010).
               10 PO-DRVB0826-MA-AFO           PIC  X(001).
               10 PO-DRVB0826-MA-DARLEHEN      PIC  X(001).
               10 PO-DRVB0826-MA-AVAG-DVAG     PIC  X(010).
               10 PO-DRVB0826-MA-KOMPASS-AVAG  PIC  X(010).
110308*        10 PO-DRVB0826-MA-AUTOTELEFON   PIC  X(014).
110308*        10 PO-DRVB0826-MA-TELETEXT      PIC  X(014).
110308         10 PO-DRVB0826-MA-AUTOTELEFON   PIC  X(026).
110308         10 PO-DRVB0826-MA-TELETEXT      PIC  X(026).
               10 PO-DRVB0826-MA-ZV-NR         PIC  X(001).
               10 PO-DRVB0826-MA-ZV-DAT-BIS-R  PIC  X(010).
               10 PO-DRVB0826-MA-ZV-DAT-BIS    PIC  X(010).
               10 PO-DRVB0826-MA-ZV-DAT-VON    PIC  X(010).
               10 PO-DRVB0826-MA-ZV-KUE-DAT    PIC  X(010).
               10 PO-DRVB0826-MA-ED-ZUSATZ     PIC S9(004) COMP.
               10 PO-DRVB0826-MA-ZV-VERL       PIC  X(001).
               10 PO-DRVB0826-MA-FG-DAT        PIC  X(010).
               10 PO-DRVB0826-MA-127-DAT       PIC  X(010).
               10 PO-DRVB0826-MA-VA-DAT        PIC  X(010).
               10 PO-DRVB0826-MA-ANWERBER      PIC S9(009) COMP.
               10 PO-DRVB0826-MA-BEFBED-NEU    PIC  X(010).
               10 PO-DRVB0826-MA-STEUER-NR     PIC  X(015).
      ****
      **** ENDE COPY DRVB0826 ****************************************
      ****
