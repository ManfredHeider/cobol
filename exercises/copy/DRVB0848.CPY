      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0848.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0848.CPv  $
      *
      *   Rev 2.1   05 Jul 2007 16:01:40   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System: Datenbasis
      *
      *   Rev 2.0   19 Mar 2007 13:29:06   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    COPY-MEMBER: DRVB0848                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0106                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *                                                               *
      *    REGISTRIERUNGSTABELLE                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R.GUTFLEISCH                                     *
      *    DATUM:    19.03.2007                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB00906!19.03.07! 2.1! NEU                       !GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0848                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB0848.
          05 PI-DRVB0848-MA-ST-NR           PIC S9(009).
      *
       01 PO-DRVB0848.

          05 PO-DRVB0848-RC                 PIC  X(003).
             88 PO-DRVB0848-RC-OK                VALUE 'OK '.
             88 PO-DRVB0848-RC-LEER              VALUE 'LER'.
             88 PO-DRVB0848-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB0848-RC-NOT-FOUND         VALUE 'NFD'.

           05  PD-NI-VIVB0106.
             10  PO-DRVB0848-NI-REG-HAFT-AB     PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-NR          PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-NAME        PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-VORNAME     PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-PLZ         PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-ORT         PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-STR         PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-GEBDAT      PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-REGISTER-TS PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-TAET-LAND   PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-CODE        PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-IHK-NR      PIC S9(4) USAGE COMP.
             10  PO-DRVB0848-NI-REG-CODE-TEXT   PIC S9(4) USAGE COMP.

          05 PO-DRVB0848-DATEN.
             10 PO-DRVB0848-MA-ST-NR          PIC S9(009).
             10 PO-DRVB0848-REG-LAST-UPDATE   PIC  X(026).
             10 PO-DRVB0848-REG-DAT-VON       PIC  X(026).
             10 PO-DRVB0848-REG-HAFT-AB       PIC  X(010).
             10 PO-DRVB0848-REG-NR            PIC  X(030).
             10 PO-DRVB0848-REG-NAME          PIC  X(050).
             10 PO-DRVB0848-REG-VORNAME       PIC  X(050).
             10 PO-DRVB0848-REG-PLZ           PIC  X(010).
             10 PO-DRVB0848-REG-ORT           PIC  X(050).
             10 PO-DRVB0848-REG-STR           PIC  X(050).
             10 PO-DRVB0848-REG-GEBDAT        PIC  X(010).
             10 PO-DRVB0848-REG-AKTION        PIC  X(003).
             10 PO-DRVB0848-REG-STATUS        PIC  X(006).
             10 PO-DRVB0848-REG-REGISTER-TS   PIC  X(035).
             10 PO-DRVB0848-REG-TAET-LAND     PIC  X(060).
             10 PO-DRVB0848-REG-CODE          PIC S9(008) COMP.
             10 PO-DRVB0848-REG-ART           PIC  X(001).
             10 PO-DRVB0848-REG-IHK-NR        PIC S9(004) COMP.
             10 PO-DRVB0848-REG-BUCH-KZ       PIC  X(001).
             10 PO-DRVB0848-REG-CODE-TEXT     PIC  X(255).
      *
      ***** ENDE COPY-MEMBER DRVB0848 *****************************
