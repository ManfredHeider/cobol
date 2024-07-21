      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1214.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1214.CPv  $
      *
      *   Rev 2.2   19 Nov 2019 08:33:26   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.1   Aug 27 2019 07:54:48   t13ae56
      *ASL00584: SL15 - Pflege Tabelle VHV
      *
      *   Rev 2.0   Jun 03 2019 13:19:50   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1214                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0231                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00584!19.06.19!2.1 ! NEU                       ! KLEEMEIER *
111119* ASL00584!11.11.19!2.2 ! VHV-ANZAHL HINZU          ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1214 - ERMITTELN SATZ AUS TABELLE VHV         *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1214.
          05 PI-DRVB1214-DATEN.
             10 PI-DRVB1214-VHV-ST-NR          PIC S9(09)       BINARY.
             10 PI-DRVB1214-KNR-VERSICH-TYP    PIC S9(04)       BINARY.
 
       01 PO-DRVB1214.
         03 PO-DRVB1214-RC                     PIC  X(03).
             88 PO-DRVB1214-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1214-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1214-DATEN.
          05 PO-DRVB1214-NI-VIVB0231.
             10 NI-DRVB1214-VHV-ST-NR          PIC S9(04)       BINARY.
             10 NI-DRVB1214-KNR-VERSICH-TYP    PIC S9(04)       BINARY.
             10 NI-DRVB1214-KTY-VERSICH-TYP    PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-BEGINN-DAT     PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-LAST-UPD       PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-ENDE-DAT       PIC S9(04)       BINARY.
             10 NI-DRVB1214-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1214-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-VER-BETRAG     PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-VER-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-ERST-BETRAG    PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-ERST-DAT       PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-INFO           PIC S9(04)       BINARY.
             10 NI-DRVB1214-VHV-EINZ-VT        PIC S9(04)       BINARY.
111119       10 NI-DRVB1214-VHV-ANZAHL         PIC S9(04)       BINARY.
          05 PO-DRVB1214-DCL-VIVB0231.
             10 PO-DRVB1214-VHV-ST-NR          PIC S9(09)       BINARY.
             10 PO-DRVB1214-KNR-VERSICH-TYP    PIC S9(04)       BINARY.
             10 PO-DRVB1214-KTY-VERSICH-TYP    PIC S9(04)       BINARY.
             10 PO-DRVB1214-VHV-BEGINN-DAT     PIC  X(10).
             10 PO-DRVB1214-VHV-LAST-UPD       PIC  X(26).
             10 PO-DRVB1214-VHV-ENDE-DAT       PIC  X(10).
             10 PO-DRVB1214-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1214-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1214-VHV-VER-BETRAG     PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1214-VHV-VER-NR         PIC S9(15)       COMP-3.
             10 PO-DRVB1214-VHV-ERST-BETRAG    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1214-VHV-ERST-DAT       PIC  X(10).
             10 PO-DRVB1214-VHV-INFO           PIC  X(100).
             10 PO-DRVB1214-VHV-EINZ-VT        PIC  X(50).
111119       10 PO-DRVB1214-VHV-ANZAHL         PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1214   --------------------------*
      *---------------------------------------------------------------*

