      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1283.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1283.CPv  $
      *
      *   Rev 2.1   Jun 17 2020 09:40:52   t13ex71
      *ARW00147: Offensive 2020 - Jetzt erst recht: Neukunden/aktiv
      *ierte Einvertragskunden - Neue Sonderprov. (VB-System)(+) - 
      *copy und PI/PO-Bereich aus GENERCBL (BPR)
      *
      *   Rev 2.0   Jun 09 2020 12:42:56   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1283                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0341                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    09.06.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-712 !09.06.20!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1283 - ERMITTELN SATZ AUS TABELLE OFAUS       *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1283.
          05 PI-DRVB1283-DATEN.
             10 PI-DRVB1283-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1283-OFAUS-ANSPR-VOM    PIC  X(10).
             10 PI-DRVB1283-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
 
       01 PO-DRVB1283.
         03 PO-DRVB1283-RC                     PIC  X(03).
             88 PO-DRVB1283-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1283-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1283-DATEN.
          05 PO-DRVB1283-NI-VIVB0341.
             10 NI-DRVB1283-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1283-OFAUS-ANSPR-VOM    PIC S9(04)       BINARY.
             10 NI-DRVB1283-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1283-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 NI-DRVB1283-OFAUS-LAST-UPDATE  PIC S9(04)       BINARY.
             10 NI-DRVB1283-OFAUS-ANSPR-BETRAG PIC S9(04)       BINARY.
             10 NI-DRVB1283-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 NI-DRVB1283-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
          05 PO-DRVB1283-DCL-VIVB0341.
             10 PO-DRVB1283-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1283-OFAUS-ANSPR-VOM    PIC  X(10).
             10 PO-DRVB1283-KNR-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1283-KTY-ANSPR-GRUND    PIC S9(04)       BINARY.
             10 PO-DRVB1283-OFAUS-LAST-UPDATE  PIC  X(26).
             10 PO-DRVB1283-OFAUS-ANSPR-BETRAG PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1283-KTY-ANSPR-STATUS   PIC S9(04)       BINARY.
             10 PO-DRVB1283-KNR-ANSPR-STATUS   PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1283   --------------------------*
      *---------------------------------------------------------------*
