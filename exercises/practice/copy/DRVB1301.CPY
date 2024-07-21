      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1301.CPv  $
      *
      *   Rev 2.1   Jan 07 2021 11:52:00   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.0   Nov 24 2020 12:41:44   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1301                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS JOIN-OZP ZJVB0040                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    24.11.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-532 !24.11.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1301 - PRÜFEN SEMINAR PLUS ERSATZ-QUALIFIKAT. *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1301.
          05 PI-DRVB1301-DATEN.
             10 PI-DRVB1301-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1301-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PI-DRVB1301-KNR-SEMZ-TYP       PIC S9(04)       BINARY.
 
       01 PO-DRVB1301.
         03 PO-DRVB1301-RC                     PIC  X(03).
             88 PO-DRVB1301-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1301-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1301-DATEN.
          05 PO-DRVB1301-SEMINAR-BESUCH        PIC  X(01).
             88 PO-DRVB1301-SEMINAR-BESUCH-JA       VALUE 'J'.
             88 PO-DRVB1301-SEMINAR-BESUCH-NO       VALUE 'N'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1301   --------------------------*
      *---------------------------------------------------------------*

