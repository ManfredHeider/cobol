      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1221.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1221.CPv  $
      *
      *   Rev 2.1   Jun 28 2019 12:36:00   t13ae56
      *AVB01570: PGM - Gutscheincode
      *
      *   Rev 2.0   Jun 24 2019 08:45:24   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1221                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0195                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    24.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-998 !24.06.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1221 - PRÜFEN ANSPRUCH SURFACE                *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1221.
          05 PI-DRVB1221-DATEN.
             10 PI-DRVB1221-MA-ST-NR           PIC S9(09)       BINARY.
 
       01 PO-DRVB1221.
          05 PO-DRVB1221-RC                    PIC  X(03).
             88 PO-DRVB1221-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1221-RC-NOK                  VALUE 'NOK'.
 
          05 PO-DRVB1221-DATEN.
             10 PO-DRVB1221-ANSPRUCH           PIC  X(01).
                88 PO-DRVB1221-ANSPR-JA             VALUE 'J'.
                88 PO-DRVB1221-ANSPR-NEIN           VALUE 'N'.
      *
             10 PO-DRVB1221-PRUEF-ERG-1        PIC  X(05).
             10 FILLER REDEFINES PO-DRVB1221-PRUEF-ERG-1.
                15 PO-DRVB1221-PRUEF-SLZ-43    PIC  X(01).
                15 PO-DRVB1221-PRUEF-SLZ-49    PIC  X(01).
                15 PO-DRVB1221-PRUEF-SLZ-72    PIC  X(01).
                15 PO-DRVB1221-PRUEF-SLZ-74    PIC  X(01).
                15 PO-DRVB1221-PRUEF-SLZ-123   PIC  X(01).
      *
             10 PO-DRVB1221-PRUEF-ERG-2        PIC  X(02).
             10 FILLER REDEFINES PO-DRVB1221-PRUEF-ERG-2.
                15 PO-DRVB1221-PRUEF-SLZ-42    PIC  X(01).
                15 PO-DRVB1221-PRUEF-SLZ-122   PIC  X(01).
      *
             10 PO-DRVB1221-PRUEF-ERG-3        PIC  X(05).
             10 FILLER REDEFINES PO-DRVB1221-PRUEF-ERG-3.
                15 PO-DRVB1221-PRUEF-BES-124   PIC  X(01).
                15 PO-DRVB1221-PRUEF-BES-126   PIC  X(01).
                15 PO-DRVB1221-PRUEF-BES-138   PIC  X(01).
                15 PO-DRVB1221-PRUEF-BES-144   PIC  X(01).
                15 PO-DRVB1221-PRUEF-BES-188   PIC  X(01).
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1221   --------------------------*
      *---------------------------------------------------------------*
