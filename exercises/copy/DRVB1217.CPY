      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1217.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1217.CPv  $
      *
      *   Rev 2.1   Jun 28 2019 12:30:16   t13ae56
      *AVB01570: PGM - Gutscheincode
      *
      *   Rev 2.0   Jun 05 2019 16:29:26   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1217                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0194                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.06.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-998 !07.06.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1217 - GUTSCHEINCODE-GENERIERUNG SURFACE      *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1217.
          05 PI-DRVB1217-DATEN.
             10 PI-DRVB1217-GUTSC-ST-NR        PIC S9(09)       BINARY.
 
       01 PO-DRVB1217.
          05 PO-DRVB1217-RC                    PIC  X(03).
             88 PO-DRVB1217-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1217-RC-NOK                  VALUE 'NOK'.
 
          05 PO-DRVB1217-DATEN.
             10 PO-DRVB1217-GUTSC-CODE         PIC  X(20).
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1217   --------------------------*
      *---------------------------------------------------------------*
