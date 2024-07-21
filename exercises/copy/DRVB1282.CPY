      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1282.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1282.CPv  $
      *
      *   Rev 2.1   Jun 17 2020 09:40:32   t13ex71
      *ARW00147: Offensive 2020 - Jetzt erst recht: Neukunden/aktiv
      *ierte Einvertragskunden - Neue Sonderprov. (VB-System)(+) - 
      *copy und PI/PO-Bereich aus GENERCBL (BPR)
      *
      *   Rev 2.0   Jun 09 2020 12:42:40   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1282                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0341                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
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
      *    PRO UPRO UEBERGEBEN WERDEN --> FUNKTIONEN-COPY             *
      *                                                               *
      *****************************************************************
 
       01  PI-ZWVB0341.
           05  PI-ZWVB0341-FUNKTION            PIC  X(008).
               88 PI-ZWVB0341-DRVB1283              VALUE 'DRVB1283'.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1282   --------------------------*
      *---------------------------------------------------------------*
