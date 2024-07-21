      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1178.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB1178.CPv  $
      *
      *   Rev 2.4   Apr 03 2020 11:23:26   t13ae56
      *AVB01574: PGM - Meldung im VB23, wenn VB einer NFR gekündigt
      * hat
      *
      *   Rev 2.3   Dec 03 2018 10:58:02   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriffsmodul Tabellen TBVB0126
      *
      *   Rev 2.2   Aug 16 2018 12:53:30   t13ex18
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.1   02 Aug 2018 16:24:44   t13ae13
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.0   Dec 04 2017 09:52:52   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1178                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0126                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!04.12.17!2.1 ! NEU                       ! WALB      *
300320* VLA-430 !30.03.20!2.4 ! DRVB1280 HINZU            ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *                                                               *
      *****************************************************************
 
       01  PI-ZWVB0126.
           05  PI-ZWVB0126-FUNKTION            PIC  X(008).
               88 PI-ZWVB0126-DRVB1179              VALUE 'DRVB1179'.
               88 PI-ZWVB0126-DRVB1202              VALUE 'DRVB1202'.
300320         88 PI-ZWVB0126-DRVB1280              VALUE 'DRVB1280'.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1178   --------------------------*
      *---------------------------------------------------------------*
