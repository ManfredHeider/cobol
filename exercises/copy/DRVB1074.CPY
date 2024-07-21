      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1074.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1074.CPv  $
      *
      *   Rev 2.1   Feb 26 2015 15:40:06   t13ae35
      *ARW00102: Ausbilderzuschuß monatlich automatisch verbuchen -
      * Zugriffsmodule
      *
      *   Rev 2.0   Jan 29 2015 16:11:20   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1074                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0070                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    26.02.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ARW00102!26.02.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *                                                               *
      *****************************************************************
 
       01  PI-ZWVB0070.
           05  PI-ZWVB0070-FUNKTION            PIC  X(008).
               88 PI-ZWVB0070-DRVB1075              VALUE 'DRVB1075'.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1074   --------------------------*
      *---------------------------------------------------------------*

