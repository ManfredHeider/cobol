      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0937.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0937.CPv  $
      *
      *   Rev 2.2   Feb 01 2011 16:50:22   t13ex13
      *ASL00367: SL-Dialog für Leistungszuschuss Allfinanz
      *
      *   Rev 2.1   May 27 2009 14:16:36   t13ex43
      *AAS00815: PGM - Neukundenbonus
      *
      *   Rev 2.0   May 06 2009 14:52:46   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0937                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0305                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    15.05.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00815!15.05.09!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00367!31.01.11!2.2 ! WEITERE FUNKTION          !GUTFLEISCH *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZWVB0305.
           05  PI-ZWVB0305-FUNKTION            PIC  X(008).
               88 PI-ZWVB0305-DRVB0938              VALUE 'DRVB0938'.
               88 PI-ZWVB0305-DRVB1005              VALUE 'DRVB1005'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *
