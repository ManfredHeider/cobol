      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0878.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0878.CPv  $
      *
      *   Rev 2.2   Jan 25 2017 12:30:36   t13ex57
      *AVB01438: Anpassung Sonderdialoge
      *
      *   Rev 2.1   14 Jan 2008 13:01:06   t13ex43
      *ASL00285: PGM - Auszahlung von Geldleistungen für Allfinanz
      *
      *   Rev 2.0   04 Dec 2007 14:14:04   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0878                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0029                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.12.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00285!04.12.07!2.1 ! NEU                       ! KLEEMEIER *
      * AVB01438!23.01.17!2.1 ! NEUE FKT FÜR SONDERDIALOG ! BOLL      *
      *         !        !    !                           !           *
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
       01  PI-ZJVB0029.
           05  PI-ZJVB0029-FUNKTION            PIC  X(008).
               88 PI-ZJVB0029-DRVB0879              VALUE 'DRVB0879'.
230117         88 PI-ZJVB0029-DRVB1159              VALUE 'DRVB1159'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *

