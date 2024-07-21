      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0271.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0271.CPv  $
      *
      *   Rev 2.3   Sep 08 2011 15:53:20   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.2   16 Jun 2000 15:00:06   T13AE27
      *neue Funktion DRVB0487
      *
      *   Rev 2.1   22 May 2000 14:06:32   T13AE27
      *neue Funktion für ZWVB0064
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:30:56   T13AE35
      *AIS00009 - Neuerstellung, Funktionen fuer ZWVB0064
      *
      *   Rev 1.0   23 Jul 1997 12:24:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0271                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0064                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HERING                                           *
      *    DATUM:    17.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00009!17.06.98! 01 ! NEU                       ! HERING   *
      * ASL00053!08.05.00! 02 ! NEUE FUNKTION DRVB0480    ! P.DUERR  *
      * ASL00061!15.06.00! 03 ! NEUE FUNKTION DRVB0487    ! P.DUERR  *
080811* AIS00070!08.08.10! 2.3! NEUE FUNKTION DRVB1026    ! KLEEMEIER*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0064.
           05  PI-ZWVB0064-FUNKTION            PIC  X(008).
               88 PI-ZWVB0064-DRVB0272              VALUE 'DRVB0272'.
080500         88 PI-ZWVB0064-DRVB0480              VALUE 'DRVB0480'.
150600         88 PI-ZWVB0064-DRVB0487              VALUE 'DRVB0487'.
080811         88 PI-ZWVB0064-DRVB1026              VALUE 'DRVB1026'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
