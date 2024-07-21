      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0273.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0273.CPv  $
      *
      *   Rev 2.3   Sep 08 2011 15:53:32   t13ex43
      *AIS00070: PGM - Turnus in SLM und OLZZ
      *
      *   Rev 2.2   26 May 2000 14:10:36   T13AE27
      *neue Funktion DRVB0483
      *
      *   Rev 2.1   22 May 2000 14:08:16   T13AE27
      *neue Funktion für ZWVB0026 eingefügt
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:32:26   T13AE35
      *AIS00009 - Neuerstellung, Funktionen fuer ZWVB0026
      *
      *   Rev 1.0   23 Jul 1997 12:25:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0273                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0026                        *
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
      * ASL00053!08.05.00! 02 ! NEUE FUNKTION DRVB0479    ! P.DUERR  *
080811* AIS00070!08.08.10! 2.3! NEUE FUNKTION DRVB1025    ! KLEEMEIER*
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
       01  PI-ZWVB0026.
           05  PI-ZWVB0026-FUNKTION            PIC  X(008).
               88 PI-ZWVB0026-DRVB0274              VALUE 'DRVB0274'.
080500         88 PI-ZWVB0026-DRVB0479              VALUE 'DRVB0479'.
250500         88 PI-ZWVB0026-DRVB0483              VALUE 'DRVB0483'.
080811         88 PI-ZWVB0026-DRVB1025              VALUE 'DRVB1025'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
