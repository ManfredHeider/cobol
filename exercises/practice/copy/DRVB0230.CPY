      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0230.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0230.cpv  $
      *
      *   Rev 2.1   Sep 17 1999 08:36:12   T13EX19
      *aki00048 - neue Fkt eingefuegt
      *
      *   Rev 2.0   Feb 27 1999 15:58:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jun 1998 15:45:24   T13EX30
      *AAS00049 - Neuerstellung fuer DELTA-Rueckkonvertierung
      *
      *   Rev 1.0   23 Jul 1997 12:19:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0230                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0019                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    02.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!02.06.98! 01 ! NEUERSTELLUNG             ! WALB     *
      * AKI00048!06.09.99!    ! neue fkt drvb0445         !zgur-vujic*
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
       01  PI-ZXVB0019.
           05  PI-ZXVB0019-FUNKTION            PIC  X(008).
               88 PI-ZXVB0019-DRVB0231              VALUE 'DRVB0231'.
               88 PI-ZXVB0019-DRVB0445              VALUE 'DRVB0445'.
      *
      ******* ENDE COPY-MEMBER DRVB0230 *****************************
