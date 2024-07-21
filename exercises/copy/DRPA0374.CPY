      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0374.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0374.CPv  $
      *
      *   Rev 2.1   Jan 07 2011 11:29:40   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.0   Feb 27 1999 14:28:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:24:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Apr 09 1997 16:48:00   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0374                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS BUFFER-OZP ZYPA0005                    
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.07.97! 01 ! NEU                       ! D.WILHELM*
      * APA01129!07.01.11!2.1 ! NEUE FUNKTIONEN PRODFILTER! RAHNER   *
      *         !        !    !                           !          *
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
       01  PI-ZYPA0005.
           05  PI-ZYPA0005-FUNKTION            PIC  X(008).
               88 PI-ZYPA0005-DRPA0375              VALUE 'DRPA0375'.
               88 PI-ZYPA0005-DRPA0443              VALUE 'DRPA0443'.
               88 PI-ZYPA0005-DRPA0444              VALUE 'DRPA0444'.
070111         88 PI-ZYPA0005-DRPA1681              VALUE 'DRPA1681'.
070111         88 PI-ZYPA0005-DRPA1720              VALUE 'DRPA1720'.
070111         88 PI-ZYPA0005-DRPA1721              VALUE 'DRPA1721'.
      *
      ******* ENDE COPY-MEMBER **************************************
