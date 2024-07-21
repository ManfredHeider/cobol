      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0750.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0750.CPv  $
      *
      *   Rev 2.2   Jun 21 2004 16:39:32   t13ae34
      *Werkzeug für Auswertungen im eingereichten Geschäft
      *
      *   Rev 2.1   Jan 04 2000 13:09:12   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0750                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0004                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                        *
      *    DATUM:    15.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!15.11.99!    ! NEU                       !BOHLINGER *
      * AAN00492!21.05.05!2.2 ! DRPA1575                  !HEIDER    *
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
       01  PI-ZXPA0004.
           05  PI-ZXPA0004-FUNKTION            PIC  X(008).
               88 PI-ZXPA0004-DRPA0751              VALUE 'DRPA0751'.
               88 PI-ZXPA0004-DRPA1575              VALUE 'DRPA1575'.
      *
      ******* ENDE COPY-MEMBER **************************************
