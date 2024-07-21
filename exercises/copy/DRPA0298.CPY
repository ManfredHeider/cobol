      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0298.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0298.CPv  $
      *
      *   Rev 2.5   Dec 10 2007 08:48:20   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.4   Sep 08 2003 13:49:44   t13ex21
      *APA00248 - Neue Funktion DRPA1543
      *
      *   Rev 2.3   12 Sep 2001 14:08:46   t13ae46
      *APA00144:Neue Funktion
      *
      *   Rev 2.2   Apr 11 2000 16:42:08   T13EX23
      *APA00093 - PDTA-ID über Tabelle PDTA
      *ermitteln
      *
      *   Rev 2.1   Nov 25 1999 09:15:08   T13EX21
      *APA00063 - Neue Funktion DRPA0605
      *
      *   Rev 2.0   Feb 27 1999 14:28:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:48:28   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:32:22   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0298                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWPA0017                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.06.97! 01 ! NEU                       ! WOLFF    *
      * APA00063!06.10.99!    ! NEUE FUNKTION             ! STORCH   *
      * APA00093!05.04.00! 2.2! NEUE FUNKTION - DRPA1366  ! GRAY     *
      * APA00144!10.08.01! 2.3! NEUE FUNKTION - DRPA0229  ! WANG     *
      * APA00248!05.09.03! 2.4! NEUE FUNKTION - DRPA1563  ! STORCH   *
      * APA00622!07.08.07! 2.5! NEUE FUNKTION - DRPA1667  ! STORCH   *
      *         !        !    !                           !          *
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
       01  PI-ZWPA0017.
           05  PI-ZWPA0017-FUNKTION            PIC  X(008).
               88 PI-ZWPA0017-DRPA0299              VALUE 'DRPA0299'.
061099         88 PI-ZWPA0017-DRPA0605              VALUE 'DRPA0605'.
050400         88 PI-ZWPA0017-DRPA1366              VALUE 'DRPA1366'.
100801         88 PI-ZWPA0017-DRPA0229              VALUE 'DRPA0229'.
050903         88 PI-ZWPA0017-DRPA1563              VALUE 'DRPA1563'.
070807         88 PI-ZWPA0017-DRPA1667              VALUE 'DRPA1667'.
      *
      ******* ENDE COPY-MEMBER **************************************
