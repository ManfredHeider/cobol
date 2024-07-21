      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0779.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0779.CPv  $
      *
      *   Rev 2.3   Nov 27 2018 07:22:20   t13ex67
      *APA02721: DAISY-Dialoganpassungen: "Kennung" (bei Ges. 21) i
      *m PO23  als Auswahlfeld
      *
      *   Rev 2.2   Jan 13 2014 09:48:26   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PO23, PO21, PF14, PF15
      *
      *   Rev 2.1   12 Sep 2001 14:18:10   t13ae46
      *APA00144: Neue Funktion DRPA0578
      *
      *   Rev 2.0   Apr 30 1999 15:34:06   T13EX21
      *APA00012 - Neue Funktion DRPA1220
      *
      *   Rev 1.0   12 Dec 1997 09:47:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0779                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0102                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. JÄGER-SOKOLUK                                 *
      *    DATUM:    06.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.01.98!01.01! NEU                      ! J. JÄGER *
      * AFPA0012!22.07.98!01.02! NEUE FUNKTION DRPA1024   !BOHLINGER *
      * AFPA0012!22.04.99!01.03! NEUE FUNKTION DRPA1220   ! STORCH   *
      * APA00144!13.08.01! 2.1 ! NEUE FUNKTION DRPA0578   ! WANG     *
      * APPA1526!09.12.13! 2.2 ! NEUE FUNKTIONEN          ! STORCH   *
      * APA02721!20.11.18! 2.3 ! NEUE FUNKTION            ! STORCH   *
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
       01  PI-ZXPA0102.
           05  PI-ZXPA0102-FUNKTION            PIC  X(008).
               88 PI-ZXPA0102-DRPA0780              VALUE 'DRPA0780'.
               88 PI-ZXPA0102-DRPA1024              VALUE 'DRPA1024'.
               88 PI-ZXPA0102-DRPA1220              VALUE 'DRPA1220'.
               88 PI-ZXPA0102-DRPA0578              VALUE 'DRPA0578'.
091213         88 PI-ZXPA0102-DRPA1779              VALUE 'DRPA1779'.
091213         88 PI-ZXPA0102-DRPA1780              VALUE 'DRPA1780'.
091213         88 PI-ZXPA0102-DRPA1781              VALUE 'DRPA1781'.
201118         88 PI-ZXPA0102-DRPA1913              VALUE 'DRPA1913'.
      *
      ******* ENDE COPY-MEMBER **************************************


