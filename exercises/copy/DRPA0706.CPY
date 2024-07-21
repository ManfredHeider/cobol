      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0706.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0706.CPv  $
      *
      *   Rev 2.5   Aug 26 2019 12:02:58   t13ex67
      *APA02843: WEPA0008
      *
      *   Rev 2.4   Sep 02 1999 11:40:18   T13EX21
      *FPA00070 - Neue Funktion DRPA0004
      *
      *   Rev 2.3   Jun 15 1999 11:49:10   T13EX21
      *APA00012 - Erweiterungen
      *
      *   Rev 2.2   May 21 1999 13:05:10   T13EX26
      *PNAP Änderungen u.a neue Funktion DRPA1240
      *
      *   Rev 2.1   Mar 23 1999 10:07:06   T13EX21
      *APA00012 - MFE-Umstellung
      *
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:41:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0706                                      *
      *    --------------------------------------------------------   *
      *    Funktionen für das List-OZP ZXPA0105                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    12.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01! NEU                       ! Jäger   *
      * AFPA0012!25.07.98!01.02! NEUE FUNKTIONEN           !SCHRIEBER*
      * AFPA0012!10.09.98!01.03! NEUE FUNKTIONEN           !SCHRIEBER*
      *         !        !     !                           !         *
      * APA00012!20.05.99! 2.0 ! FUNKTION DRPA1240 NEU     ! PARENT  *
      * FPA00070!01.09.99! 2.1 ! NEUE FUNKTIONEN DRPA0004  ! STORCH  *
      * APA02843!20.08.19! 2.5 ! NEUE FUNKTION DRPA1926    ! STORCH  *
      *         !        !     !                           !         *
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
       01  PI-ZXPA0105.
           05  PI-ZXPA0105-FUNKTION            PIC  X(008).
               88 PI-ZXPA0105-DRPA0703              VALUE 'DRPA0703'.
               88 PI-ZXPA0105-DRPA0704              VALUE 'DRPA0704'.
               88 PI-ZXPA0105-DRPA0705              VALUE 'DRPA0705'.
               88 PI-ZXPA0105-DRPA1039              VALUE 'DRPA1039'.
               88 PI-ZXPA0105-DRPA1045              VALUE 'DRPA1045'.
               88 PI-ZXPA0105-DRPA1046              VALUE 'DRPA1046'.
               88 PI-ZXPA0105-DRPA1050              VALUE 'DRPA1050'.
               88 PI-ZXPA0105-DRPA1061              VALUE 'DRPA1061'.
               88 PI-ZXPA0105-DRPA1062              VALUE 'DRPA1062'.
               88 PI-ZXPA0105-DRPA1063              VALUE 'DRPA1063'.
               88 PI-ZXPA0105-DRPA1068              VALUE 'DRPA1068'.
               88 PI-ZXPA0105-DRPA1240              VALUE 'DRPA1240'.
               88 PI-ZXPA0105-DRPA0004              VALUE 'DRPA0004'.
               88 PI-ZXPA0105-DRPA1926              VALUE 'DRPA1926'.
      *

