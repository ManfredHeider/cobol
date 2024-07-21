      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0654.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0654.CPv  $
      *
      *   Rev 2.6   Mar 17 2020 08:34:50   T13ae37
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - PVW
      *
      *   Rev 2.5   Jan 10 2014 12:52:10   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.4   Jun 24 1999 16:56:52   T13EX21
      *APA00012 - Neue Funktion
      *
      *   Rev 2.3   Jun 19 1999 11:14:22   T13EX22
      *CRPA0069 umgesetzt
      *
      *   Rev 2.2   Apr 13 1999 10:28:04   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B
      *
      *   Rev 2.1   Mar 16 1999 14:02:02   T01JM2S
      *
      *
      *   Rev 2.0   Feb 27 1999 14:29:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:44:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:57:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0654  FUNKTIONSAUFRUFE FÜR ZWPA0113      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      * APA00012!08.03.99!2.1 ! NEUE FUNKTION DRPA1186    ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!        !2.2 ! CRPA0032/1.5.1.           !          *
      *         !30.03.99!    !  - NEUE FUNKTION DRPA1208 ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!14.06.99!2.3 ! NEUE FUNKTION DRPA1263    ! MUND     *
      *         !        !    !                           !          *
      * APA00012!24.06.99!2.4 ! NEUE FUNKTION DRPA1272    ! STORCH   *
      * APA01526!26.11.13!2.5 ! NEUE FUNKTION DRPA1775    ! BÖTTCHER *
      * APA02884!21.11.19!2.6 ! NEUE FUNKTION DRPA1942    ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-ZWPA0113.
           05  PI-ZWPA0113-FUNKTION            PIC  X(008).
               88 PI-ZWPA0113-DRPA0657              VALUE 'DRPA0657'.
               88 PI-ZWPA0113-DRPA0658              VALUE 'DRPA0658'.
               88 PI-ZWPA0113-DRPA0659              VALUE 'DRPA0659'.
080399         88 PI-ZWPA0113-DRPA1186              VALUE 'DRPA1186'.
300399         88 PI-ZWPA0113-DRPA1208              VALUE 'DRPA1208'.
140699         88 PI-ZWPA0113-DRPA1263              VALUE 'DRPA1263'.
240699         88 PI-ZWPA0113-DRPA1272              VALUE 'DRPA1272'.
261113         88 PI-ZWPA0113-DRPA1775              VALUE 'DRPA1775'.
211119         88 PI-ZWPA0113-DRPA1942              VALUE 'DRPA1942'.
      *

