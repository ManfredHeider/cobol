      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0609.cpy  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\drpa0609.cpv  $
      *
      *   Rev 2.4   25 May 2000 17:36:30   T13EX44
      *NEUE FUNKTION DRPA1388
      *
      *   Rev 2.3   Jun 30 1999 12:39:14   T13AE18
      *APA00012
      *
      *   Rev 2.2   Jun 24 1999 16:56:48   T13EX21
      *APA00012 - Neue Funktion
      *
      *   Rev 2.1   Mar 16 1999 14:01:08   T01JM2S
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:31:46   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:54:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0609                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWPA0106                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !11.11.96!01.01! NEU                      ! JÄGER    *
      *         !        !     !                          !          *
      * DAISY   !10.02.98!01.02! FUNKTION LESEN MIT MAX.  ! PARENT   *
      *         !        !     ! VERSION HINZUGEFÜGT      !          *
      *         !        !     !                          !          *
      * APA00012!08.03.99!02.01! NEUE FUNKTION DRPA1189   ! WOLFF    *
      *         !        !     !                          !          *
      * APA00012!24.06.99!02.02! NEUE FUNKTION DRPA1270   ! STORCH   *
      *         !        !     !                          !          *
      * APA00012!30.06.99!02.02! NEUE FUNKTION DRPA1274   ! Bohlinger*
      *         !        !     !                          !          *
      * APA00090!25.05.00!02.04! NEUE FUNKTION DRPA1388   ! SCHEIDEL *
      *         !        !     !                          !          *
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
       01  PI-ZWPA0106.
           05  PI-ZWPA0106-FUNKTION            PIC  X(008).
               88 PI-ZWPA0106-DRPA0611              VALUE 'DRPA0611'.
               88 PI-ZWPA0106-DRPA0612              VALUE 'DRPA0612'.
               88 PI-ZWPA0106-DRPA0663              VALUE 'DRPA0663'.
               88 PI-ZWPA0106-DRPA0850              VALUE 'DRPA0850'.
080399         88 PI-ZWPA0106-DRPA1189              VALUE 'DRPA1189'.
240699         88 PI-ZWPA0106-DRPA1270              VALUE 'DRPA1270'.
300699         88 PI-ZWPA0106-DRPA1274              VALUE 'DRPA1274'.
250500         88 PI-ZWPA0106-DRPA1388              VALUE 'DRPA1388'.
      *
      ******* ENDE COPY-MEMBER **************************************
