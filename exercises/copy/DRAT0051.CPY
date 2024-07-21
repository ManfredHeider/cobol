      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0051.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0051.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:44:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER: DRAT0051   KNR-KONST. FUER ANSCHRIFT-GRUND    * 00002000
      *    --------------------------------------------------------   * 00003000
      *    ERSTELLT: SCHOELLHAMMER                                    * 00006000
      *    DATUM:    14.05.1993                                       * 00007000
      ***************************************************************** 00009000
      *    VERSION   01   16.09.93                                   *  00009101
      ****************************************************************  00009201
      * AENDERUNGEN:                                                 *  00009301
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009401
      * -------------------------------------------------------------*  00009501
      *         !16.09.93! 01 ! NEU                       ! SCHÖLLH  *  00009601
      *         !        !    !                           !          *  00009701
      *         !        !    !                           !          *  00009801
      *         !        !    !                           !          *  00009901
      *         !        !    !                           !          *  00010001
      *         !        !    !                           !          *  00010101
      *         !        !    !                           !          *  00010201
      *         !        !    !                           !          *  00010301
      *                                                              *  00010401
      * ENDE-LOGBUCH                                                 *  00010501
      ****************************************************************  00010601
      *                                                                 00011000
      *   SCHLÜSSELWERTE FÜR ANSCHRIFT-GRUND                            00020000
      *                                                                 00030000
       01  FILLER.                                                      00031000
           05  K-KNR-GUELTIGE-ANSCHR          PIC S9(004) COMP VALUE 00.00032000
           05  K-KNR-UNBEKANNT-VERZOGEN       PIC S9(004) COMP VALUE 01.00032200
           05  K-KNR-ANNAHME-VERWEIGERT       PIC S9(004) COMP VALUE 02.00032300
           05  K-KNR-ANSCHR-VERSTORBEN        PIC S9(004) COMP VALUE 03.00032400
           05  K-KNR-ANSCHR-KUENDIGUNG        PIC S9(004) COMP VALUE 04.00032500
           05  K-KNR-ANSCHR-BEENDET           PIC S9(004) COMP VALUE 05.00032600
      *                                                                 00040000
      ******** ENDE COPY-MEMBER DRAT0051  KNR-K. ANSCHR-GRUND ********* 00050000
