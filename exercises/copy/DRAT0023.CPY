      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:38:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001001
      *    COPY-MEMBER: DRAT0023  KONSTANTEN FÜR ERROR UND TRACE      * 00002001
      *    --------------------------------------------------------   * 00003001
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00004001
      *    DATUM:    02.03.1993                                       * 00005001
      ***************************************************************** 00006001
      *    VERSION   1    10.03.95 15:57:56
      ****************************************************************  00008001
      * AENDERUNGEN:                                                 *  00009001
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009101
      * -------------------------------------------------------------*  00009201
      *         !05.04.93! 01 ! NEU                       ! SCHÖLLH. *  00009301
      * FEVB0218!03.03.95!    ! KONSTANTEN FÜR INFO-DISPLAY SCHÖLLH. *  00010401
      *         !10.03.95! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00070500
      *         !        !    !                           !          *  00010501
      *         !        !    !                           !          *  00010501
      *         !        !    !                           !          *  00010501
      *         !        !    !                           !          *  00010501
      *                                                              *  00010601
      * ENDE-LOGBUCH                                                 *  00010701
      ****************************************************************  00010801
      *                                                                 00100000
       01  K-ERROR-KONSTANTEN.                                          00110000
                                                                        00120000
      *   DEFINITIONEN FUER ALLGEMEINE FEHLERMELDUNGEN                  00130000
           05  K-SF-PGM-NAME         PIC  X(008) VALUE 'PGAT9999'.      00140000
                                                                        00150000
      *   LAENGE D-ERROR-BEREICH (DRUCKAUFBEREITETE BILDSCHIRMAUSGABE)  00160000
           05  K-D-ERROR-LEN         PIC S9(004) COMP VALUE 1920.       00170000
                                                                        00180000
      *   LAENGE ERR-VAR-RAHMEN (DRUCKAUFBEREITETE BILDSCHIRMAUSGABE)   00181000
           05  K-ERR-VAR-RAHMEN-LEN  PIC S9(004) COMP VALUE 0320.       00182000
                                                                        00183000
      *   LAENGE PI-UPAT0005-BEREICH                                    00190000
           05  K-PI-UPAT0005-LEN     PIC S9(009) COMP VALUE  100.       00200000
                                                                        00210000
      *   DEFINITIONEN FUER TRACE-TRANSAKTIONEN                         00212000
           05  K-TRANS-TRACE-AN      PIC  X(004) VALUE 'TRC1'.          00213000
           05  K-TRANS-TRACE-AUS     PIC  X(004) VALUE 'TRC0'.          00213100
           05  K-TRANS-TRACE-KURZ    PIC  X(004) VALUE 'TRCK'.          00213200
           05  K-TRANS-TRACE-LANG    PIC  X(004) VALUE 'TRCL'.          00213300
                                                                        00229100
      *   DEFINITIONEN FUER INFO-TRANSAKTIONEN                          00229200
030395     05  K-STRICH-ZEILE       PIC  X(79)  VALUE ALL '-'.          00229500
                                                                        00230000
      ******** ENDE COPY-MEMBER KONST. ERROR UND TRACE (DRAT0023)  **** 00330000
