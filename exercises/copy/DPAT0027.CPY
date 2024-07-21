      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:16:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0027   BATCH ERR-ERROR-BEREICH AUSGEBEN   * 00020006
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: P. SCHMIDT                                       * 00040000
      *    DATUM:    25.01.1994                                       * 00050000
      ***************************************************************** 00070000
      *    VERSION   01   25.01.94                                   *  00080000
      ****************************************************************  00090000
      * AENDERUNGEN:                                                 *  00091000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00092000
      * -------------------------------------------------------------*  00093000
      * AFVB0067!25.01.94! 01 ! NEU                       ! SCHMIDT .*  00094005
      *         !        !    !                           !          *  00096000
      *         !        !    !                           !          *  00097000
      *         !        !    !                           !          *  00098000
      *         !        !    !                           !          *  00099000
      *                                                              *  00099100
      * ENDE-LOGBUCH                                                 *  00099200
      ****************************************************************  00099300
      *------------------------------------------------------------*    00099400
      *                                                                 00099500
      * UR01-FEHLER-DISPLAY                                        *    00099902
      *------------------------------------------------------------*    00100000
      * INPUT : ERR-ERROR-BEREICH (DRAT0004)                       *    00101000
      *                                                            *    00102000
      * OUTPUT: FEHLERMELDUNGEN MIT DISPLAY                        *    00103000
      *                                                            *    00104000
      * VERARBEITUNG:                                              *    00105000
      *   - AUFBEREITUNG ERR-ERROR-BEREICH MIT DISPLAY -           *    00106000
      *                                                            *    00107000
      *------------------------------------------------------------*    00108000
      *  ZUGEHÖRIGES WORKING-STORAGE COPY-MEMBER DWAT0027          *    00108105
      *------------------------------------------------------------*    00108301
       UR01-FEHLER-DISPLAY SECTION.                                     00109001
       UR01-ANF.                                                        00109100
      *                                                                 00109201
           DISPLAY D-ZEILE-01                                           00109301
                                                                        00109401
      ****** AUSGABE DES FEHLER-TYPS                                    00109501
                                                                        00109601
           EVALUATE TRUE                                                00109701
           WHEN ERR-FTYP-CICS                                           00109901
              MOVE 'CICS-FEHLER                             '           00110001
                                         TO D-ERR-FEHLER-TYP-TEXT       00110101
           WHEN ERR-FTYP-PROG                                           00110201
              MOVE 'LOGISCHER PROGRAMM FEHLER               '           00110301
                                         TO D-ERR-FEHLER-TYP-TEXT       00110401
           WHEN ERR-FTYP-DATEI                                          00110501
              MOVE 'DATEI-FEHLER                            '           00110601
                                         TO D-ERR-FEHLER-TYP-TEXT       00110701
           WHEN ERR-FTYP-MVS-SYSTEM                                     00110801
              MOVE 'MVS-BETRIEBSSYSTEM FEHLER               '           00110901
                                         TO D-ERR-FEHLER-TYP-TEXT       00111001
           WHEN ERR-FTYP-SQL                                            00111101
              MOVE 'SQL-FEHLER                              '           00111201
                                         TO D-ERR-FEHLER-TYP-TEXT       00111301
           WHEN ERR-FTYP-ROLLBACK                                       00111401
              MOVE 'SCHREIBMODUL-FEHLER - ROLLBACK          '           00111501
                                         TO D-ERR-FEHLER-TYP-TEXT       00111601
           WHEN OTHER                                                   00111701
              CONTINUE                                                  00111801
           END-EVALUATE                                                 00112001
      *                                                                 00112101
           DISPLAY D-ZEILE-02                                           00112301
      *                                                                 00112401
           MOVE K-PGM-NAME               TO D-ERR-PROGNAME              00112702
           MOVE ERR-UPRONAME             TO D-ERR-UPRONAME              00112801
           DISPLAY D-ZEILE-03                                           00112901
      *                                                                 00113001
           MOVE ERR-ORT-SEC              TO D-ERR-ORT-SEC               00113101
           MOVE ERR-ORT-LFD              TO D-ERR-ORT-LFD               00113201
           DISPLAY D-ZEILE-04                                           00113301
      *                                                                 00113401
           MOVE ERR-SQLCODE              TO D-ERR-SQLCODE               00113501
           MOVE ERR-SQLERRMC             TO D-ERR-MELD-TEXT             00114001
           MOVE ERR-CICS-RC              TO D-ERR-CICS-RC               00120001
           MOVE ERR-FILE-STATUS          TO D-ERR-FILE-STATUS           00130001
           DISPLAY D-ZEILE-05                                           00131001
      *                                                                 00131101
           DISPLAY D-ZEILE-06                                           00132001
           DISPLAY D-ZEILE-07                                           00133001
           DISPLAY D-ZEILE-08                                           00134001
           DISPLAY D-ZEILE-09                                           00135001
           DISPLAY D-ZEILE-10                                           00136004
           DISPLAY D-VAR-ZEILE01                                        00137004
           DISPLAY D-VAR-ZEILE02                                        00138004
           DISPLAY D-VAR-ZEILE03                                        00139004
           DISPLAY D-VAR-ZEILE04                                        00140004
           DISPLAY D-ZEILE-11                                           00150004
           DISPLAY D-ZEILE-12                                           00160004
      *                                                                 00170001
           .                                                            00333400
       UR01-EXIT.                                                       00333500
           EXIT.                                                        00333600
      *                                                                 00333700
      **** ENDE COPY-MEMBER  DPAT0027 (ERR-ERROR-BEREICH AUSGEBEN) **** 00333800
