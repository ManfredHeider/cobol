      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0039.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0039.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:17:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0039                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    RAHMEN: FEHLERAUFBEREITUNG                                 * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    22.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   4    14.10.95 10:15:58
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0144!22.08.94!    ! NEU                       ! SCHÖLLH. *  00140003
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00140103
      * FEVB0156!28.09.94!    ! SQLERRMC NACH D-.-SQLERRMC! SCHÖLLH. *  00141000
      *         !28.09.94! 02 ! PROD.ÜBERGABE             ! SCHÖLLH. *  00139300
      * FEVB0218!03.03.95!    ! D-ERR-ORT-SEC NICHT FÜLLEN! SCHÖLLH. *  00142000
      *         !10.03.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *  00070500
      * AFVB0226!11.09.95!    ! D-ERR-ORT-SEC DOCH FÜLLEN ! SCHÖLLH. *  00142000
      *         !14.10.95! 04 ! PROD.UEBERGABE            ! SCHÖLLH. *  00136001
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00144000
      *                                                              *  00145000
      * ENDE-LOGBUCH                                                 *  00146000
      ****************************************************************  00147000
      *                                                                 00147100
      *------------------------------------------------------------*    00147500
       UR81-ERR-AUFBER SECTION.                                         00147600
       UR81-ANF.                                                        00147700
      *------------------------------------------------------------*    00147800
      * INPUT : ERR-ERROR-BEREICH:                                 *    00147900
      *         EINGABE-WERTE - ABH. VON ERR-FEHLER-TYP:           *    00148000
      *         -------------------------------------------        *    00149000
      *         ERR-FEHLER-TYP ! 1(SQL) ! 3(PROG)!4(DATEI)!        *    00149100
      *         ---------------+--------+--------+--------+        *    00149200
      *         ERR-ORT        !  (X)   !  (X)   !  (X)   !        *    00149300
      *         ERR-MELD-KEY   !        !  (X)   !        !        *    00149400
      *         ERR-PROGNAME   !   X    !   X    !   X    !        *    00149500
      *         ERR-SQLCODE    !   X    !        !        !        *    00149600
      *         ERR-FILE-STATUS!        !        !   X    !        *    00149700
      *         ERR-VAR (OPT)  !  (X)   !  (X)   !  (X)   !        *    00149800
      *         -------------------------------------------        *    00149900
      *                                                            *    00150000
      *         EINGABE-WERTE - ABH. VON ERR-AUFBEREITUNG:         *    00150100
      *         -------------------------------------------------- *    00150200
      *         ERR-AUFBEREITUNG     !0(NORMAL)! 1(DB2S) !         *    00150300
      *         ---------------------+---------+---------+         *    00150400
      *         ERR-PARAMETER-STEUER !         !    X    !         *    00150500
      *         ERR-PARAMETER-RETURN !         !    X    !         *    00150600
      *         -------------------------------------------------- *    00150700
      *                                                            *    00150800
      * OUTPUT: D-ERROR-BEREICH MIT AUFBEREITETEN FEHLERINFOS      *    00150900
      *                                                            *    00151000
      * VERARBEITUNG:                                              *    00151100
      *   - AUFBEREITUNG DER ERROR-SCHNITTSTELLE ABHÄNGIG VON      *    00151200
      *     ERR-FEHLER-TYP UND ERR-AUFBEREITUNG                    *    00151300
      *------------------------------------------------------------*    00151500
                                                                        00151600
           INITIALIZE                    D-ERROR-BEREICH                00151701
                                                                        00151801
      *    * FEHLER-TYP                                                 00151900
                                                                        00152001
           EVALUATE TRUE                                                00154102
           WHEN ERR-FTYP-SQL                                            00154201
              MOVE 'SQL-FEHLER                              '           00154301
                                         TO D-ERR-FEHLER-TYP-TEXT       00154401
           WHEN ERR-FTYP-CICS                                           00154501
              MOVE 'CICS-FEHLER                             '           00154601
                                         TO D-ERR-FEHLER-TYP-TEXT       00154701
           WHEN ERR-FTYP-PROG                                           00154801
              MOVE 'LOGISCHER PROGRAMM-FEHLER               '           00154901
                                         TO D-ERR-FEHLER-TYP-TEXT       00155001
           WHEN ERR-FTYP-DATEI                                          00155101
              MOVE 'DATEI-FEHLER (VSAM)                     '           00155201
                                         TO D-ERR-FEHLER-TYP-TEXT       00155301
           WHEN ERR-FTYP-MVS-SYSTEM                                     00155401
              MOVE 'MVS-SYSTEM-FEHLER                       '           00155501
                                         TO D-ERR-FEHLER-TYP-TEXT       00155601
           WHEN ERR-FTYP-ROLLBACK                                       00155701
              MOVE 'ROLLBACK - SQL-FEHLER MIT INKONSISTENZEN'           00155801
                                         TO D-ERR-FEHLER-TYP-TEXT       00155901
           END-EVALUATE                                                 00156001
                                                                        00156100
      *    * STANDARD-FELDER                                            00157100
                                                                        00157200
           MOVE ERR-PROGNAME             TO D-ERR-PROGNAME              00157300
           MOVE ERR-UPRONAME             TO D-ERR-UPRONAME              00157400
030395*     MOVE ERR-ORT-SEC              TO D-ERR-ORT-SEC              00157500
110995     MOVE ERR-ORT-SEC              TO D-ERR-ORT-SEC               00157500
           MOVE ERR-ORT-LFD              TO D-ERR-ORT-LFD               00157600
           MOVE ERR-SQLCODE              TO D-ERR-SQLCODE               00158000
           MOVE ERR-CICS-RC              TO D-ERR-CICS-RC               00158200
           MOVE ERR-FILE-STATUS          TO D-ERR-FILE-STATUS           00158300
280994*     MOVE ERR-SQLERRMC             TO D-ERR-MELD-TEXT            00158100
280994     MOVE ERR-SQLERRMC             TO D-ERR-SQLERRMC              00158100
           MOVE ERR-TRACE                TO D-ERR-TRACE                 00158400
           MOVE ERR-VAR                  TO D-ERR-VAR                   00158500
                                                                        00158600
      *    * BEHANDLUNG VON ERR-AUFBEREITUNG                            00158700
      *    * (NUR DIE FÄLLE, DIE NICHTS MIT CICS ZU TUN HABEN)          00158800
                                                                        00158900
           EVALUATE TRUE                                                00159000
                                                                        00159100
           WHEN ERR-AUFBER-DB2S                                         00159200
                                                                        00159300
              MOVE ERR-PARAMETER-STEUER  TO PARAMETER-STEUER            00159400
              MOVE ERR-PARAMETER-RETURN  TO PARAMETER-RETURN            00159500
                                                                        00159600
              MOVE PS-PROGRAMM-ID        TO D-PS-PGM-ID                 00159700
              MOVE PS-PROGRAMM-KZ        TO D-PS-PGM-KZ                 00159800
              MOVE PS-USER-ID            TO D-PS-USER-ID                00159900
              MOVE PS-TERMINAL-ID        TO D-PS-TERM-ID                00160000
              MOVE PS-DATE-AEND          TO D-PS-DATE-AE                00160100
              MOVE PS-DATE-VON           TO D-PS-DATE-VON               00160200
              MOVE PS-AEND-KZ            TO D-PS-AEND-KZ                00160300
                                                                        00160400
              MOVE PR-RETURN-CODE        TO D-PR-CODE                   00160500
              MOVE PR-FEHLER-TYP         TO D-PR-TYP                    00160600
              MOVE PR-PROGRAMM-ID        TO D-PR-PGM-ID                 00160700
              MOVE PR-FEHLER-NR          TO D-PR-F-NR                   00160800
              MOVE PR-FEHLER-TEXT        TO D-PR-F-TEXT                 00160900
                                                                        00161000
              MOVE D-DB2S                TO D-ERR-VAR-RAHMEN            00161100
                                                                        00161200
           WHEN OTHER                                                   00161300
              CONTINUE                                                  00161400
           END-EVALUATE                                                 00161500
                                                                        00161600
           .                                                            00161700
       UR81-EXIT.                                                       00161800
           EXIT.                                                        00162000
      *                                                                 00170000
      ******** ENDE COPY-MEMBER (DPAT0039) *******************          00207400
                                                                        00207500
