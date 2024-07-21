      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0026.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0026.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:16:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0026   H-MELDUNGEN MERKEN UND AUSGEBEN    * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00040000
      *    DATUM:    17.05.1993                                       * 00050000
      ***************************************************************** 00070000
      *    VERSION   02   27.09.93                                   *  00080001
      ****************************************************************  00090000
      * AENDERUNGEN:                                                 *  00091000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00092000
      * -------------------------------------------------------------*  00093000
      *         !16.09.93! 01 ! NEU                       ! SCHOELLH.*  00094000
      *         !21.09.93!    ! INIT. I-HM                ! SCHOELLH.*  00095000
      *         !27.09.93! 02 ! PROD.ÜBERGABE             ! HECKER   *  00095101
      *         !        !    !                           !          *  00096000
      *         !        !    !                           !          *  00097000
      *         !        !    !                           !          *  00098000
      *         !        !    !                           !          *  00099000
      *                                                              *  00099100
      * ENDE-LOGBUCH                                                 *  00099200
      ****************************************************************  00099300
      *                                                                 00099500
      *    EINSATZ: VGL. DWAT0026 !!!                                   00099600
      *                                                                 00099700
      *------------------------------------------------------------*    00099800
      * UA1-H-MELDUNGEN-INI                                        *    00099900
      *------------------------------------------------------------*    00100000
      * INPUT : -                                                  *    00101000
      *                                                            *    00102000
      * OUTPUT: CADI-HMELD-INFO-TAB (LEER)                         *    00103000
      *                                                            *    00104000
      * VERARBEITUNG:                                              *    00105000
      *   - INITIALISIEREN DER H-MELDUNG-TABELLE                   *    00106000
      *                                                            *    00107000
      *------------------------------------------------------------*    00108000
       UA1-H-MELDUNGEN-INI SECTION.                                     00109000
       UA1-ANF.                                                         00109100
           MOVE 'UA1  '                     TO ERR-ORT-SEC              00109200
           PERFORM UR2-TRACE                                            00109300
      *                                                                 00109400
      *    * TABELLE INITIALISIEREN                                     00109500
           INITIALIZE                       CADI-HMELD-INFO-TAB         00109600
           .                                                            00109700
       UA1-EXIT.                                                        00109800
           EXIT.                                                        00109900
                                                                        00110000
      *------------------------------------------------------------*    00111000
      * UA2-H-MELDUNG-INFO                                         *    00112000
      *------------------------------------------------------------*    00113000
      * INPUT : ZI-DWAT0026-UA2-INPUT                              *    00114000
      *                                                            *    00115000
      * OUTPUT: CADI-HMELD-INFO-ZEILE                              *    00116000
      *                                                            *    00117000
      * VERARBEITUNG:                                              *    00118000
      *   - BEIM 1. AUFRUF TABELLE INITIALISIEREN                  *    00119000
      *   - MERKEN ZU JEDER MITGETEILTEN H-MELDUNG-NR, OB SIE      *    00120000
      *     ANZUZEIGEN IST ODER NICHT                              *    00130000
      *     (IMMER AKTUELLER STAND; KANN NACH AENDERUNG DER EINGA- *    00140000
      *      BEDATEN VARIIEREN)                                    *    00150000
      *                                                            *    00160000
      *------------------------------------------------------------*    00170000
       UA2-H-MELDUNG-INFO SECTION.                                      00180000
       UA2-ANF.                                                         00190000
           MOVE 'UA2  '                     TO ERR-ORT-SEC              00200000
           PERFORM UR2-TRACE                                            00210000
      *                                                                 00220000
      *    * ZEILE MIT GLEICHER H-MELDNR ODER FREIE ZEILE SUCHEN        00230000
           SET I-HM                         TO 1                        00231000
           SEARCH CADI-HMELD-INFO-ZEILE                                 00240000
                  VARYING I-HM                                          00250000
              AT END                                                    00260000
                 MOVE 22                       TO ERR-ORT-LFD           00270000
                 MOVE 'UA2: TABELLE VOLL / UA2-INPUT: '                 00280000
                                            TO ERR-VAR                  00281000
                 MOVE ZI-DWAT0026-UA2-H-MELDNR TO ERR-VAR (32:4)        00282000
                 MOVE ZI-DWAT0026-UA2-ANZUZEIG TO ERR-VAR (37:1)        00283000
                 PERFORM UR93-PROG-FEHLER                               00284000
                                                                        00285000
              WHEN CADI-HMELD-NR(I-HM) = ZERO             OR            00286000
                   CADI-HMELD-NR(I-HM) = ZI-DWAT0026-UA2-H-MELDNR       00287000
                                                                        00288000
                 MOVE ZI-DWAT0026-UA2-H-MELDNR                          00289000
                                            TO CADI-HMELD-NR(I-HM)      00289100
                 IF UA2-ANZUZEIG-J                                      00289200
                    MOVE K-JA               TO CADI-HMELD-ANZUZEIG(I-HM)00289300
                 ELSE                                                   00289400
                    MOVE K-NEIN             TO CADI-HMELD-ANZUZEIG(I-HM)00289500
                 END-IF                                                 00289600
           END-SEARCH                                                   00289700
           .                                                            00289800
       UA2-EXIT.                                                        00289900
           EXIT.                                                        00290000
      *                                                                 00312700
      *------------------------------------------------------------*    00313000
      * UA3-H-MELDUNGEN-AUSGEBEN                                   *    00320000
      *------------------------------------------------------------*    00321000
      * INPUT : CADI-HMELD-INFO-TAB                                *    00322000
      *                                                            *    00323000
      * OUTPUT: ZO-DWAT0026-UA3-OUTPUT,                            *    00323100
      *         CADI-MELDUNG, CURSOR AUF OK                        *    00323200
      *                                                            *    00323300
      * VERARBEITUNG:                                              *    00323400
      *   -                                                        *    00323500
      *                                                            *    00323600
      *------------------------------------------------------------*    00323700
       UA3-H-MELDUNGEN-AUSGEBEN SECTION.                                00323800
       UA3-ANF.                                                         00323900
           MOVE 'UA3  '                    TO ERR-ORT-SEC               00324000
           PERFORM UR2-TRACE                                            00325000
      *                                                                 00326000
      *    * ZEILE MIT GLEICHER H-MELDNR ODER FREIE ZEILE SUCHEN        00327000
           SET I-HM                        TO 1                         00327100
           SEARCH CADI-HMELD-INFO-ZEILE                                 00328000
                  VARYING I-HM                                          00329000
              AT END                                                    00330000
      *          MOVE SPACE                TO CADI-HMELD-S-STATUS       00331000
                 SET UA3-ANZEIGE-ENDE      TO TRUE                      00331100
                                                                        00331200
              WHEN CADI-HMELD-ANZUZEIG (I-HM)     = K-JA   AND          00331300
                   CADI-HMELD-ANGEZEIGT(I-HM) NOT = K-JA                00331400
                                                                        00331500
                 MOVE K-H                  TO CADI-MELDUNG-TYP          00331600
                 MOVE CADI-HMELD-NR(I-HM)  TO CADI-MELDUNG-NR           00331700
                 MOVE K-JA                 TO CADI-HMELD-ANGEZEIGT(I-HM)00331800
                                                                        00331900
                 MOVE K-CURSOR             TO EROKL IN MFI              00332000
                 SET ABSCHL-ANZEIGE-UNVERAENDERT TO TRUE                00333000
                                                                        00333100
                 SET UA3-ANZEIGE-NOCH      TO TRUE                      00333200
           END-SEARCH                                                   00333300
           .                                                            00333400
       UA3-EXIT.                                                        00333500
           EXIT.                                                        00333600
      *                                                                 00333700
      ******** ENDE COPY-MEMBER  DPAT0026 (H-MELDUNGEN BEARBEITEN) **** 00333800
