      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPZV0002.CPY  $
      *    $Revision:   1.0  $
      *
      *    $Log:   L:/MF/CPY/DPZV0002.CPv  $
      *
      *   Rev 1.0   Nov 04 1995 08:32:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPZV0002       SCHREIBMODULE                  *
      *                                  *--  LOG-TABLE VERARBEITUNG  *
      *    --------------------------------------------------------   *
      *    VERSION   001                                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. KUHN          (DVAG)                          *
      *    DATUM:    17.03.1993                                       *
      *    STAND:    17.03.1993                                       *
      *****************************************************************
      *
       Verarb-Log-Table.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *    *--  Verarb-Update                                        *
      *    !--  Verarb-Insert                                        *
      *    `--  Verarb-Delete                                        *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  -/-                                                    *
      *                                                              *


      *
      *
      *
           MOVE  WB-TBAT0005-Gesamt     TO  TBAT0005-Gesamt

           PERFORM  WITH TEST AFTER
                    VARYING  WI-Index1 FROM 1 BY  1
                                    UNTIL  WI-Index1 > K-Schreib-Max
                                     OR    (SQLCODE  NOT =  -904)
CCC005*       EXEC SQL
CCC004*          INSERT  INTO    TBAT0005
CCC004*                 (
CCC004*                      LOG_TABLE_ID,
CCC004*                      LOG_MA_NR,
CCC004*                      LOG_DAT_AB,
CCC004*                      LOG_DAT_AEND,
CCC004*                      LOG_PROGRAMM_ID,
CCC004*                      LOG_USER_ID,
CCC004*                      LOG_TERMINAL_ID,
CCC004*                      LOG_AEND_KZ,
CCC004*                      LOG_AEND_TYP,
CCC004*                      LOG_TUPEL_ALT_GS,
CCC004*                      LOG_TUPEL_NEU_GS
CCC004*                 )
CCC004*            VALUES
CCC004*                 (
CCC004*                     :LOG-TABLE-ID,
CCC004*                     :LOG-MA-NR,
CCC004*                     :LOG-DAT-AB,
CCC004*                     :LOG-DAT-AEND,
CCC004*                     :LOG-PROGRAMM-ID,
CCC004*                     :LOG-USER-ID,
CCC004*                     :LOG-TERMINAL-ID,
CCC004*                     :LOG-AEND-KZ,
CCC004*                     :LOG-AEND-TYP,
CCC004*                     :LOG-TUPEL-ALT-GS:NI-LOG-TUPEL-ALT-GS,
CCC004*                     :LOG-TUPEL-NEU-GS:NI-LOG-TUPEL-NEU-GS
CCC004*                 )
CCC003*       END-EXEC

           END-PERFORM

           IF   SQLCODE  =   ZERO
             CONTINUE
            ELSE
              SET   Verarb-Fehler  TO  TRUE
              SET   Rlback         TO  TRUE
              MOVE  SQLCODE        TO  WLR-Fehler-Nr
              IF    SQLERRML       >   ZERO
                  MOVE  SQLERRMC       TO   WLR-Fehler-Text
               ELSE
                   MOVE  SPACE          TO   WLR-Fehler-Text
              END-IF
           END-IF
           .
CCC009*    EJECT
