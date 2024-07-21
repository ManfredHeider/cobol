      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPZV0001.CPY  $
      *    $Revision:   1.0  $
      *
      *    $Log:   L:/MF/CPY/DPZV0001.CPv  $
      *
      *   Rev 1.0   Nov 04 1995 08:31:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPZV0001       SCHREIBMODULE                  *
      *                                  *--  STEUERUNG               *
      *                                  *--  PARAMETERPRÜFUNG        *
      *                                  *--  VERARBEITUNG            *
      *                                       (ohne SQL-Verarbeitung) *
      *    --------------------------------------------------------   *
      *    VERSION   001                                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. KUHN          (DVAG)                          *
      *    DATUM:    21.01.1993                                       *
      *    STAND:    17.03.1993                                       *
      *****************************************************************
      *
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Hauptprogramm                                          *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      * (* = Entscheidungsknoten)                                    *
      *                                                              *
      *    !-- Init-Vorlauf                                          *
      *    ! *-- Update-Nachlesen                                    *
      *    !                                                         *
      *    !-- PS-Parm-Pruefen                                       *
      *    !                                                         *
      *    !-- Init-Param                                            *
      *    !                                                         *
      *    *--  Verarb-Update                                        *
      *    ! !---  Update-Pruefen                                    *
      *    ! !---  Update-Ausfuehren                                 *
      *    ! *---  Verarb-Log-Table                                  *
      *    !                                                         *
      *    *--  Verarb-Insert                                        *
      *    ! !---  Insert-Pruefen                                    *
      *    ! !---  Insert-Ausfuehren                                 *
      *    ! *---  Verarb-Log-Table                                  *
      *    !                                                         *
      *    *--  Verarb-Delete                                        *
      *      !---  Delete-Pruefen                                    *
      *      !---  Delete-Ausfuehren                                 *
      *      *---  Verarb-Log-Table                                  *
      *                                                              *


      *    Initialisierung der Tablespezifischen Variablen

           PERFORM  Init-Vorlauf

      *    Work-Bereich für Link-Return füllen

           SET   Verarb-OK        TO   TRUE
           SET   Fehler-Typ-aus   TO   TRUE
           MOVE  K-Programm-ID    TO   WLR-Programm-ID
           MOVE  ZERO             TO   WLR-Fehler-Nr
           MOVE  SPACE            TO   WLR-Fehler-Text


           PERFORM  PS-Parm-Pruefen

           IF   Verarb-OK

              PERFORM   Init-Param

              IF   Verarb-OK

                   EVALUATE  TRUE

                     WHEN    WLS-Update

                                      PERFORM  Verarb-Update

                     WHEN    WLS-Insert

                                      PERFORM  Verarb-Insert

                     WHEN    WLS-Delete

                                      PERFORM  Verarb-Delete

                     WHEN    OTHER
                                      SET   Verarb-Fehler  TO  TRUE
                                      SET   Logik-Fehler   TO  TRUE
                                      MOVE   1   TO  WLR-Fehler-Nr
                                      MOVE SPACE TO WLR-Fehler-Text
                   END-EVALUATE
              END-IF
           END-IF

           MOVE  Workbereich-Link-Return   TO   Parameter-Return
           .
           GOBACK
           .
CCC009*    EJECT
      *---------------------------------------------------------------
       PS-Parm-Pruefen.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  -/-                                                    *
      *                                                              *

      *    Prüfen Daten aus Link-Bereich

           IF Verarb-OK
              IF PS-Date-Aend         =   SPACE
                 SET   Verarb-Fehler  TO  TRUE
                 SET   Logik-Fehler   TO  TRUE
                 MOVE       2         TO  WLR-Fehler-Nr
                 MOVE  SPACE          TO  WLR-Fehler-Text
              ELSE
                 IF PS-Date-Von          =   SPACE
                    SET   Verarb-Fehler  TO  TRUE
                    SET   Logik-Fehler   TO  TRUE
                    MOVE       3         TO  WLR-Fehler-Nr
                    MOVE  SPACE          TO  WLR-Fehler-Text
                 ELSE
                    IF PS-Programm-ID       =   SPACE
                       SET   Verarb-Fehler  TO  TRUE
                       SET   Logik-Fehler   TO  TRUE
                       MOVE       4         TO  WLR-Fehler-Nr
                       MOVE  SPACE          TO  WLR-Fehler-Text
                    ELSE
                       MOVE PS-Programm-ID  TO  WLS-Programm-ID
                       MOVE PS-Programm-KZ  TO  WLS-Programm-KZ

                       MOVE PS-Date-Aend    TO  WB-Date-Display
                       MOVE WB-Date-JJJJ-D  TO  WB-Date-JJJJ-A
                       MOVE WB-Date-MM-D    TO  WB-Date-MM-A
                       MOVE WB-Date-TT-D    TO  WB-Date-TT-A

                       MOVE PS-Date-Von     TO  WB-Date-Display
                       MOVE WB-Date-JJJJ-D  TO  WB-Date-JJJJ-V
                       MOVE WB-Date-MM-D    TO  WB-Date-MM-V
                       MOVE WB-Date-TT-D    TO  WB-Date-TT-V

                       IF WB-Date-Von          >=  WB-Date-Aend
                          MOVE PS-Date-Von     TO  WLS-Date-Von
                          MOVE PS-Date-Aend    TO  WLS-Date-Aend
                       ELSE
                          SET   Verarb-Fehler  TO  TRUE
                          SET   Logik-Fehler   TO  TRUE
                          MOVE       5         TO  WLR-Fehler-Nr
                          MOVE  SPACE          TO  WLR-Fehler-Text
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF Verarb-OK
              EVALUATE  TRUE
                  WHEN  WLS-Dialog
                            IF PS-User-ID   =  SPACE
                               SET   Verarb-Fehler  TO  TRUE
                               SET   Logik-Fehler   TO  TRUE
                               MOVE   7     TO   WLR-Fehler-Nr
                               MOVE  SPACE  TO  WLR-Fehler-Text
                            ELSE
                               MOVE  PS-User-ID   TO  WLS-User-ID
                               IF PS-Terminal-ID       = SPACE
                                  SET   Verarb-Fehler  TO  TRUE
                                  SET   Logik-Fehler   TO  TRUE
                                  MOVE   8     TO   WLR-Fehler-Nr
                                  MOVE  SPACE  TO   WLR-Fehler-Text
                               ELSE
                                  MOVE  PS-Terminal-ID  TO
                                        WLS-Terminal-ID
                               END-IF
                            END-IF
                  WHEN  WLS-BATCH
                            IF PS-Terminal-ID       =   SPACE
                               MOVE  PS-Terminal-ID TO
                                     WLS-Terminal-ID
                               IF PS-User-ID        =   SPACE
                                  MOVE  PS-User-ID  TO  WLS-User-ID
                               ELSE
                                  SET   Verarb-Fehler  TO  TRUE
                                  SET   Logik-Fehler   TO  TRUE
                                  MOVE   7     TO   WLR-Fehler-Nr
                                  MOVE  SPACE  TO  WLR-Fehler-Text
                               END-IF
                            ELSE
                               SET   Verarb-Fehler  TO  TRUE
                               SET   Logik-Fehler   TO  TRUE
                               MOVE   8     TO   WLR-Fehler-Nr
                               MOVE  SPACE  TO  WLR-Fehler-Text
                            END-IF
                  WHEN  OTHER
                            SET   Verarb-Fehler  TO  TRUE
                            SET   Logik-Fehler   TO  TRUE
                            MOVE   9     TO  WLR-Fehler-Nr
                            MOVE  SPACE  TO  WLR-Fehler-Text
              END-EVALUATE
           END-IF

           IF Verarb-OK
              MOVE  PS-Aend-KZ   TO  WLS-Aend-KZ

              EVALUATE  TRUE

                WHEN    WLS-Update
                        IF Z-Last-Update  =  SPACE
                           SET   Verarb-Fehler  TO  TRUE
                           SET   Logik-Fehler   TO  TRUE
                           MOVE   37    TO   WLR-Fehler-Nr
                           MOVE  SPACE  TO  WLR-Fehler-Text
                        ELSE
                           CONTINUE
                        END-IF

                WHEN    WLS-Insert
                        IF Z-Last-Update  =  SPACE
                           CONTINUE
                        ELSE
                           SET   Verarb-Fehler  TO  TRUE
                           SET   Logik-Fehler   TO  TRUE
                           MOVE   27    TO  WLR-Fehler-Nr
                           MOVE  SPACE  TO  WLR-Fehler-Text
                        END-IF

                WHEN    WLS-Delete
                        IF Z-Last-Update  =  SPACE
                           SET   Verarb-Fehler  TO  TRUE
                           SET   Logik-Fehler   TO  TRUE
                           MOVE   47    TO   WLR-Fehler-Nr
                           MOVE  SPACE  TO  WLR-Fehler-Text
                        ELSE
                           CONTINUE
                        END-IF

                WHEN    OTHER
                        CONTINUE

              END-EVALUATE
           END-IF
           .
CCC009*    EJECT

      *---------------------------------------------------------------
       Init-Param.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    *---  Update-Nachlesen                                    *
      *                                                              *


           IF     WLS-Dialog
                  SET   WB-LOG-Table-VA  TO  TRUE
            ELSE
                  SET   WB-LOG-Table-VA  TO  TRUE
           END-IF

           MOVE   PS-Aend-KZ         TO   WLS-Aend-KZ

           MOVE   K-Table-ID         TO   WB-LOG-Table-ID
           MOVE   WLS-Date-Aend      TO   WB-LOG-Dat-Aend
           MOVE   WLS-Date-Von       TO   WB-LOG-Dat-Ab
           MOVE   WLS-Programm-ID    TO   WB-LOG-Programm-ID
           MOVE   WLS-User-ID        TO   WB-LOG-User-ID
           MOVE   WLS-Terminal-ID    TO   WB-LOG-Terminal-ID
           MOVE   K-Nein             TO   WB-LOG-Aend-KZ

           MOVE   Zero               TO   WB-LOG-MA-Nr

           INITIALIZE   Z-Table-Key-vgl

           PERFORM  VARYING  WI-Index1 FROM 1 BY  1
                                    UNTIL  WI-Index1 > K-Attrib-Anzahl
              MOVE   ZERO    TO   WB-NI-TBAT0005-Ele (WI-Index1)
           END-PERFORM

           IF     WLS-Update
                  PERFORM   Update-Nachlesen
      *
      *           Nicht gefunden bei Nachlesen ist genauso zu
      *           behandeln wie Timestamp-Unterschied
      *
                  IF VERARB-FEHLER  AND  SQLCODE  =  100
                     SET  Logik-Fehler  TO  TRUE
                     MOVE     34        TO  WLR-Fehler-Nr
                     MOVE    SPACE      TO  WLR-Fehler-Text
                  ELSE
                     CONTINUE
            ELSE
                  CONTINUE
           END-IF
           .
CCC009*    EJECT
      *---------------------------------------------------------------
       Verarb-Insert.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  Insert-Pruefen                                         *
      *    -  Insert-Ausfuehren                                      *
      *    *--  Verarb-LOG-Table                                     *
      *                                                              *


      *
      *    Prüfen, ob Übergabebereiche für Insert korrekt gefüllt sind
      *
           PERFORM  Insert-Pruefen

      *
      *    Bei fehlerfreier Prüfung wird Insert ausgeführt
      *
           IF   Verarb-OK
             PERFORM   Insert-Ausfuehren

      *
      *    Nach erfolgter Verarbeitung Prüfung ob LOG-Tablesatz
      *    erstellt werden muß.
      *
             IF   Verarb-OK
               IF   WB-LOG-Table-VA
                 MOVE  K-Insert            TO  WB-LOG-Aend-Typ
                 MOVE  Z-LOG-MA-Nr         TO  WB-LOG-MA-Nr

                 MOVE    -1                TO  WB-NI-LOG-Tupel-Alt-GS
                 MOVE   ZERO               TO  WB-LOG-Tupel-Alt-GS-Len
                 MOVE  LOW-VALUE           TO  WB-LOG-Tupel-Alt-GS-Text

                 MOVE   ZERO               TO  WB-NI-LOG-Tupel-Neu-GS
                 MOVE  K-Bereich-Lang      TO  WB-LOG-Tupel-Neu-GS-Len
                 MOVE  WB-Gesamt           TO  WB-LOG-Tupel-Neu-GS-Text

                 PERFORM   Verarb-LOG-Table
               END-IF
             END-IF
           END-IF
           .
CCC009*    EJECT
      *---------------------------------------------------------------
       Insert-Pruefen.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *    `-- Verarb-Insert                                         *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  -/-                                                    *
      *                                                              *


      *
      *
      *
           IF   WB-Gesamt    =   LOW-VALUE
             SET   Verarb-Fehler  TO  TRUE
             SET   Logik-Fehler   TO  TRUE
             MOVE   20    TO   WLR-Fehler-Nr
             MOVE  SPACE  TO   WLR-Fehler-Text
            ELSE
              IF   Z-Table-Key   =  Z-Table-Key-vgl
                SET   Verarb-Fehler  TO  TRUE
                SET   Logik-Fehler   TO  TRUE
                MOVE   22    TO   WLR-Fehler-Nr
                MOVE  SPACE  TO   WLR-Fehler-Text
               ELSE
                CONTINUE
              END-IF
           END-IF
           .
CCC009*    EJECT
      *---------------------------------------------------------------
       Verarb-Update.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  Update-Pruefen                                         *
      *    -  Update-Ausfuehren                                      *
      *    *--  Verarb-LOG-Table                                     *
      *                                                              *


      *
      *    Prüfen, ob Übergabebereiche für Update korrekt gefüllt sind
      *
           PERFORM  Update-Pruefen

      *
      *    Bei fehlerfreier Verarbeitung wird Update ausgeführt
      *
           IF   Verarb-OK
             PERFORM   Update-Ausfuehren

      *
      *    Nach erfolgter Verarbeitung Prüfung ob LOG-Tablesatz
      *    erstellt werden muß.
      *
             IF   Verarb-OK
               IF   WB-LOG-Table-VA
                 MOVE  K-Update            TO  WB-LOG-Aend-Typ
                 MOVE  Z-LOG-MA-Nr         TO  WB-LOG-MA-Nr

                 MOVE   ZERO               TO  WB-NI-LOG-Tupel-Alt-GS
                 MOVE  K-Bereich-Lang      TO  WB-LOG-Tupel-Alt-GS-Len
                 MOVE  WB-Update-Gesamt   TO   WB-LOG-Tupel-ALT-GS-Text

                 MOVE   ZERO               TO  WB-NI-LOG-Tupel-Neu-GS
                 MOVE  K-Bereich-Lang      TO  WB-LOG-Tupel-Neu-GS-Len
                 MOVE  WB-Gesamt          TO   WB-LOG-Tupel-Neu-GS-Text

                 PERFORM   Verarb-LOG-Table
               END-IF
             ELSE
      *
      *      Nicht gefunden bei Update ist genauso zu
      *      behandeln wie Timestamp-Unterschied
      *
               IF VERARB-FEHLER  AND  SQLCODE  =  100
                  SET  Logik-Fehler  TO  TRUE
                  MOVE     34        TO  WLR-Fehler-Nr
                  MOVE    SPACE      TO  WLR-Fehler-Text
               ELSE
                  CONTINUE
               END-IF
             END-IF
           END-IF
           .
CCC009*    EJECT
      *---------------------------------------------------------------
       Update-Pruefen.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *    `-- Verarb-Update                                         *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  -/-                                                    *
      *                                                              *

           IF Z-Last-Update     =  Z-Last-Update-NL

              IF WB-Update-Gesamt     =  WB-Gesamt

                 SET   Verarb-Fehler  TO  TRUE
                 SET   Logik-Fehler   TO  TRUE
                 MOVE   32            TO  WLR-Fehler-Nr
                 MOVE  SPACE          TO  WLR-Fehler-Text
              ELSE
                 CONTINUE
              END-IF

           ELSE
              SET   Verarb-Fehler  TO  TRUE
              SET   Logik-Fehler   TO  TRUE
              MOVE   34            TO  WLR-Fehler-Nr
              MOVE  SPACE          TO  WLR-Fehler-Text
           END-IF

           .
CCC009*    EJECT
      *---------------------------------------------------------------
       Verarb-Delete.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  Delete-Pruefen                                         *
      *    -  Delete-Ausfuehren                                      *
      *    *--  Verarb-LOG-Table                                     *
      *                                                              *


      *
      *    Prüfen, ob Übergabebereiche für Delete korrekt gefüllt sind
      *
           PERFORM  Delete-Pruefen

      *
      *    Bei fehlerfreier Prüfung wird Delete ausgeführt
      *
           IF   Verarb-OK
             PERFORM   Delete-Ausfuehren

      *
      *    Nach erfolgter Verarbeitung Prüfung ob LOG-Tablesatz
      *    erstellt werden muß.
      *
             IF   Verarb-OK
               IF   WB-LOG-Table-VA
                 MOVE  K-Delete            TO  WB-LOG-Aend-Typ
                 MOVE  Z-LOG-MA-Nr         TO  WB-LOG-MA-Nr

                 MOVE   ZERO               TO  WB-NI-LOG-Tupel-Alt-GS
                 MOVE  K-Bereich-Lang      TO  WB-LOG-Tupel-Alt-GS-Len
                 MOVE  WB-Gesamt           TO  WB-LOG-Tupel-ALT-GS-Text

                 MOVE    -1                TO  WB-NI-LOG-Tupel-Neu-GS
                 MOVE   ZERO               TO  WB-LOG-Tupel-Neu-GS-Len
                 MOVE  LOW-VALUE           TO  WB-LOG-Tupel-Neu-GS-Text

                 PERFORM   Verarb-LOG-Table
               END-IF
             ELSE
      *
      *      Nicht gefunden bei Delete ist genauso zu
      *      behandeln wie Timestamp-Unterschied
      *
               IF VERARB-FEHLER  AND  SQLCODE  =  100
                  SET  Logik-Fehler  TO  TRUE
                  MOVE     44        TO  WLR-Fehler-Nr
                  MOVE    SPACE      TO  WLR-Fehler-Text
               ELSE
                  CONTINUE
               END-IF
             END-IF
           END-IF
           .
CCC009*    EJECT
      *---------------------------------------------------------------
       Delete-Pruefen.
      *---------------------------------------------------------------
      * Aufruf durch:                                                *
      *                                                              *
      *    -  Main                                                   *
      *    `-- Verarb-Delete                                         *
      *                                                              *
      * Aufgerufene Paragraphen:                                     *
      *                                                              *
      *    -  -/-                                                    *
      *                                                              *


      *
      *
      *
           IF WB-Gesamt    =   LOW-VALUE
              SET   Verarb-Fehler  TO  TRUE
              SET   Logik-Fehler   TO  TRUE
              MOVE   40            TO  WLR-Fehler-Nr
              MOVE  SPACE          TO  WLR-Fehler-Text
           ELSE
              IF Z-Table-Key   =   Z-Table-Key-vgl
                 SET   Verarb-Fehler  TO  TRUE
                 SET   Logik-Fehler   TO  TRUE
                 MOVE   42            TO  WLR-Fehler-Nr
                 MOVE  SPACE          TO  WLR-Fehler-Text
              ELSE
                 CONTINUE
              END-IF
           END-IF
           .
CCC009*    EJECT
