      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPOP0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPOP0002.CPv  $
      *
      *   Rev 2.0   Jun 07 1999 10:01:18   T13EX15
      *NT-Migration
      *
      *   Rev 1.0   Nov 04 1995 08:19:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPOP0002                                      *
      *    --------------------------------------------------------   *
      *    VERSION   1    22.06.95 13:26:24
      *    --------------------------------------------------------   *
      *    ERSTELLT: B. BOHLINGER                                     *
      *    DATUM:    01.09.1994                                       *
      *    STAND:    21.06.1995                                       *
      *****************************************************************
      * VERTRAGSNUMMERNPRÜFUNG
      *
      *------------------------------------------------------------*
      * INPUT : ZI-DWOP0002-INPUT                                  *
      *                                                            *
      * OUTPUT: ZO-DWOP0002-OUTPUT                                 *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   VERTRAGSNUMMER-PRÜFUNG                                   *
      *   - FUNKTION IVSNR-PRÜFEN                                  *
      *     PRÜFEN DER KORREKTEN ZUORDNUNG DER INTERNEN ZUR        *
      *     EXTERNEN VERTRAGSNUMMER                                *
      *                                                            *
      *                                                            *
      *                                                            *
      *------------------------------------------------------------*
      ****************************************************************  00210000
      * AENDERUNGEN:                                                 *  00220000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00230000
      * -------------------------------------------------------------*  00240000
      * AFOP0030!21.06.95! 01 ! NEU                       ! BOHLINGER*  00250000
      *         !        !    !                           !          *  00260000
      *         !        !    !                           !          *  00270000
      *         !        !    !                           !          *  00280000
      *         !        !    !                           !          *  00290000
      *         !        !    !                           !          *  00300000
      *                                                              *  00310000
      * ENDE-LOGBUCH                                                 *  00320000
      ****************************************************************  00330000
      *                                                                 00960000
      *
       UY2-OP02-VERTRAGSNR-PRUEFEN  SECTION.
       UY2-OP02-A.
           MOVE 'UY2 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    ANFANGSWERTE SETZEN
           SET DWOP0002-RC-NOK             TO TRUE
           SET DWOP0002-RC-IVSNR-OK        TO TRUE
           SET DWOP0002-RC-INI             TO TRUE
           SET DWOP0002-RC-INFO-NEIN       TO TRUE
           SET DWOP0002-RC-INFO-INIT       TO TRUE

           EVALUATE TRUE

           WHEN DWOP0002-FKT-IVSNR-PRUEF
                SET S-SNSATZ-SUCHEN     TO TRUE
                PERFORM  UY21-SN-DATA-STARTBR

      *         LESEN DER SN-DAT MIT ALTERNATEKEY
                PERFORM  UNTIL S-SNSATZ-GEFUNDEN  OR
                         S-SNSATZ-NOTFND OR
                         PGM-STATUS-FEHLER
                   IF PGM-STATUS-OK
                      PERFORM  UY22-SN-DATA-READN
                   END-IF
                   IF PGM-STATUS-OK
                      PERFORM  UY26-VSNR-PRUEF
                   END-IF
                END-PERFORM

                IF PGM-STATUS-OK
                   PERFORM  UY23-SN-DATA-ENDBR
                ELSE
                   GO TO UY2-OP02-EXIT
                END-IF
                IF DWOP0002-RC-NOK
      *            LESEN DER SN-DATA MIT EXTERNER VSNR
                   PERFORM UY24-SN-DAT-READ
                END-IF

      *    WHEN DWOP0002-FKT-VSNR-PRUEF
      *         CONTINUE
           WHEN ANY
                MOVE  K-1                       TO  ERR-ORT-LFD
                MOVE  'DPOP0002'                TO  ERR-VAR-ZEILE01
                MOVE  'FUNKTION NICHT BEKANNT'  TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER           TO  TRUE
                PERFORM XR93-UPRO-PROG-FEHLER
           END-EVALUATE
           .
       UY2-OP02-EXIT.
           EXIT.

      *
       UY21-SN-DATA-STARTBR SECTION.
       UY21-A.
           MOVE 'UY21 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    STARTBROUWSE SN-DATA (ALTERNATIVKEY IST IVSNR)

      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-SN-NAMEA        TO Z-DWOP0002-DAT-NAME
           MOVE ZI-DWOP0002-IVSNR          TO Z-DWOP0002-IVSNR

CCC005*    EXEC CICS STARTBR
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RIDFLD   (Z-DWOP0002-IVSNR)
CCC004*              GTEQ
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
           WHEN DFHRESP(ENDFILE)
                SET DWOP0002-RC-IVSNR-NOTFND TO TRUE
                SET S-SNSATZ-NOTFND          TO TRUE
           WHEN OTHER
                MOVE  K-1                    TO  ERR-ORT-LFD
                MOVE  ZI-DWOP0002-KEY        TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE
           .
       UY21-EXIT.
           EXIT.


      *
       UY22-SN-DATA-READN SECTION.
       UY22-A.
           MOVE 'UY22 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    LESEN DER SN-DATA ( ALTERNATIVKEY)


      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-SN-NAMEA        TO Z-DWOP0002-DAT-NAME
           MOVE K-DWOP0002-SN-LAENGE       TO Z-DWOP0002-DAT-LAENGE

CCC005*    EXEC CICS READNEXT
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RIDFLD   (Z-DWOP0002-IVSNR)
CCC004*              INTO     (SN-SATZA)
CCC004*              LENGTH   (Z-DWOP0002-DAT-LAENGE)
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
           WHEN DFHRESP(DUPKEY)
                CONTINUE

           WHEN DFHRESP(NOTFND)
           WHEN DFHRESP(ENDFILE)
                SET DWOP0002-RC-IVSNR-NOTFND TO TRUE
                SET S-SNSATZ-NOTFND          TO TRUE
           WHEN OTHER
                MOVE  K-1                    TO  ERR-ORT-LFD
                MOVE  ZI-DWOP0002-KEY        TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE
           .
       UY22-EXIT.
           EXIT.

      *
       UY26-VSNR-PRUEF SECTION.
       UY26-A.
           MOVE 'UY26 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE



           IF ZI-DWOP0002-IVSNR NOT = SNIVSNR IN SN-SATZA
              SET DWOP0002-RC-IVSNR-NOTFND   TO TRUE
              SET S-SNSATZ-NOTFND            TO TRUE
              GO TO UY26-EXIT
           END-IF


      *    DER SN-SATZ DER GESELLSCHAFT MIT DER IVSNR WURDE GEFUNDEN
           IF ZI-DWOP0002-GES = SNGES IN SN-SATZA

              MOVE SNVSNR IN SN-SATZA   TO Z-DWOP0002-VSNR-MERK
              IF ZI-DWOP0002-VSNR = SNVSNR IN SN-SATZA
      *          ZUORDNUNG DER VERTRAGSNUMMERN IST OK
                 SET DWOP0002-RC-OK                   TO TRUE
                 SET S-SNSATZ-GEFUNDEN                TO TRUE
              ELSE
      *          SN-SATZ SEHT NOCH AUF INT. VSNR
                 IF SNVSNR IN SN-SATZA  = SNIVSNR IN SN-SATZA
                    SET DWOP0002-RC-IVSNR-AUFINT      TO TRUE
                    SET S-SNSATZ-GEFUNDEN             TO TRUE
                 ELSE
      *             IVSNR SIND GLEICH, JODOCH UNTERSCHIEDL. EXTERNE VSNR
                    SET DWOP0002-RC-IVSNR-VSNR-NOK    TO TRUE
                    SET S-SNSATZ-NOTFND               TO TRUE
                 END-IF
              END-IF
           END-IF

           .
       UY26-EXIT.
           EXIT.


      *
       UY23-SN-DATA-ENDBR SECTION.
       UY23-A.
           MOVE 'UY23 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    ENDBROUWSE SN-DATA

      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-SN-NAMEA        TO Z-DWOP0002-DAT-NAME

CCC005*    EXEC CICS ENDBR
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
           WHEN DFHRESP(INVREQ)
                CONTINUE
           WHEN OTHER
                MOVE  K-1                    TO  ERR-ORT-LFD
                MOVE  'FEHLER BEI ENBR'      TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE
           .
       UY23-EXIT.
           EXIT.


      *
       UY24-SN-DAT-READ SECTION.
       UY24-A.
           MOVE 'UY24 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    DIREKTES LESEN DER SN-DAT


      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-SN-NAME         TO Z-DWOP0002-DAT-NAME
           MOVE K-DWOP0002-SN-LAENGE       TO Z-DWOP0002-DAT-LAENGE
           MOVE ZI-DWOP0002-KEY            TO Z-DWOP0002-KEY

CCC005*    EXEC CICS READ
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RIDFLD   (Z-DWOP0002-KEY)
CCC004*              INTO     (SN-SATZ)
CCC004*              LENGTH   (Z-DWOP0002-DAT-LAENGE)
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
      *         ES WURDE EIN SATZ MIT DER VSNR GEFUNDEN
                EVALUATE TRUE

      *         ZUR INTVSNR GIBT ES EIN SATZ, DER AUF INTERN STEHT
                WHEN DWOP0002-RC-IVSNR-AUFINT
                     SET DWOP0002-RC-AUFINT-VSNRFND  TO TRUE


      *         ZUR INTVSNR GIBT ES KEINEN SATZ
      *         ZUR VSNR WURDE EIN SATZ GEFUNDEN
                WHEN DWOP0002-RC-IVSNR-NOTFND
                     SET DWOP0002-RC-INTNOTFND-VSNRFND  TO TRUE

      *         ZUORDNUNG IVSNR/VSNR IST NICHT OK,
      *         ZUR VSNR WURDE EIN SATZ GEFUNDEN
                WHEN DWOP0002-RC-IVSNR-VSNR-NOK
                     CONTINUE

                WHEN ANY
                     MOVE  K-1                       TO  ERR-ORT-LFD
                     MOVE 'RETURNCODE NICHT BEKANNT' TO  ERR-VAR-ZEILE01
                     SET PGM-STATUS-FEHLER           TO TRUE            00010000
                     PERFORM XR93-UPRO-PROG-FEHLER
                END-EVALUATE


           WHEN DFHRESP(NOTFND)
           WHEN DFHRESP(ENDFILE)

      *         ES WURDE KEIN SATZ MIT DER VSNR GEFUNDEN
                EVALUATE TRUE
      *         ZUORDNUNG IVSNR/VSNR IST OK
                WHEN DWOP0002-RC-IVSNR-AUFINT
                     SET DWOP0002-RC-OK                 TO TRUE

      *         ES WURDE KEIN SATZ MIT DER VSNR UND MIT IVSNR GEFUNDEN
      *         KEIN ENTSPRECHENDER SNSATZ VORHANDEN --> KEIN ANTRAG
                WHEN DWOP0002-RC-IVSNR-NOTFND
                     SET DWOP0002-RC-OK                 TO TRUE
                     SET DWOP0002-RC-INFO-JA            TO TRUE
                     SET DWOP0002-RC-IVSNR-VSNR-NOTFND  TO TRUE

      *         ZUR INTSNR WURDE SATZ GEFUNDEN MIT ANDERER VSNR
      *         ZUORDNUNG IVSNR/VSNR IST NICHT IN ORDNUNG
                WHEN DWOP0002-RC-IVSNR-VSNR-NOK
      *              LESEN DER HISTORIEN-DATEI
                     SET S-VNH-SUCHEN TO TRUE
                     PERFORM  UY25-VNH-LESEN
                     IF PGM-STATUS-FEHLER
                        GO TO  UY24-EXIT
                     END-IF
                     IF S-VNH-GEFUNDEN
                        SET DWOP0002-RC-OK           TO TRUE
                     ELSE
                        SET DWOP0002-RC-OK                TO TRUE
                        SET DWOP0002-RC-INFO-JA           TO TRUE
                        SET DWOP0002-RC-INTFND-VSNRNOTFND TO TRUE
                     END-IF

                WHEN ANY
                     MOVE  K-2                       TO  ERR-ORT-LFD
                     MOVE 'RETURNCODE NICHT BEKANNT' TO  ERR-VAR-ZEILE01
                     SET PGM-STATUS-FEHLER           TO TRUE            00010000
                     PERFORM XR93-UPRO-PROG-FEHLER
                END-EVALUATE



           WHEN OTHER
                MOVE  K-3                    TO  ERR-ORT-LFD
                MOVE  ZI-DWOP0002-KEY        TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE

           .
       UY24-EXIT.
           EXIT.



      *
       UY25-VNH-LESEN SECTION.
       UY25-A.
           MOVE 'UY25 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

           SET S-VNH-SUCHEN TO TRUE

      *    START AUF DIE VNH-DATA (ALTERNATE-KEY)
           MOVE ZERO                       TO Z-DWOP0002-VNH-KEYA
           MOVE ZI-DWOP0002-GES            TO Z-DWOP0002-GES
           MOVE Z-DWOP0002-VSNR-MERK       TO Z-DWOP0002-VSNR

           PERFORM UY251-VNH-DATA-STARTBR
           IF S-VNH-ENDE
              GO TO UY25-EXIT
           END-IF

      *    LESEN DER VHN-DATA

           PERFORM  UNTIL S-VNH-GEFUNDEN OR
                          S-VNH-ENDE OR
                          PGM-STATUS-FEHLER
                    PERFORM UY252-VNH-DATA-READN
                    IF ZI-DWOP0002-GES NOT  = DAT30-ALTERKEY-GES
                                              IN VNH-SATZA
                       SET S-VNH-ENDE          TO TRUE
      *                GO TO UY25-EXIT
                    ELSE
                       IF Z-DWOP0002-VSNR-MERK = DAT30-VSNR-ALT
                                                 IN VNH-SATZA
                          SET S-VNH-GEFUNDEN   TO TRUE
                       ELSE
                          IF Z-DWOP0002-VSNR-MERK < DAT30-VSNR-ALT
                                                    IN VNH-SATZA
                             SET S-VNH-ENDE       TO TRUE
                          END-IF
                       END-IF
                    END-IF
           END-PERFORM

           PERFORM UY253-VNH-DATA-ENDBR
           .
       UY25-EXIT.
           EXIT.


      *
       UY251-VNH-DATA-STARTBR SECTION.
       UY251-A.
           MOVE 'UY251'                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    STARTBROUWSE VNH-DATA

      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-VNH-NAMEA       TO Z-DWOP0002-DAT-NAME

CCC005*    EXEC CICS STARTBR
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RIDFLD   (Z-DWOP0002-VNH-KEYA)
CCC004*              GTEQ
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
           WHEN DFHRESP(ENDFILE)
                SET S-VNH-ENDE               TO TRUE
           WHEN OTHER
                MOVE  K-1                    TO  ERR-ORT-LFD
                MOVE  ZI-DWOP0002-KEY        TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE
           .
       UY251-EXIT.
           EXIT.

      *
       UY252-VNH-DATA-READN SECTION.
       UY252-A.
           MOVE 'UY252'                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE


      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-VNH-NAMEA        TO Z-DWOP0002-DAT-NAME
           MOVE K-DWOP0002-VNH-LAENGE       TO Z-DWOP0002-DAT-LAENGE

CCC005*    EXEC CICS READNEXT
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RIDFLD   (Z-DWOP0002-VNH-KEYA)
CCC004*              INTO     (VNH-SATZA)
CCC004*              LENGTH   (Z-DWOP0002-DAT-LAENGE)
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
                CONTINUE

           WHEN DFHRESP(NOTFND)
           WHEN DFHRESP(ENDFILE)
                SET S-VNH-ENDE              TO TRUE
           WHEN OTHER
                MOVE  K-1                    TO  ERR-ORT-LFD
                MOVE  ZI-DWOP0002-KEY        TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE

           .
       UY252-EXIT.
           EXIT.



       UY253-VNH-DATA-ENDBR SECTION.
       UY253-A.
           MOVE 'UY253 '                     TO ERR-ORT-SEC
           PERFORM UR2-TRACE

      *    ENDBROUWSE VNH-DATA

      *    ANFANGSWERTE SETZEN
           MOVE K-DWOP0002-VNH-NAMEA        TO Z-DWOP0002-DAT-NAME

CCC005*    EXEC CICS ENDBR
CCC004*              DATASET  (Z-DWOP0002-DAT-NAME)
CCC004*              RESP     (C-CICS-RC)
CCC003*    END-EXEC
      *
           EVALUATE C-CICS-RC
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE  K-1                    TO  ERR-ORT-LFD
                MOVE  'FEHLER BEI ENBR'      TO  ERR-VAR-ZEILE01
                MOVE  Z-DWOP0002-DAT-NAME    TO  ERR-VAR-ZEILE02
                SET PGM-STATUS-FEHLER        TO  TRUE                   00010000
                PERFORM XR92-UPRO-CICS-FEHLER
           END-EVALUATE
           .
       UY253-EXIT.
           EXIT.

      ******* ENDE COPY-MEMBER DPOP0002 (VERTRAGSNUMMERN-PRÜFEN) *******
CCC009*    EJECT
