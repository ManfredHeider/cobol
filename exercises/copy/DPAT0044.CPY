      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0044.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DPAT0044.CPv  $
      *
      *   Rev 2.2   Sep 20 2007 13:54:38   t13ae34
      *AAN00755: Verzweigung von AN11 zu den Daisy-Dialogen
      *
      *   Rev 2.1   10 May 2001 13:05:42   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.0   Feb 27 1999 11:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 20 1997 16:59:36   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   Nov 04 1995 08:18:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0044      VB: DIALOG-STATUS-ABH BEARB.    *
      *                 (ERSETZT FLIESSEND DPAT0011 (WG. CTV-ANBDG.)) *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    15.03.1995                                       *
      *****************************************************************
      *    VERSION   1    02.05.95 16:36:55
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0182!15.03.95!    ! NEU (ERSETZT DPAT0011)    ! SCHÖLLH. *
      *         !28.04.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFPA0002!20.01.97! 02 ! ANPASSUNG AN OP-DPN/OP-DPP! SCHÖLLH. *
      *         !        !    !  -IMPLEMENT. IN PGAT0001  !          *
      * AAN00755!20.09.07!2.2 ! AN-TRANSAKTIONEN          ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIE FOLGENDEN SECTIONS DIENEN DER BEARBEITUNG VON
      *    AKTIVITAETEN AUF DER OBERSTEN DIALOG-EBENE IN VB-DIALOGEN,
      *    DIE ABHAENGIG VOM DIALOG-STATUS SIND
      *    - ZR2-FT-DIALOG-STATUS-ABH
      *    - ZR3-FUSS-DIALOG-STATUS-ABH
      *
      *------------------------------------------------------------*
       ZR2-FT-DIALOG-STATUS-ABH SECTION.
      *------------------------------------------------------------*
      * INPUT : CADD-DIALOG-STATUS, Z-FTASTE                       *
      *                                                            *
      * OUTPUT: FT-RC                                              *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - AUSFUEHREN VON AKTIVITÄTEN BEI PRUEFUNG DER FUNKTIONS- *
      *       TASTEN, DIE VOM DIALOG-STATUS ABHÄNGEN               *
      *                                                            *
      * ANMERKUNG: AUFRUF NUR AUF OBERSTER DIALOG-EBENE ZULÄSSIG   *
      *                                                            *
      *------------------------------------------------------------*
       ZR2-ANF.
           MOVE 'ZR2  '                      TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET FTASTE-OK                     TO TRUE

           PERFORM UR5-INDEX-DZST-POSITIONIEREN
      *    * ÜBER I-DZST KANN JETZT AUF DIE ZUSTANDSPARAMETER
      *    * ZUGEGRIFFEN WERDEN

           EVALUATE Z-FTASTE
           WHEN DFHENTER
              EVALUATE TRUE
      *       ---------------------------------------------------- *
      *       TRANS + DATFRG PRUEFEN AUF OBERSTER DIALOG-EBENE     *
      *       VOR ODER NACH EINER BEARBEITUNG                      *
      *       ---------------------------------------------------- *
200907*>>>>>  BEI AN-TRANSAKTIONEN IST IMMER EINE NEUE TRANS MÖGLICH
              WHEN ERTRANSI IN MFI NOT = SPACE AND
                   CARR-DIALOG-AKT(1:2) = 'AN'
      *             * WECHSEL ZU ANDERER TRANSAKTION
                    MOVE K-OP-DWD            TO CADR-E-OP
                    MOVE ERTRANSI IN MFI     TO CADR-E-TRANS-NEXT
                    MOVE ERKEYI   IN MFI     TO CADU-OB
                    MOVE K-CURSOR            TO ERTRANSL IN MFI
                    SET FTASTE-ABBR-VERZW    TO TRUE
200907*<<<<<
              WHEN ERTRANSI IN MFI NOT = SPACE
200197*           IF CARD-DIALOG-SCHRITT-AKT = CARR-DIALOG-AKT AND
200197           IF CARR-UANW = ZERO                          AND
200197              CARR-PAGE = 1                             AND
                    (T-DIALOG-STATUS(I-DZST)
                     = K-DIALOG-STATUS-INI           OR
                       K-DIALOG-STATUS-ABGESCHLOSSEN   )
                 THEN
      *             * WECHSEL ZU ANDERER TRANSAKTION
                    MOVE K-OP-DWD            TO CADR-E-OP
                    MOVE ERTRANSI IN MFI     TO CADR-E-TRANS-NEXT
                    MOVE ERKEYI   IN MFI     TO CADU-OB
                    MOVE K-CURSOR            TO ERTRANSL IN MFI
                    SET FTASTE-ABBR-VERZW    TO TRUE
                 ELSE
                    MOVE K-E                 TO CARD-MELDRAHM-TYP
                    MOVE 25                  TO CARD-MELDRAHM-NR
                    MOVE K-CURSOR            TO ERTRANSL IN MFI
                    SET FTASTE-ABBR          TO TRUE
                 END-IF
              WHEN OTHER
                 CONTINUE
              END-EVALUATE
           WHEN OTHER
              CONTINUE
           END-EVALUATE
           .
       ZR2-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       ZR3-FUSS-DIALOG-STATUS-ABH SECTION.
      *------------------------------------------------------------*
      * INPUT : S-DIALOG-ZUSTAND, T-DIALOG-ZUSTAND-TAB             *
      *                                                            *
      * OUTPUT: SPEZIELLE FUSS-FELDER (Z.B. CTV)                   *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - AUSFUEHREN VON AKTIVITÄTEN BEI VERSORGEN DER FUSS-     *
      *       FELDER, DIE VOM DIALOG-STATUS ABHÄNGEN               *
      *                                                            *
      * ANMERKUNG: AUFRUF NUR AUF OBERSTER DIALOG-EBENE ZULÄSSIG   *
      *                                                            *
      *------------------------------------------------------------*
       ZR3-ANF.
           MOVE 'ZR3  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           PERFORM UR5-INDEX-DZST-POSITIONIEREN
      *    * ÜBER I-DZST KANN JETZT AUF DIE ZUSTANDSPARAMETER
      *    * ZUGEGRIFFEN WERDEN

           IF T-DIALOG-STATUS(I-DZST) = K-DIALOG-STATUS-ABGESCHLOSSEN
              PERFORM ZR31-CTV-FELDER
           END-IF
           .
       ZR3-EXIT.
           EXIT.

      *------------------------------------------------------------*
       ZR31-CTV-FELDER SECTION.
      *------------------------------------------------------------*
      * INPUT : CARD-DIALOG-SCHRITT-AKT, CARD-CTV-BRIEFMENUE       *
      *                                                            *
      * OUTPUT: EVTL. CTV-FELDER IM FUSS                           *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - PRÜFEN, OB CTV-BRIEFMENUE ZUR TRANS VORHANDEN IST      *
      *   - GGF. CTV-FELDER ANZEIGEN                               *
      *   - GGF. FTASTE F22 ANZEIGEN                               *
      *------------------------------------------------------------*
       ZR31-ANF.
           MOVE 'ZR31 ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           IF CARD-CTV-BRIEFMENUE NOT = SPACE     AND
                                  NOT = LOW-VALUE
              SET CTV-ANZEIGE                TO TRUE
      *       * CTV-FELDER ANZEIGEN UND EVTL. CURSOR SETZEN
              MOVE K-CTV                     TO ARCTVTXO IN MFO
              MOVE DFHBMASK                  TO ARCTVTXA IN MFI
              MOVE SPACE                     TO ERCTVO   IN MFO
              MOVE DFHBMFSE                  TO ERCTVA   IN MFI
              IF T-CTV-FELD-CURSOR(I-DZST) = K-J
                 MOVE K-CURSOR               TO ERCTVL   IN MFI
              END-IF
           END-IF
           .
       ZR31-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER DPAT0044 (VB: DIALOG-STATUS-ABH) *****

