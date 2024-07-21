      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0012.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DPAT0012.CPv  $
      *
      *   Rev 2.6   Feb 21 2007 12:25:34   t13ae34
      *ACV00054: Change Request CTV-Service-Funktion
      *
      *   Rev 2.5   Feb 15 2007 10:01:40   t13ae34
      *ACV00054: Anpassung Driver-Rahmen -> über AAN00655
      *
      *   Rev 2.4   Jan 27 2006 11:37:30   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.3   10 May 2001 13:03:36   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.2   Apr 12 1999 12:02:06   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   Rev 2.0   MAR 01 1999 14:44:14   T13AE34
      *NT-UMSTELLUNG
      *
      *   Rev 1.1   MAR 20 1997 16:59:26   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   NOV 04 1995 08:12:42   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    COPY-MEMBER: DPAT0012      VB: DIALOG-STATUS-UNABH BEARB.  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    12.03.1993                                       *
      *****************************************************************
      *    VERSION   03   15.10.93                                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.93! 01 ! NEU                       ! SCHÖLLH. *
      *         !15.09.93! 02 ! F13 DURCH F21 ERSETZT     ! SCHÖLLH. *
      *         !15.10.93!    ! PRÜFEN EROKI AUF 'J'/'N'  ! SCHÖLLH. *
      *         !15.10.93! 03 ! PROD.ÜBERGABE             ! HECKER   *
      * AFPA0002!20.01.97! 04 ! ANPASSUNG AN OP-DPN/OP-DPP! SCHÖLLH. *
      *         !        !    !  -IMPLEMENT. IN PGAT0001  !          *
      * AAT00005!17.02.99!1.2 ! EURO-SWITCH IN NEUDIALOGEN! HEIDER   *
      * ACV00015!09.03.01!2.3 ! CTV AUS NEUDIALOGEN       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIE FOLGENDEN SECTIONS DIENEN DER BEARBEITUNG VON
      *    AKTIVITAETEN AUF DER OBERSTEN DIALOG-EBENE IN VB-DIALOGEN,
      *    DIE UNABHAENGIG VOM DIALOG-STATUS SIND:
      *    - ZR4-FT-DIALOG-STATUS-UNABH
      *
      *------------------------------------------------------------*
      * ZR4-FT-DIALOG-STATUS-UNABH                                 *
      *------------------------------------------------------------*
      * INPUT : Z-FTASTE                                           *
      *                                                            *
      * OUTPUT: FT-RC                                              *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - AUSFUEHREN VON AKTIVITÄTEN BEI PRUEFUNG DER FUNKTIONS- *
      *       TASTEN, DIE VOM DIALOG-STATUS UNABHÄNGIG SIND        *
      *                                                            *
      *------------------------------------------------------------*
       ZR4-FT-DIALOG-STATUS-UNABH SECTION.
       ZR4-ANF.
           MOVE 'ZR4  '                     TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET FTASTE-OK                    TO TRUE
170299*>>>>>
      *    BEI EURO-SWITCH VERARBEITUNG IST NUR F20 ERLAUBT
           EVALUATE TRUE

           WHEN CARD-EURO-SWITCH-UMGERECHNET

               EVALUATE Z-FTASTE
                  WHEN NOT DFHPF20

                    MOVE K-E                TO CARD-MELDRAHM-TYP
                    MOVE 33                 TO CARD-MELDRAHM-NR
                    SET FTASTE-ABBR         TO TRUE
                    MOVE K-CURSOR           TO ERTRANSL IN MFI

               WHEN OTHER
                    CONTINUE
               END-EVALUATE

           WHEN OTHER
                CONTINUE
           END-EVALUATE
      *
           EVALUATE TRUE
           WHEN CARD-EURO-SWITCH-JA
               SET CARD-EURO-SWITCH-OHNE-FEHLER TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE

      *    BEI EURO-SWITCH VERARBEITUNG DARF BEI BETäTIGEN VON F20
      *    KEIN WEITERES FELD VERäNDERT WERDEN
           EVALUATE TRUE

           WHEN CARD-EURO-SWITCH-URSPRUNG
               EVALUATE Z-FTASTE
                  WHEN DFHPF20

                   PERFORM UR8-EURO-MASKE-ENTSICHERN

                   EVALUATE S-BILDNR
                   WHEN K-1
                      MOVE M1O TO Z-EINBER
                   WHEN K-2
                      MOVE M2O TO Z-EINBER
                   WHEN K-3
                      MOVE M3O TO Z-EINBER
                   WHEN OTHER
                      CONTINUE
                   END-EVALUATE

      *            FEHLERMELDUNG, WENN F20 IN  VERBINDUNG MIT
      *            FELDäNDERUNG
                   PERFORM VARYING I-EURO-SWITCH FROM K-1 BY K-1
                      UNTIL I-EURO-SWITCH > 1904 OR
                            CARD-EURO-SWITCH-MIT-FEHLER
                      MOVE Z-EURO-SWITCH-EINBER(I-EURO-SWITCH:K-1) TO
                           Z-EURO-SWITCH-ZEICHEN
                           IF DARSTELLBARES-ZEICHEN AND
                              Z-EURO-SWITCH-EINBER(I-EURO-SWITCH:K-1)
                              NOT =
                              Z-EINBER            (I-EURO-SWITCH:K-1)

                              MOVE K-E        TO CARD-MELDRAHM-TYP
                              MOVE 34         TO CARD-MELDRAHM-NR
                              SET FTASTE-ABBR TO TRUE
                              MOVE K-CURSOR   TO ERTRANSL IN MFI
                              SET CARD-EURO-SWITCH-MIT-FEHLER TO TRUE

      *                       EINSTELLEN DER URSPRüNGLICHEN MASKE
                              EVALUATE S-BILDNR
                              WHEN K-1
                                 MOVE Z-EURO-SWITCH-EINBER TO M1O
                              WHEN K-2
                                 MOVE Z-EURO-SWITCH-EINBER TO M2O
                              WHEN K-3
                                 MOVE Z-EURO-SWITCH-EINBER TO M3O
                              WHEN OTHER
                                 CONTINUE
                              END-EVALUATE

                           END-IF
                   END-PERFORM

               WHEN OTHER
                    CONTINUE
               END-EVALUATE

           WHEN OTHER
                CONTINUE
           END-EVALUATE

170299*<<<<<
      *
           EVALUATE Z-FTASTE

           WHEN DFHPF12
      *    * ABFANGEN VON F12 (DIALOG-ABBRUCH)
      *    * (FALLS NICHT AUF OBERSTER DIALOG-EBENE)
200197*        IF CARD-DIALOG-SCHRITT-AKT NOT = CARR-DIALOG-AKT
200197        IF CARR-UANW > ZERO  OR
200197           CARR-PAGE > 1
200197        THEN
                 MOVE K-OP-F12                  TO CADR-E-OP
                 SET FTASTE-ABBR-VERZW-BILDLAST TO TRUE
              END-IF
      *

170299*>>>>>
           WHEN DFHPF20
      *    * ABFANGEN VON F20 (EURO-SWITCH)
              EVALUATE TRUE
              WHEN CARD-EURO-SWITCH-JA
                   SET DS-FUSS-VERSORGEN      TO TRUE
      *
                 EVALUATE TRUE

                 WHEN CARD-EURO-SWITCH-OHNE-FEHLER
      *
                    EVALUATE TRUE

                        WHEN CARD-EURO-SWITCH-URSPRUNG
                             SET CARD-EURO-SWITCH-UMRECHNEN TO TRUE
                        WHEN CARD-EURO-SWITCH-UMGERECHNET
                             SET CARD-EURO-SWITCH-ZURUECK TO TRUE
                        WHEN CARD-EURO-SWITCH-ZURUECK
                             CONTINUE
                        WHEN OTHER
                             CONTINUE

                    END-EVALUATE
      *
                 WHEN OTHER
                      CONTINUE
                 END-EVALUATE
      *
              WHEN OTHER
                   CONTINUE

              END-EVALUATE
170299*<<<<<
      *
           WHEN DFHPF21
      *    * ABFANGEN VON F21 (SPRUNG ZU OK-FELD)
      *    * (FALLS OK-FELD ANGEZEIGT WIRD)
              IF ABSCHL-ANZEIGE-N            OR
                 ABSCHL-ANZEIGE-J
              THEN
                 MOVE K-CURSOR              TO EROKL IN MFI
                 SET FTASTE-BILDAUSGABE     TO TRUE
              END-IF
      *
090301*>>>>>
           WHEN DFHPF22

                EVALUATE TRUE
                WHEN CTV-ANZEIGE

      *            * CTV
                     MOVE K-OP-CTV             TO CADR-E-OP
                     MOVE SPACE                TO CADR-E-TRANS-NEXT

                     PERFORM UR9-CTV-CADU-OB-ERMITTELN

                     SET OP-OK-VERZWEIGEN      TO TRUE
                     SET FTASTE-ABBR-VERZW-BILDLAST    TO TRUE

                WHEN OTHER

                     CONTINUE

                END-EVALUATE
090301*<<<<<
      *
151093     WHEN DFHENTER
151093*    * PRÜFEN OB INHALT VON EROKI = 'J' ODER 'N' IST
151093*    * (FALLS OK-FELD ANGEZEIGT WIRD)
151093        IF ABSCHL-ANZEIGE-N            OR
151093           ABSCHL-ANZEIGE-J
151093        THEN
151093           IF EROKI IN MFI NOT = K-J AND
151093                           NOT = K-N
151093              MOVE K-E                TO CARD-MELDRAHM-TYP
151093              MOVE 29                 TO CARD-MELDRAHM-NR
151093              MOVE K-CURSOR           TO EROKL IN MFI
151093              SET ABSCHL-ANZEIGE-UNVERAENDERT TO TRUE
151093              SET FTASTE-ABBR         TO TRUE
151093           END-IF
151093        END-IF

           END-EVALUATE
           .
       ZR4-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER DPAT0012 (VB: DIALOG-STATUS-UNABH) ***

