      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0011.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0011.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 20 1997 16:59:16   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   Nov 04 1995 08:12:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0011      VB: DIALOG-STATUS-ABH BEARB.    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    12.03.1993                                       *
      *****************************************************************
      *    VERSION   02   16.09.93                                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.93! 01 ! NEU                       ! SCHOELLH.*
      *         !15.09.93! 02 ! KOMMENTAR KORRIGIERT      ! SCHOELLH.*
      * AFPA0002!20.01.97! 03 ! ANPASSUNG AN OP-DPN/OP-DPP! SCHÖLLH. *
      *         !        !    !  -IMPLEMENT. IN PGAT0001  !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
      * ZR2-FT-DIALOG-STATUS-ABH                                   *
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
       ZR2-FT-DIALOG-STATUS-ABH SECTION.
       ZR2-ANF.
           MOVE 'ZR2  '                      TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET FTASTE-OK                     TO TRUE

           EVALUATE Z-FTASTE
           WHEN DFHENTER
              EVALUATE TRUE
      *       ---------------------------------------------------- *
      *       TRANS + DATFRG PRUEFEN AUF OBERSTER DIALOG-EBENE     *
      *       VOR ODER NACH EINER BEARBEITUNG                      *
      *       ---------------------------------------------------- *
              WHEN ERTRANSI IN MFI NOT = SPACE
200197*           IF CARD-DIALOG-SCHRITT-AKT = CARR-DIALOG-AKT AND
200197           IF CARR-UANW = ZERO                          AND
200197              CARR-PAGE = 1                             AND
                    (DIALOG-STATUS-INI            OR
                     DIALOG-STATUS-ABGESCHLOSSEN    )

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
      * ZR3-FUSS-DIALOG-STATUS-ABH                                 *
      *------------------------------------------------------------*
      * INPUT : CADD-DIALOG-STATUS                                 *
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
       ZR3-FUSS-DIALOG-STATUS-ABH SECTION.
       ZR3-ANF.
           MOVE 'ZR3  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           IF DIALOG-STATUS-ABGESCHLOSSEN
              MOVE K-CTV                     TO ARCTVTXO IN MFO
              MOVE DFHBMASK                  TO ARCTVTXA IN MFI
              MOVE SPACE                     TO ERCTVO   IN MFO
              MOVE DFHBMFSE                  TO ERCTVA   IN MFI
           END-IF
           .
       ZR3-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER DPAT0011 (VB: DIALOG-STATUS-ABH) *****

