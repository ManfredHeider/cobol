      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  12.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAN0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAN0002.CPv  $
      *
      *   Rev 2.1   Jul 07 2020 11:27:28   u130377
      *AAN01460: Reorganisation/Umbau UWAN0001 auf Gesellschaftsmod
      *ule
      *
      *   Rev 2.0   Mar 12 2020 10:17:12   u130377
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01460!12.03.20!2.0 ! Erstellung                ! Töpfer *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Gesellschaftsunabhängige Routinensteuerung je nach
      *    Verarbeitungsvariante.
      *
      *
      ****************************************************************
      *
      *------------------------------------------------------------*
       X01-FUNKTION-DRAN0140 SECTION.
       X01-ANF.
      *------------------------------------------------------------*
      * STEUERUNG DER FUNKTION DRAN0140                            *
      *                                                            *
      *   - ADRESSIERUNG DER ZUGEHOERIGEN FUNKTIONS-PARAMETER      *
      *     PI-DRAN0140 und PO-DRAN0140                            *
      *   - ANSTEUERUNG DER ROUTINEN VORLAUF, VERARBEITUNG UND     *
      *     NACHLAUF                                               *
      *                                                            *
      *   - DER NACHLAUF MUSS IMMER DURCHGEFUEHRT WERDEN, UM DEN   *
      *     RC DER FUNKTION ZU BESCHICKEN                          *
      *                                                            *
      *------------------------------------------------------------*
      *
CCC009*    SET Z-POINTER              TO ADDRESS OF PI-EINGABE
CCC009*    SET ADDRESS OF PI-DRAN0140 TO Z-POINTER
CCC009*    SET Z-POINTER              TO ADDRESS OF PO-RUECKGABE
CCC009*    SET ADDRESS OF PO-DRAN0140 TO Z-POINTER
      *
           INITIALIZE PO-DRAN0140

           MOVE K-RC-NOT-FOUND                 TO Z-RC-1
           MOVE K-RC-PE-S-NICHT-ERMITTELT      TO Z-RC-2
           MOVE K-RC-VOR-NICHT-ERMITTELT       TO Z-RC-3
      *
           PERFORM X011-VORLAUF-DRAN0140
           PERFORM X012-VERARB-DRAN0140
           PERFORM X013-NACHLAUF-DRAN0140
      *
           .
       AAA-EXIT.
           EXIT.

      *------------------------------------------------------------*
       X011-VORLAUF-DRAN0140 SECTION.
       X011-ANF.
      *------------------------------------------------------------*
      * VORLAUF FUER FUNKTION DRAN0140
      *
      *  - PLAUSIBILITÄTSPRÜFUNG DER ÜBERGABEPARAMETER
      *  - SETZEN WORKFLOW AUF GRUNDWERT
      *------------------------------------------------------------
      *
           IF PGEB-GES IN PI-DRAN0140      =  ZERO
              MOVE 'ÜBERGABE-PARAMETER PGEB-GES UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-2                     TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           END-IF

           EVALUATE TRUE
      *    GÜLTIGE KOMBINATIONEN DER PI-FELDER
           WHEN PGEB-GES          IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-NR           IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-AVERM        IN PI-DRAN0140     = K-ZERO AND
                ANTA-VORSCHLAG-NR IN PI-DRAN0140     = SPACE
           WHEN PGEB-GES          IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-NR           IN PI-DRAN0140     = K-ZERO AND
                ANTA-AVERM        IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-VORSCHLAG-NR IN PI-DRAN0140     = SPACE
           WHEN PGEB-GES          IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-NR           IN PI-DRAN0140     = K-ZERO AND
                ANTA-AVERM        IN PI-DRAN0140     = K-ZERO AND
                ANTA-VORSCHLAG-NR IN PI-DRAN0140 NOT = SPACE
           WHEN PGEB-GES          IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-NR           IN PI-DRAN0140     = K-ZERO AND
                ANTA-AVERM        IN PI-DRAN0140 NOT = K-ZERO AND
                ANTA-VORSCHLAG-NR IN PI-DRAN0140 NOT = SPACE
      *
                CONTINUE
      *
           WHEN OTHER

              MOVE 'UNG. KOMBINATION DER PI-FELDER       '
                TO ERR-VAR-ZEILE01
              MOVE K-3                     TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT

           END-EVALUATE

           INITIALIZE                D-ERR-VAR-ZEILE04
           MOVE PGEB-GES          IN PI-DRAN0140
             TO Z-PI-PGEB-GES
                PGEB-GES          IN D-ERR-VAR-ZEILE04

           MOVE ANTA-NR           IN PI-DRAN0140
             TO Z-PI-NR
                ANTA-NR           IN D-ERR-VAR-ZEILE04

           MOVE ANTA-AVERM        IN PI-DRAN0140
             TO Z-PI-AVERM
                ANTA-AVERM        IN D-ERR-VAR-ZEILE04

           MOVE ANTA-VORSCHLAG-NR IN PI-DRAN0140
             TO Z-PI-VORSCHLAG-NR
                ANTA-VORSCHLAG-NR IN D-ERR-VAR-ZEILE04

           MOVE HVANTNUM-LFD      IN PI-DRAN0140
             TO Z-PI-HVANTNUM-LFD
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X013-NACHLAUF-DRAN0140 SECTION.
       X013-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-RC                                               *
      * OUTPUT: PO-DRAN0140-RC                                     *
      * VERARBEITUNG:                                              *
      *   - RC BESTÜCKEN                                           *
      *   - MASKENFELDER (-> DRAN0178) FÜLLEN                      *
      *------------------------------------------------------------*
      *
      *
           MOVE Z-RC-1           TO PO-DRAN0140-RC-1
           MOVE Z-RC-2           TO PO-DRAN0140-RC-2
           MOVE Z-RC-3           TO PO-DRAN0140-RC-3


           MOVE Z-PO-HINWEISTEXT TO PO-DRAN0140-HINWEISTEXT

           EVALUATE TRUE
           WHEN STAT-DIALOG

                PERFORM U02-MASKENFELDER-FUELLEN

           WHEN OTHER

                CONTINUE

           END-EVALUATE

           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X03-FUNKTION-DRAN0148 SECTION.
       X03-ANF.
      *------------------------------------------------------------*
      * STEUERUNG DER FUNKTION DRAN0148                            *
      *                                                            *
      *   - ADRESSIERUNG DER ZUGEHOERIGEN FUNKTIONS-PARAMETER      *
      *     PI-DRAN0147 UND PO-DRAN0148                            *
      *   - ANSTEUERUNG DER ROUTINEN VORLAUF, VERARBEITUNG UND     *
      *     NACHLAUF                                               *
      *                                                            *
      *   - DER NACHLAUF MUSS IMMER DURCHGEFUEHRT WERDEN, UM DEN   *
      *     RC DER FUNKTION ZU BESCHICKEN                          *
      *                                                            *
      *------------------------------------------------------------*
      *
CCC009*    SET Z-POINTER              TO ADDRESS OF PI-EINGABE
CCC009*    SET ADDRESS OF PI-DRAN0148 TO Z-POINTER
CCC009*    SET Z-POINTER              TO ADDRESS OF PO-RUECKGABE
CCC009*    SET ADDRESS OF PO-DRAN0148 TO Z-POINTER
      *
           PERFORM X031-VORLAUF-DRAN0148
      *
           IF Z-RC = K-RC-OK
              PERFORM X032-VERARB-DRAN0148
           END-IF
      *
           PERFORM X033-NACHLAUF-DRAN0148
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X031-VORLAUF-DRAN0148 SECTION.
       X031-ANF.
      *------------------------------------------------------------*
      * VORLAUF FUER FUNKTION DRAN0148
      *
      *  - PLAUSIBILITÄTSPRÜFUNG DER ÜBERGABEPARAMETER
      *  - SETZEN WORKFLOW AUF GRUNDWERT
      *------------------------------------------------------------*
      *
           IF PGEB-GES IN WF-ANTRAG-KOPF-NEU    =  K-ZERO
              MOVE 'ÜBERGABE-PARAMETER PGEB-GES IST LEER'
                TO ERR-VAR-ZEILE01
              MOVE K-8                      TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE PGEB-GES IN WF-ANTRAG-KOPF-NEU TO Z-PI-PGEB-GES
           END-IF
      *
           IF ANTA-NR  IN WF-ANTRAG-NEU    =  K-ZERO
              MOVE 'ÜBERGABE-PARAMETER ANTA-NR IST LEER'
                TO ERR-VAR-ZEILE01
              MOVE K-9                     TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE ANTA-NR  IN WF-ANTRAG-NEU TO Z-PI-NR
           END-IF
      *

      *    KONFIGURATION ZU PGEB ERMITTELN
           PERFORM U14-KONFIGURATION-JE-PGEB

           EVALUATE TRUE
           WHEN ANTA-PIN IN WF-ANTRAG-NEU = K-ZERO AND
                S-MIT-PIN

              MOVE 'ANTA-PIN IST LEER'
                TO ERR-VAR-ZEILE01
              MOVE K-10                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT

           WHEN ANTA-PIN IN WF-ANTRAG-NEU NOT = K-ZERO AND
                S-MIT-VNR

              MOVE 'ANTA-PIN IST NICHT LEER'
                TO ERR-VAR-ZEILE01
              MOVE K-11                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT

           WHEN OTHER

               CONTINUE

           END-EVALUATE

           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X033-NACHLAUF-DRAN0148 SECTION.
       X033-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-RC                                               *
      * OUTPUT: PO-DRAN0148-RC                                     *
      * VERARBEITUNG:                                              *
      *------------------------------------------------------------*
      *
           MOVE Z-RC             TO PO-DRAN0148-RC
           .
       AAA-EXIT.
           EXIT.
      *
       X04-FUNKTION-DRAN0149 SECTION.
       X04-ANF.
      *------------------------------------------------------------*
      * STEUERUNG DER FUNKTION DRAN0149                            *
      *                                                            *
      *   - ADRESSIERUNG DER ZUGEHOERIGEN FUNKTIONS-PARAMETER      *
      *     PI-DRAN0149 UND PO-DRAN0149                            *
      *   - ANSTEUERUNG DER ROUTINEN VORLAUF, VERARBEITUNG UND     *
      *     NACHLAUF                                               *
      *                                                            *
      *   - DER NACHLAUF MUSS IMMER DURCHGEFUEHRT WERDEN, UM DEN   *
      *     RC DER FUNKTION ZU BESCHICKEN                          *
      *                                                            *
      *------------------------------------------------------------*
      *
CCC009*    SET Z-POINTER              TO ADDRESS OF PI-EINGABE
CCC009*    SET ADDRESS OF PI-DRAN0149 TO Z-POINTER
CCC009*    SET Z-POINTER              TO ADDRESS OF PO-RUECKGABE
CCC009*    SET ADDRESS OF PO-DRAN0149 TO Z-POINTER
      *
           PERFORM X041-VORLAUF-DRAN0149
      *
           IF Z-RC = K-RC-OK
              PERFORM X042-VERARB-DRAN0149
           END-IF
      *
           PERFORM X043-NACHLAUF-DRAN0149
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X041-VORLAUF-DRAN0149 SECTION.
       X041-ANF.
      *------------------------------------------------------------*
      * VORLAUF FUER FUNKTION DRAN0149
      *
      *  - PLAUSIBILITÄTSPRÜFUNG DER ÜBERGABEPARAMETER
      *  - SETZEN WORKFLOW AUF GRUNDWERT
      *------------------------------------------------------------
      *
           IF PGEB-GES IN PI-DRAN0149      =  ZERO
              MOVE 'ÜBERGABE-PARAMETER PGEB-GES UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-16                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE PGEB-GES IN PI-DRAN0149  TO Z-PI-PGEB-GES
           END-IF
      *
           IF ANTA-NR IN PI-DRAN0149       =  ZERO
              MOVE 'ÜBERGABE-PARAMETER ANTA-NR UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-17                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE ANTA-NR IN PI-DRAN0149   TO Z-PI-NR
           END-IF

      *
      *    KONFIGURATION ZU PGEB ERMITTELN
           PERFORM U14-KONFIGURATION-JE-PGEB
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X043-NACHLAUF-DRAN0149 SECTION.
       X043-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-RC                                               *
      * OUTPUT: PO-DRAN0143-RC                                     *
      * VERARBEITUNG:                                              *
      *------------------------------------------------------------*
      *
           MOVE Z-RC             TO PO-DRAN0149-RC
           .
       AAA-EXIT.
           EXIT.
      *
       X05-FUNKTION-DRAN0188 SECTION.
       X05-ANF.
      *------------------------------------------------------------*
      * STEUERUNG DER FUNKTION DRAN0188                            *
      *                                                            *
      *   - ADRESSIERUNG DER ZUGEHOERIGEN FUNKTIONS-PARAMETER      *
      *     PI-DRAN0188 UND PO-DRAN0188                            *
      *   - ANSTEUERUNG DER ROUTINEN VORLAUF, VERARBEITUNG UND     *
      *     NACHLAUF                                               *
      *                                                            *
      *   - DER NACHLAUF MUSS IMMER DURCHGEFUEHRT WERDEN, UM DEN   *
      *     RC DER FUNKTION ZU BESCHICKEN                          *
      *                                                            *
      *------------------------------------------------------------*
      *
CCC009*    SET Z-POINTER              TO ADDRESS OF PI-EINGABE
CCC009*    SET ADDRESS OF PI-DRAN0188 TO Z-POINTER
CCC009*    SET Z-POINTER              TO ADDRESS OF PO-RUECKGABE
CCC009*    SET ADDRESS OF PO-DRAN0188 TO Z-POINTER
      *
           PERFORM X051-VORLAUF-DRAN0188
      *
           IF Z-RC = K-RC-OK
              PERFORM X052-VERARB-DRAN0188
           END-IF
      *
           PERFORM X053-NACHLAUF-DRAN0188
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X051-VORLAUF-DRAN0188 SECTION.
       X051-ANF.
      *------------------------------------------------------------*
      * VORLAUF FUER FUNKTION DRAN0188
      *
      *  - PLAUSIBILITÄTSPRÜFUNG DER ÜBERGABEPARAMETER
      *  - SETZEN WORKFLOW AUF GRUNDWERT
      *------------------------------------------------------------
      *
           IF PGEB-GES IN PI-DRAN0188      =  ZERO
              MOVE 'ÜBERGABE-PARAMETER PGEB-GES UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-24                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE PGEB-GES IN PI-DRAN0188  TO Z-PI-PGEB-GES
           END-IF
      *
           IF ANTA-NR IN PI-DRAN0188       =  ZERO
              MOVE 'ÜBERGABE-PARAMETER ANTA-NR UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-25                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE ANTA-NR IN PI-DRAN0188   TO Z-PI-NR
           END-IF
      *
      *    KONFIGURATION ZU PGEB ERMITTELN
           PERFORM U14-KONFIGURATION-JE-PGEB
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X053-NACHLAUF-DRAN0188 SECTION.
       X053-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-RC                                               *
      * OUTPUT: PO-DRAN0143-RC                                     *
      * VERARBEITUNG:                                              *
      *------------------------------------------------------------*
      *
           MOVE Z-RC             TO PO-DRAN0188-RC
           .
       AAA-EXIT.
           EXIT.
      *
       X06-FUNKTION-DRAN0162 SECTION.
       X06-ANF.
      *------------------------------------------------------------*
      * STEUERUNG DER FUNKTION DRAN0162                            *
      *                                                            *
      *   - ADRESSIERUNG DER ZUGEHOERIGEN FUNKTIONS-PARAMETER      *
      *     PI-DRAN0162 UND PO-DRAN0162                            *
      *   - ANSTEUERUNG DER ROUTINEN VORLAUF, VERARBEITUNG UND     *
      *     NACHLAUF                                               *
      *                                                            *
      *   - DER NACHLAUF MUSS IMMER DURCHGEFUEHRT WERDEN, UM DEN   *
      *     RC DER FUNKTION ZU BESCHICKEN                          *
      *                                                            *
      *------------------------------------------------------------*
      *
CCC009*    SET Z-POINTER              TO ADDRESS OF PI-EINGABE
CCC009*    SET ADDRESS OF PI-DRAN0162 TO Z-POINTER
CCC009*    SET Z-POINTER              TO ADDRESS OF PO-RUECKGABE
CCC009*    SET ADDRESS OF PO-DRAN0162 TO Z-POINTER
      *
           PERFORM X061-VORLAUF-DRAN0162
      *
           IF Z-RC = K-RC-OK
              PERFORM X062-VERARB-DRAN0162
           END-IF
      *
           PERFORM X063-NACHLAUF-DRAN0162
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X061-VORLAUF-DRAN0162 SECTION.
       X061-ANF.
      *------------------------------------------------------------*
      * VORLAUF FUER FUNKTION DRAN0162
      *
      *  - PLAUSIBILITÄTSPRÜFUNG DER ÜBERGABEPARAMETER
      *  - SETZEN WORKFLOW AUF GRUNDWERT
      *------------------------------------------------------------
      *
           IF PI-DRAN0162-PGEB-GES         =  ZERO
              MOVE 'ÜBERGABE-PARAMETER PGEB-GES UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-27                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE PI-DRAN0162-PGEB-GES     TO Z-PI-PGEB-GES
           END-IF
      *
           IF PI-DRAN0162-ANTA-NR          =  ZERO
              MOVE 'ÜBERGABE-PARAMETER ANTA-NR UNGUELTIG'
                TO ERR-VAR-ZEILE01
              MOVE K-28                    TO ERR-ORT-LFD
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           ELSE
             MOVE PI-DRAN0162-ANTA-NR      TO Z-PI-NR
           END-IF
      *
      *    KONFIGURATION ZU PGEB ERMITTELN
           PERFORM U14-KONFIGURATION-JE-PGEB
      *
           .
       AAA-EXIT.
           EXIT.
      *
       X063-NACHLAUF-DRAN0162 SECTION.
       X063-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-RC                                               *
      * OUTPUT: PO-DRAN0162-RC                                     *
      * VERARBEITUNG:                                              *
      *------------------------------------------------------------*
      *
           MOVE Z-RC             TO PO-DRAN0162-RC
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0121-ERMITTELN-ANTRAG SECTION.
       X0121-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-PI-PGEB-GES                                      *
      *         Z-PI-NR                                            *
      * OUTPUT: WF-ANTRAG-...                                      *
      * VERARBEITUNG:                                              *
      *   - LESEN ANTRAG ÜBER ANTRAGSNUMMER                        *
      *------------------------------------------------------------*
      *
           MOVE Z-PI-PGEB-GES TO PGEB-GES IN PI-DRAN0124

           MOVE Z-PI-NR TO ANTA-NR  IN PI-DRAN0124

           SET  PI-ZTAN0008-DRAN0124-SEL  TO TRUE
      *
CCCPY1*    CALL K-ZTAN0008 USING BY
CCC004*         REFERENCE PI-ZTAN0008
CCC004*         PI-DRAN0124
CCC004*         PO-DRAN0124
CCC004*         ERR-ERROR-BEREICH
CCC004*         STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
      *    RETURNCODE ABFRAGEN
           EVALUATE TRUE
           WHEN PO-DRAN0124-RC-OK

                MOVE K-RC-OK TO Z-RC-1
                MOVE K-RC-PE-S-ERMITTELT TO Z-RC-2
      *
                MOVE PO-DRAN0124-ANTRAG-KOPF
                  TO WF-ANTRAG-KOPF-NEU
                  IN WF-ANTRAG-NEU
                MOVE ANTA-LAST-UPDATE
                  IN PO-DRAN0124
                  TO WF-ANTA-LAST-UPDATE-NEU
                  IN WF-ANTRAG-NEU
                MOVE PO-DRAN0124-ANTRAG-PE-STRUKTUR
                  TO WF-ANTRAG-PE-STRUKTUR-NEU
                  IN WF-ANTRAG-NEU

           WHEN PO-DRAN0124-RC-NOT-FOUND

      *         BEI PIN-GES ÜBER SCHNITTSTELLENTABELLE VERSUCHEN
                EVALUATE TRUE
                WHEN S-MIT-PIN AND
                     Z-PI-NR <= 9999999
      *              PIN-GESELLSCHAFT UND ZUGRIFF ÜBER PIN

                     MOVE Z-PI-NR TO ANTA-PIN IN WF-ANTRAG-NEU

                     PERFORM U60-ZTAN0008-DRAN0187
                     EVALUATE TRUE
                     WHEN PO-DRAN0187-RC-OK
      *                   OK
                          MOVE K-RC-OK TO Z-RC-1
                          MOVE K-RC-PE-S-ERMITTELT TO Z-RC-2
      *
                          MOVE PO-DRAN0187-ANTRAG-KOPF
                            TO WF-ANTRAG-KOPF-NEU IN WF-ANTRAG-NEU
                          MOVE ANTA-LAST-UPDATE IN PO-DRAN0187
                            TO WF-ANTA-LAST-UPDATE-NEU IN WF-ANTRAG-NEU
                          MOVE PO-DRAN0187-ANTRAG-PE-STRUKTUR
                            TO WF-ANTRAG-PE-STRUKTUR-NEU
                            IN WF-ANTRAG-NEU
      *
                    END-EVALUATE

                WHEN OTHER

                     CONTINUE

                END-EVALUATE
      *
           WHEN OTHER
                MOVE 'ZTAN0008/DRAN0124: RETURNCOCE UNGUELTIG'
                                              TO ERR-VAR-ZEILE01
                MOVE PO-DRAN0124-RC           TO ERR-VAR-ZEILE02
                MOVE D-ERR-VAR-ZEILE03        TO ERR-VAR-ZEILE03
                MOVE D-ERR-VAR-ZEILE04        TO ERR-VAR-ZEILE04
                MOVE K-4                      TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT
           END-EVALUATE
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0122-ERMITTELN-PRODUKTE SECTION.
       X0122-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-PGEB-GES                                         *
      *         WF-ANTRAG-NEU                                      *
      * OUTPUT: WF-ANTRAG-...                                      *
      * VERARBEITUNG:                                              *
      *   - LESEN ANTP-SÄTZE (SOVIEL WIE ES GIBT)                  *
      *------------------------------------------------------------*
      *
      **** CUSOR ÖFFNEN U. ERSTEN SATZ LESEN
           MOVE Z-PI-PGEB-GES
             TO PGEB-GES IN PI-DRAN0137
           MOVE ANTA-NR  IN WF-ANTRAG-KOPF-NEU
             TO ANTP-NR  IN PI-DRAN0137

           MOVE K-PGM-NAME
             TO PI-DRAN0137-PROGRAMM
           MOVE K-LIST-FIRST
             TO PI-DRAN0137-UFKT
            SET PI-ZXAN0009-DRAN0137     TO TRUE
      *
CCCPY1*    CALL K-ZXAN0009 USING BY REFERENCE PI-ZXAN0009
CCC004*                                       PI-DRAN0137
CCC004*                                       PO-DRAN0137
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
      *    RETURNCODE ABFRAGEN
           EVALUATE TRUE
           WHEN PO-DRAN0137-RC-OK
      ***  ERSTER SATZ IN AUSGANGSDATEN EINSTELLEN
                MOVE PO-DRAN0137-ANTRAG-PRODUKT
                  TO WF-ANTRAG-PRODUKT-TABELLE-NEU IN WF-ANTRAG-NEU(K-1)
      *
           WHEN PO-DRAN0137-RC-LEER
                MOVE  K-RC-OK-PEWA-O-PROD TO Z-RC-1
      *
           WHEN OTHER
                MOVE 'ZXAN0009/DRAN0137 -FIRST-: RETURNCOCE UNGUELTIG'
                                              TO ERR-VAR-ZEILE01
                MOVE PO-DRAN0137-RC           TO ERR-VAR-ZEILE02
                MOVE D-ERR-VAR-ZEILE03        TO ERR-VAR-ZEILE03
                MOVE D-ERR-VAR-ZEILE04        TO ERR-VAR-ZEILE04
                MOVE K-5                      TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT
           END-EVALUATE
      *
      *
      **** WEITERE VORHANDENE SÄTZE LESEN
           PERFORM X0123-VERARB-DRAN0137 UNTIL
                   PO-DRAN0137-RC NOT = K-RC-OK
      *
      **** CURSOR SCHLIEßEN
           MOVE K-LIST-QUIT TO PI-DRAN0137-UFKT
      *
CCCPY1*    CALL K-ZXAN0009 USING BY REFERENCE PI-ZXAN0009
CCC004*                                       PI-DRAN0137
CCC004*                                       PO-DRAN0137
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
      *    RETURNCODE ABFRAGEN
           EVALUATE TRUE
               WHEN PO-DRAN0137-RC-OK

                    CONTINUE
      *
               WHEN OTHER
                  MOVE 'ZXAN0009/DRAN0137 -QUIT-: RETURNCOCE UNGUELTIG'
                    TO ERR-VAR-ZEILE01
                  MOVE PO-DRAN0137-RC      TO ERR-VAR-ZEILE02
                  MOVE D-ERR-VAR-ZEILE03        TO ERR-VAR-ZEILE03
                  MOVE D-ERR-VAR-ZEILE04        TO ERR-VAR-ZEILE04
                  MOVE K-6                 TO ERR-ORT-LFD
                  PERFORM XR93-UPRO-PROG-FEHLER
                  GO TO XST-EXIT
            END-EVALUATE
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0123-VERARB-DRAN0137 SECTION.
       X0123-ANF.
      *------------------------------------------------------------*
      * INPUT : PGEB-GES IN PI-DRAN0140                            *
      *         ANTA-NR  IN PI-DRAN0140                            *
      * OUTPUT: WF-ANTRAG-...                                      *
      * VERARBEITUNG:                                              *
      *   - LESEN ANTP-SÄTZE (SOVIEL WIE ES GIBT)                  *
      *     UND EINSTELLEN IN TABELLE PRODUKTE IN AUSGANGSDATEN    *
      *------------------------------------------------------------*
      *
      ***  WEITERE VORHANDENE PRODUKTE EINSTELLEN
           PERFORM VARYING I-PROD FROM K-2 BY K-1 UNTIL I-PROD > K-15
                OR PO-DRAN0137-RC NOT = K-RC-OK
      *
                   MOVE K-LIST-NEXT TO PI-DRAN0137-UFKT
      *
CCCPY1*            CALL K-ZXAN0009 USING BY REFERENCE PI-ZXAN0009
CCC004*                                               PI-DRAN0137
CCC004*                                               PO-DRAN0137
CCC004*                                               ERR-ERROR-BEREICH
CCC004*                                         STANDARD-STATUS-BEREICH
CCCPY2*            END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
                   IF ERR-RC-SCHWERER-FEHLER
                      GO TO XST-EXIT
                   END-IF
      *
      *    RETURNCODE ABFRAGEN
                   EVALUATE TRUE
                       WHEN PO-DRAN0137-RC-OK
                          MOVE PO-DRAN0137-ANTRAG-PRODUKT
                            TO WF-ANTRAG-PRODUKT-TABELLE-NEU
                            IN WF-ANTRAG-NEU(I-PROD)
      *
                       WHEN PO-DRAN0137-RC-EOF
                            CONTINUE
      *
                       WHEN OTHER
                          MOVE 'ZXAN0009/DRAN0137 - NEXT -: RETURNCOCE U
      -                   'NGUELTIG'               TO ERR-VAR-ZEILE01
                          MOVE PO-DRAN0137-RC      TO ERR-VAR-ZEILE02
                          MOVE D-ERR-VAR-ZEILE03
                            TO ERR-VAR-ZEILE03
                          MOVE D-ERR-VAR-ZEILE04
                            TO ERR-VAR-ZEILE04
                          MOVE K-7                 TO ERR-ORT-LFD
                          PERFORM XR93-UPRO-PROG-FEHLER
                          GO TO XST-EXIT
                    END-EVALUATE
      *
           END-PERFORM
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0321-ANTRAG-SPEICHERN SECTION.
       X0321-ANF.
      *------------------------------------------------------------*
      * INPUT : Z-PI-PGEB-GES                                      *
      *         Z-PI-NR                                            *
      * OUTPUT: WF-ANTRAG-...                                      *
      * VERARBEITUNG:                                              *
      *   - LESEN ANTA-SATZ, WENN VORHANDEN NIX TUN SONST NEU      *
      *     SPEICHERN                                              *
      *------------------------------------------------------------*
      *
           MOVE Z-PI-PGEB-GES
             TO PGEB-GES IN PI-DRAN0124
           MOVE Z-PI-NR
             TO ANTA-NR  IN PI-DRAN0124

           SET PI-ZTAN0008-DRAN0124-SEL TO TRUE
      *
CCCPY1*    CALL K-ZTAN0008 USING BY REFERENCE PI-ZTAN0008
CCC004*                                       PI-DRAN0124
CCC004*                                       PO-DRAN0124
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
      *    RETURNCODE ABFRAGEN
           EVALUATE TRUE
           WHEN PO-DRAN0124-RC-OK
      *    ANTRAG VORHANDEN
                MOVE K-RC-DUPLICATE-VALUE TO Z-RC
                MOVE 'ANTRAG BEREITS IM BESTAND VORHANDEN!'
                  TO Z-PO-HINWEISTEXT

                GO TO AAA-EXIT
      *
           WHEN PO-DRAN0124-RC-NOT-FOUND
      *    ANTRAG NICHT VORHANDEN SPEICHERN
                PERFORM X0322-ANTRAG-SPEICHERN
                MOVE K-RC-OK             TO Z-RC
      *
           WHEN OTHER
                MOVE 'ZTAN0008/DRAN0124: RETURNCOCE UNGUELTIG'
                                              TO ERR-VAR-ZEILE01
                MOVE PO-DRAN0124-RC           TO ERR-VAR-ZEILE02
                MOVE D-ERR-VAR-ZEILE03
                  TO ERR-VAR-ZEILE03
                MOVE D-ERR-VAR-ZEILE04
                  TO ERR-VAR-ZEILE04
                MOVE K-12                     TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT
           END-EVALUATE
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0322-ANTRAG-SPEICHERN SECTION.
       X0322-ANF.
      *------------------------------------------------------------*
      * INPUT : PGEB-GES IN PI-DRAN0148                            *
      *         VNR      IN PI-DRAN0148                            *
      *         WF-ANTRAG-...                                      *
      * OUTPUT: RC                                                 *
      * VERARBEITUNG:                                              *
      *   - INSERT DATENWORKFLOW IN DATENBESTÄNDE                  *
      *------------------------------------------------------------*
      *
      *    UMWANDELN IN GROSS-KLEINBUCHSTABEN
           MOVE ANTA-KUNDE-NAME
             IN WF-ANTRAG-NEU
             TO TXT-TEXT
           MOVE LENGTH
             OF ANTA-KUNDE-NAME
             IN WF-ANTRAG-NEU
             TO TXT-LAENGE
           PERFORM U61-UPAT0035
           MOVE TXT-TEXT
             TO ANTA-KUNDE-NAME
             IN WF-ANTRAG-NEU

      *    UMWANDELN IN GROSS-KLEINBUCHSTABEN
           MOVE ANTA-KUNDE-VNAME
             IN WF-ANTRAG-NEU
             TO TXT-TEXT
           MOVE LENGTH
             OF ANTA-KUNDE-VNAME
             IN WF-ANTRAG-NEU
             TO TXT-LAENGE
           PERFORM U61-UPAT0035
           MOVE TXT-TEXT
             TO ANTA-KUNDE-VNAME
             IN WF-ANTRAG-NEU

      *    UMWANDELN IN GROSS-KLEINBUCHSTABEN
           MOVE ANTA-ORT
             IN WF-ANTRAG-NEU
             TO TXT-TEXT
           MOVE LENGTH
             OF ANTA-ORT
             IN WF-ANTRAG-NEU
             TO TXT-LAENGE
           PERFORM U61-UPAT0035
           MOVE TXT-TEXT
             TO ANTA-ORT
             IN WF-ANTRAG-NEU

      *    UMWANDELN IN GROSS-KLEINBUCHSTABEN
           MOVE ANTA-KUNDE-BGRP
             IN WF-ANTRAG-NEU
             TO TXT-TEXT
           MOVE LENGTH
             OF ANTA-KUNDE-BGRP
             IN WF-ANTRAG-NEU
             TO TXT-LAENGE
           PERFORM U61-UPAT0035
           MOVE TXT-TEXT
             TO ANTA-KUNDE-BGRP
             IN WF-ANTRAG-NEU
 
      *    UMWANDELN IN GROSS-KLEINBUCHSTABEN
           MOVE ANTA-KUNDE-BRAN
             IN WF-ANTRAG-NEU
             TO TXT-TEXT
           MOVE LENGTH
             OF ANTA-KUNDE-BRAN
             IN WF-ANTRAG-NEU
             TO TXT-LAENGE
           PERFORM U61-UPAT0035
           MOVE TXT-TEXT
             TO ANTA-KUNDE-BRAN
             IN WF-ANTRAG-NEU
      *

      *
           EVALUATE TRUE
           WHEN PI-UWAN0001-DRAN0149
           WHEN PI-UWAN0001-DRAN0188

                MOVE WF-ANTRAG-KOPF-NEU        IN WF-ANTRAG-NEU
                  TO PI-DRAN0323-ANTRAG-KOPF
                MOVE WF-ANTRAG-PE-STRUKTUR-NEU IN WF-ANTRAG-NEU
                  TO PI-DRAN0323-ANTRAG-PE-STRUKTUR
                 SET PI-ZTAN0008-DRAN0323-INS-ERSTZ  TO TRUE
CCCPY1*         CALL K-ZTAN0008 USING BY REFERENCE PI-ZTAN0008
CCC004*                                            PI-DRAN0323
CCC004*                                            PO-DRAN0323
CCC004*                                            ERR-ERROR-BEREICH
CCC004*                                         STANDARD-STATUS-BEREICH
CCCPY2*         END-CALL
                IF ERR-RC-SCHWERER-FEHLER
                   GO TO XST-EXIT
                END-IF
      *
                EVALUATE TRUE
                WHEN PO-DRAN0323-RC-OK
      *
                     PERFORM X0323-ANTRAG-PROD-SPEICHERN
      *
                WHEN OTHER
                     MOVE 'ZTAN0008/DRAN0323: RETURNCOCE UNGUELTIG'
                                                   TO ERR-VAR-ZEILE01
                     MOVE PO-DRAN0323-RC           TO ERR-VAR-ZEILE02
                     MOVE D-ERR-VAR-ZEILE03
                       TO ERR-VAR-ZEILE03
                     MOVE D-ERR-VAR-ZEILE04
                       TO ERR-VAR-ZEILE04
                     MOVE K-13                     TO ERR-ORT-LFD
                     PERFORM XR93-UPRO-PROG-FEHLER
                     GO TO XST-EXIT
                END-EVALUATE

           WHEN OTHER
      *
                MOVE WF-ANTRAG-KOPF-NEU        IN WF-ANTRAG-NEU
                  TO PI-DRAN0121-ANTRAG-KOPF
                MOVE WF-ANTRAG-PE-STRUKTUR-NEU IN WF-ANTRAG-NEU
                  TO PI-DRAN0121-ANTRAG-PE-STRUKTUR
                 SET PI-ZTAN0008-DRAN0121-INS  TO TRUE
      *
CCCPY1*         CALL K-ZTAN0008 USING BY REFERENCE PI-ZTAN0008
CCC004*                                            PI-DRAN0121
CCC004*                                            PO-DRAN0121
CCC004*                                            ERR-ERROR-BEREICH
CCC004*                                        STANDARD-STATUS-BEREICH
CCCPY2*         END-CALL
      *
      *  -      AUSWERTEN RETURNCODE
      *
                IF ERR-RC-SCHWERER-FEHLER
                   GO TO XST-EXIT
                END-IF
      *
      *         RETURNCODE ABFRAGEN
                EVALUATE TRUE
                WHEN PO-DRAN0121-RC-OK
      *
                     PERFORM X0323-ANTRAG-PROD-SPEICHERN
      *
                WHEN OTHER
                     MOVE 'ZTAN0008/DRAN0121: RETURNCOCE UNGUELTIG'
                                                   TO ERR-VAR-ZEILE01
                     MOVE PO-DRAN0121-RC           TO ERR-VAR-ZEILE02
                     MOVE D-ERR-VAR-ZEILE03
                       TO ERR-VAR-ZEILE03
                     MOVE D-ERR-VAR-ZEILE04
                       TO ERR-VAR-ZEILE04
                     MOVE K-13                     TO ERR-ORT-LFD
                     PERFORM XR93-UPRO-PROG-FEHLER
                     GO TO XST-EXIT
                END-EVALUATE

           END-EVALUATE
      *
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0323-ANTRAG-PROD-SPEICHERN SECTION.
       X0323-ANF.
      *------------------------------------------------------------*
      * INPUT : PGEB-GES IN PI-DRAN0148                            *
      *         VNR      IN PI-DRAN0148                            *
      *         WF-ANTRAG-...                                      *
      * OUTPUT: RC                                                 *
      * VERARBEITUNG:                                              *
      *   - INSERT DATENWORKFLOW (PRODUKTE) IN DATENBESTÄNDE       *
      *------------------------------------------------------------*
      *
           MOVE K-ZERO
             TO Z-ANTP-LFD-NR

           PERFORM VARYING I-PROD FROM K-1 BY K-1 UNTIL
              I-PROD > K-15
              ADD K-1 TO I-PROD GIVING I-PROD-START


              EVALUATE TRUE
              WHEN PGEB-GES IN WF-ANTRAG-PRODUKTE-NEU(I-PROD)
                   >  K-ZERO

                   MOVE ANTA-NR
                     IN WF-ANTRAG-KOPF-NEU
                     TO ANTP-NR
                     IN WF-ANTRAG-NEU(I-PROD)
      *
                   EVALUATE TRUE
                   WHEN ANTP-EINH-AVERM
                     IN WF-ANTRAG-NEU(I-PROD)
                      > K-MAXIMALE-EINHEITEN
                        MOVE K-MAXIMALE-EINHEITEN
                          TO ANTP-EINH-AVERM
                          IN WF-ANTRAG-NEU(I-PROD)
                   END-EVALUATE
      *
                   MOVE WF-ANTRAG-PRODUKT-TABELLE-NEU
                     IN WF-ANTRAG-NEU(I-PROD)
                     TO PI-DRAN0127-ANTRAG-PRODUKT

      *            KONSTANT EURO ALS WÄHRUNG
                   MOVE K-KNR-WAEHRUNG-EURO
                     TO KNR-WAEHRUNG
                     IN PI-DRAN0127

      **           LFD.NR. HOCHZÄHLEN
                   ADD K-1 TO Z-ANTP-LFD-NR
                   MOVE Z-ANTP-LFD-NR
                     TO ANTP-LFD-NR   IN PI-DRAN0127

                   PERFORM X0324-ANTRAG-PRODUKT-SPEICHERN

              END-EVALUATE

           END-PERFORM
      *
           .
      *
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       X0324-ANTRAG-PRODUKT-SPEICHERN SECTION.
       X0324-ANF.
      *------------------------------------------------------------*
      * INPUT : PGEB-GES IN PI-DRAN0148                            *
      *         VNR      IN PI-DRAN0148                            *
      *         WF-ANTRAG-...                                      *
      * OUTPUT: RC                                                 *
      * VERARBEITUNG:                                              *
      *   - INSERT DATENWORKFLOW (PRODUKTE) IN DATENBESTÄNDE       *
      *------------------------------------------------------------*
      *
           SET PI-ZTAN0009-DRAN0127-INS TO TRUE
      *
CCCPY1*    CALL K-ZTAN0009 USING BY REFERENCE PI-ZTAN0009
CCC004*                                       PI-DRAN0127
CCC004*                                       PO-DRAN0127
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
      *  - AUSWERTEN RETURNCODE
      *
           IF ERR-RC-SCHWERER-FEHLER
              GO TO XST-EXIT
           END-IF
      *
      *    RETURNCODE ABFRAGEN
           EVALUATE TRUE
           WHEN PO-DRAN0127-RC-OK
                CONTINUE
      *
           WHEN OTHER
                MOVE 'ZTAN0009/DRAN0127: RETURNCOCE UNGUELTIG'
                                              TO ERR-VAR-ZEILE01
                MOVE PO-DRAN0127-RC           TO ERR-VAR-ZEILE02
                MOVE D-ERR-VAR-ZEILE03
                  TO ERR-VAR-ZEILE03
                MOVE D-ERR-VAR-ZEILE04
                  TO ERR-VAR-ZEILE04
                MOVE K-14                     TO ERR-ORT-LFD
                PERFORM XR93-UPRO-PROG-FEHLER
                GO TO XST-EXIT
           END-EVALUATE
           .
      *
       AAA-EXIT.
           EXIT.
      *
      * ---------------------------------------------------
      * DPAN0002-ENDE
      * ---------------------------------------------------
      *

