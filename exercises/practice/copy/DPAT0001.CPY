      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0001.CPY  $
      *    $Revision:   2.12  $
      *
      *    $LOG:   L:\MF\CPY\DPAT0001.CPv  $
      *
      *   Rev 2.2   APR 12 1999 14:44:10   T13AE34
      *AAT00005-PVCS-Kopf
      *
      *   Rev 2.1   APR 06 1999 14:44:10   T13AE34
      *AAT00005-Euro-Switch
      *
      *   Rev 2.0   MAR 01 1999 14:44:10   T13AE34
      *NT-UMSTELLUNG
      *
      *   Rev 1.8   13 NOV 1998 16:53:04   T13EX26
      *PEG-PRODUKTIONSÜBERGABE
      *
      *   Rev 1.7   27 AUG 1997 17:53:54   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.6   10 JUL 1997 12:37:34   T13AE34
      *AFPA0021 - S. LETZTER PUT
      *
      *   Rev 1.5   08 JUL 1997 08:52:08   T13AE34
      *AFPA0021 - CICS-NAME / VERZWEIGUNG VORSCHALT-PG
      *
      *   Rev 1.4   MAR 20 1997 16:55:50   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.3   MAR 26 1996 16:51:02   T13EX11
      *AFVB0226  NEU: DIALOG-DWL-AUFRUF
      *
      *   Rev 1.2   MAR 07 1996 14:47:44   T13AE17
      *AFAN0096   ANPASSUNG WB
      *
      *   Rev 1.1   NOV 15 1995 16:37:12   T13EX11
      *AFVB0226 FÜLLEN VARIABLE FEHLERINFOS IN
      *         FEHLERMELDUNGEN
      *
      *   Rev 1.0   NOV 04 1995 08:07:22   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    COPY-MEMBER: DPAT0001       DIALOG-RAHMENSTEUERUNG         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    15.02.1993                                       *
      *****************************************************************
      *    VERSION   7    07.03.96 15:30:22
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.93! 01 ! NEU                       ! SCHOELLH.*
      *         !17.05.93! 02 ! FEHLERBEREINIGUNG         ! SCHOELLH.*
      *         !27.08.93!    ! AUFRUF VON R31 UNABH. VON ! SCHOELLH.*
      *         !        !    !  DIALOGEBENE              !          *
      *         !15.09.93!    ! ÄND AUFRUF UPVB0003 BEI FT! SCHOELLH.*
      *         !15.10.93!    ! ABSCHL-ANZEIGE-UNVERAEND. ! SCHÖLLH. *
      *         !        !    !  JETZT ALS UNABH. FELD    !          *
      *         !15.10.93! 03 ! PROD.ÜBERGABE             ! HECKER   *
      * AFRZ0013!26.08.94! 04 ! KUS-SYSTEM / RACF         ! WIECHERS *
      * AFVB0163!06.01.95!    ! EING. EROKI und ertrans   ! SCHÖLLH. *
      *         !        !    !  AUF GROSS UMGESTELLT     !          *
      *         !10.01.95! 05 ! PRODUKTIONSÜBERGABE       ! GIESE    *
      * FEVB0218!03.03.95!    ! DISPLAY INFOS IN ZEILE 21;! SCHÖLLH. *
      *         !        !    ! UMST. KLEIN-GROSS GEÄNDERT!          *
      *         !10.03.95! 06 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0182!15.03.95!    ! DIALOG-ANBINDUNG ZUR CTV; ! SCHÖLLH. *
      *         !28.04.95! 07 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0226!27.09.95!    ! FÜLLEN VARIABLER WERTE IN ! SCHÖLLH. *
      *         !        !    ! FEHLERMELDUNGEN           !          *
      *         !07.11.95! 08 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFAN0096!07.03.96! 09 ! TRACE FUER R, R1, R11, R12!          *
      *         !        !    ! AUSGESTERNT; KLEINE KORR. ! KÜKEN.   *
      * AFVB0226!02.02.96! 10 ! EINBAU 'DIALOG-DWL-AUFRUF'! SCHÖLLH  *
      * AFPA0002!21.01.97! 11 ! EINBAU EXIT-SECTIONS FüR  ! AUERBACH *
      *         !        !    ! USERDATEN-SICHERUNG       !          *
      *         !20.01.97!    ! INITIALISIERUNGEN,        ! SCHÖLLH  *
      *         !        !    ! ANPASSUNG AN OP-DPN/OP-DPP!          *
      *         !        !    !  -IMPLEMENT. IN PGAT0001, !          *
      *         !        !    ! UR2-OP-TAB-INI NEU        !          *
      *         !03.02.97!    ! MELDUNGS-TYP = 'V' FüR DIE! AUERBACH *
      *         !        !    ! ANZEIGE 'VSTATUS'         !          *
      *         !        !    ! MELDUNGS-TYP = 'B' (BLINK)!          *
      *         !        !    ! AUSGEBAUT                 !          *
      *         !        !    ! UPAT0003 DYNAMISCHER CALL !          *
      * FEVB0304!07.02.97! 11 ! ABFANGEN PA1/2/3: IGNORIE-! SCHÖLLH  *
      *         !        !    !  REN (-> DIREKT RETURN)   !          *
      * AFPA0021!04.07.97! 12 ! CICS-NAME ERMITTELT AUS   ! HEIDER   *
      *         !        !    ! ZWAT0008/FKT. 0143        !          *
      * AFPA0022!21.08.98!    ! R61- UND R613- UMGEBAUT   ! OPRITZ   *
      * AFPA0022!21.08.98!    ! R614-          NEU        ! OPRITZ   *
      *         !        !    ! R614-          NEU        !          *
      *         !        !    ! CALL UPAT0018  NEU        !          *
      * APA00012!27.07.98!    ! SPEICHERUNG DER ADRESSEN  ! AUERBACH *
      *         !        !    ! VON DFHEIBLK UND DFHCOMM  !          *
      * AAT00005!17.02.99!1.9 ! EURO-SWITCH IN NEUDIALOGEN! HEIDER   *
      * ACV00015!09.03.01!2.3 ! CTV AUS NEUDIALOGEN       ! HEIDER   *
      * FCV00004!10.07.01!2.3 ! CTV -> CADU-OB NICHT AUS  ! HEIDER   *
      *         !        !    ! BVB0015                   !          *
      * ACV00054!31.01.06!2.4 ! CTV -> CADD-CTV-MELDUNG   ! HEIDER   *
      * ACV00054!06.03.06!2.3 ! CTV-MELDUNG 8-STELLIG     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *------------------------------------------------------------*
      * STEUERUNG                                                  *
      *------------------------------------------------------------*
      * INPUT : TCTUA, CA, TS-QUEUE(MAPS), AKT. MAP, DFHEIB        *
      *                                                            *
      * OUTPUT: CA, TS-QUEUE(MAPS)                                 *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     STEUERUNG DER BEARBEITUNG EINES DIALOGSCHRITTS         *
      *                                                            *
      *                                                            *
      *------------------------------------------------------------*
       R-RAHMENSTEUERUNG SECTION.
       R-ANF.
      *    * ERST NACH 'MOVE CARU-TRACE TO ERR-TRACE' SINNVOLL
070396*    MOVE 'R    ' TO ERR-ORT-SEC
070396*    PERFORM ZR1-TRACE-KURZ
      *
           PERFORM R1-EINSTIEG

           IF DS-BILDEINGABE
              PERFORM R2-BILDEINGABE
           END-IF

           IF DS-FTASTE-BEHANDELN
              PERFORM R3-FTASTE-BEHANDELN
           END-IF

           IF DS-DIALOG-INIT
              PERFORM R4-DIALOG-INIT
           END-IF

           IF DS-BEARBEITUNG
              PERFORM R5-BEARBEITUNG
           END-IF

           IF DS-FUSS-VERSORGEN
              PERFORM R6-FUSS-VERSORGEN
           END-IF

           IF DS-BILDAUSGABE
              PERFORM R7-BILDAUSGABE
           END-IF

           IF DS-RETURN      OR
              DS-VERZWEIGEN
           THEN
              PERFORM R8-FORTSETZUNG
           ELSE
              MOVE K-1            TO ERR-ORT-LFD
              MOVE K-23           TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
           END-IF
           .
       R-EXIT.
           GOBACK.

      *------------------------------------------------------------*
      * R1-EINSTIEG                                                *
      *------------------------------------------------------------*
      * INPUT : TCTUA, DFHCOMMAREA, TS-QUEUE(MAPS)                 *
      *                                                            *
      * OUTPUT: LETZTER STAND DER MAPS, DS-STATUS, CA,             *
      *         AKT. DATUM UND AKT. ZEIT                           *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     PLAUSI-PRÜFUNGEN ZUM PROGRAMMAUFRUF                    *
      *     ERMITTELN AKT. DATUM UND ZEIT                          *
      *     SETZEN DES DS-STATUS ABHAENGIG VON CADR-A-RC UND       *
      *        CARD-BILDLAST                                       *
      *     FALLS NICHT DIALOG-ERSTEINSTIEG                        *
      *        MAPS ENTSICHERN                                     *
      *        USERDATEN ENTSICHERN                                *
      *------------------------------------------------------------*
       R1-EINSTIEG SECTION.
       R1-ANF.
      *    * ERST NACH 'MOVE CARU-TRACE TO ERR-TRACE' SINNVOLL
070396*    MOVE 'R1   ' TO ERR-ORT-SEC
070396*    PERFORM ZR1-TRACE-KURZ
      *
           PERFORM R11-EINSTIEG-INIT-VOR-CA

           PERFORM R12-EINSTIEG-START

           PERFORM R13-EINSTIEG-INIT-NACH-CA

           PERFORM R14-PRUEFEN-AUFRUF-MODUS

           IF NOT DS-DIALOG-INIT
              PERFORM R15-BILDER-ENTSICHERN

210197***     ----------------------------------------------------------
210197***     ACHTUNG: WENN SECTION NICHT GEFUNDEN WIRD, DANN BITTE COPY
210197***             'DPAT0050' (DUMMY-SECTION) IN DIALOG-PGM AUFNEHMEN
210197        PERFORM DE-USERDATEN-ENTSICHERN
210197***     ----------------------------------------------------------

           END-IF
           .
       R1-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R11-EINSTIEG-INIT-VOR-CA                                   *
      *------------------------------------------------------------*
      * INPUT : -                                                  *
      *                                                            *
      * OUTPUT: ERR-ERROR-BEREICH                                  *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   INITS VOR UEBERTRAGEN DER CA AUS DFHCOMMAREA:            *
      *   - VORBELEGUNGEN                                          *
      *                                                            *
      *------------------------------------------------------------*
       R11-EINSTIEG-INIT-VOR-CA SECTION.
       R11-ANF.
      *    * ERST NACH 'MOVE CARU-TRACE TO ERR-TRACE' SINNVOLL
070396*    MOVE 'R11  ' TO ERR-ORT-SEC
070396*    PERFORM ZR1-TRACE-KURZ
      *
      *    * VORBELEGUNG VERSCH. FELDER
      *
           INITIALIZE                ERR-ERROR-BEREICH
200197     INITIALIZE                S-FTASTE-RC
200197     INITIALIZE                S-OP-RC
           .
       R11-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R12-EINSTIEG-START                                         *
      *------------------------------------------------------------*
      * INPUT : TCTUA, DFHCOMMAREA                                 *
      *                                                            *
      * OUTPUT: CA                                                 *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     PLAUSI-PRÜFUNGEN ZUM PROGRAMMAUFRUF                    *
      *                                                            *
      *------------------------------------------------------------*
       R12-EINSTIEG-START SECTION.
       R12-ANF.
      *    * ERST NACH 'MOVE CARU-TRACE TO ERR-TRACE' SINNVOLL
070396*    MOVE 'R12  ' TO ERR-ORT-SEC
070396*    PERFORM ZR1-TRACE-KURZ
      *
      *****     HANDEL CONDITION - FALLS RESP-OPTION VERGESSEN WURDE
      *                            (PROGRAMMIERFEHLER)
      *
CCC005*    EXEC CICS HANDLE CONDITION
CCC004*              ERROR(ZR91-ANF)
CCC003*    END-EXEC
      *
      *****     PRUEFEN: AUFRUF DURCH DRIVER !?
      *
           IF EIBCALEN NOT = K-CA-LEN
CCC005*       EXEC CICS SEND
CCC004*                 FROM   (K-TACERR)
CCC004*                 LENGTH (K-TACERR-LEN)
CCC004*                 ERASE
CCC004*                 WAIT
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-2             TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF

CCC005*       EXEC CICS RETURN
CCC003*       END-EXEC
           END-IF
      *
      *****     ADRESSIEREN TCTUA
      *
CCC009*    EXEC CICS ADDRESS
CCC009*              TCTUA(ADDRESS OF TCTUA-AREA)
CCC004*              RESP (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-3             TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF
      *
      *****     UEBERTRAGEN COMMAREA IN WORKING-STORAGE
      *
           MOVE DFHCOMMAREA        TO C-COMMAREA
      *
      *****     TRACE ZU BEGINN HPRO START
      *
           MOVE CARU-TRACE          TO ERR-TRACE
           MOVE 'H>>> ' TO ERR-ORT-SEC
           PERFORM UR2-TRACE
           MOVE 'R1   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
           .
       R12-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R13-EINSTIEG-INIT-NACH-CA                                  *
      *------------------------------------------------------------*
      * INPUT : CA,                                                *
      *                                                            *
      * OUTPUT: S-DIALOG-ZUSTAND, INIT. ERR-RC U. ERR-VAR          *
      *         AKT. DATUM UND AKT. ZEIT                           *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   INITS NACH UEBERTRAGEN DER CA AUS DFHCOMMAREA:           *
      *   - VORBELEGUNGEN                                          *
      *   - ERMITTELN AKT. DATUM UND ZEIT                          *
270798*   - ERMITTELN DER ADRESSEN VON DFHEIBLK UND DFHCOMMAREA    *
270798*     UND ABLEGEN IM STANDARD-STATUS-BEREICH. DIE ADRESSEN   *
270798*     WERDEN FüR DEN CICS-ZUGRIFF UNTERHALB VON REINEN       *
270798*     COBOL-PROGRAMMEN (FACHFUNKTIONEN) BENöTIGT.            *
270798*     Bsp.: ZAssnnnn ruft ein ZDssnnnn                       *
      *                                                            *
      *------------------------------------------------------------*
       R13-EINSTIEG-INIT-NACH-CA SECTION.
       R13-ANF.
           MOVE 'R13  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * VERSORGUNG VERSCH. FELDER AUS CA
      *
           MOVE CARD-DIALOG-ZUSTAND TO S-DIALOG-ZUSTAND
      *
      *    * AKT. DATUM UND ZEIT ERMITTELN
      *
CCC005*    EXEC CICS ASKTIME ABSTIME(Z-ABSZEIT)
CCC004*              RESP (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-4             TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

CCC005*    EXEC CICS FORMATTIME ABSTIME(Z-ABSZEIT)
CCC004*              DATESEP('.')
CCC004*              DDMMYY(D-AKT-DATUM-KURZ)
CCC004*              YYMMDD(Z-AKT-DATUM-TEMP)
CCC004*              YEAR  (Z-YEAR)
CCC004*              TIMESEP(':')
CCC004*              TIME(D-AKT-ZEIT)
CCC004*              RESP (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-5         TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

           MOVE Z-AKT-JJ-TEMP                TO Z-AKT-JJ
           MOVE Z-AKT-MONAT-TEMP             TO Z-AKT-MONAT
           MOVE Z-AKT-TAG-TEMP               TO Z-AKT-TAG
           MOVE Z-YEAR                       TO Z-AKT-JAHR
      *
      *    * CICS-DATUM VERSORGEN
      *    * FORMATE: TT.MM.JJ, TT.MM.JJJJ, JJJJ-MM-TT
      *
           MOVE CADU-CICS-TAG                TO D-CICS-K-TAG
                                                D-CICS-L-TAG
                                                D-CICS-I-TAG
           MOVE CADU-CICS-MONAT              TO D-CICS-K-MONAT
                                                D-CICS-L-MONAT
                                                D-CICS-I-MONAT
           MOVE CADU-CICS-JJ                 TO D-CICS-K-JJ
           MOVE CADU-CICS-JAHR               TO D-CICS-L-JAHR
                                                D-CICS-I-JAHR

270798***  ABLEGEN DER ADDRESSEN VON DFHEIBLK UND DFHCOMMAEREA
270798***  IM STANDARD-STATUS-BEREICH
CCC009*    SET STAT-PTR-DFHEIBLK       TO ADDRESS OF DFHEIBLK
CCC009*    SET STAT-PTR-DFHCOMMAREA    TO ADDRESS OF DFHCOMMAREA
           .
       R13-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R14-PRUEFEN-AUFRUF-MODUS                                   *
      *------------------------------------------------------------*
      * INPUT : AUFRUF-MODUS, CARD-A-RC, CARD-BILDLAST             *
      *                                                            *
      * OUTPUT: DS-STATUS, CARD-CURABS                             *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     SETZEN DES DS-STATUS ABHAENGIG VON                     *
      *     CADR-A-AUFRUF-MODUS, CADR-A-RC UND CARD-BILDLAST       *
      *                                                            *
      *------------------------------------------------------------*
       R14-PRUEFEN-AUFRUF-MODUS SECTION.
       R14-ANF.
           MOVE 'R14  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * ANFANGSBEARBEITUNG ABHAENGIG VOM AUFRUF-MODUS
      *
      *    --------------------------------------------------------*
      *    * PRUEFUNG CADR-A-AUFRUF-MODUS
           EVALUATE TRUE
      *    --------------------------------------------------------*
           WHEN KEIN-DRIVER-AUFRUF
              MOVE K-PGM-NAME           TO CADI-MELDUNG-PGM
              SET  DS-BILDEINGABE       TO TRUE
      *    --------------------------------------------------------*
020296*    * DIALOG-DWL-AUFRUF IST FÜR DIE STEUERUNG IDENTISCH MIT
020296*    *    DIALOG-ERSTAUFRUF; DIFFERENZIERTE ABFRAGE IN I1 MÖGLICH
           WHEN DIALOG-ERSTAUFRUF
020296     WHEN DIALOG-DWL-AUFRUF
              MOVE K-PGM-NAME           TO CADI-MELDUNG-PGM
              SET  DS-DIALOG-INIT       TO TRUE
      *    --------------------------------------------------------*
           WHEN DIALOG-WIEDERAUFRUF
              PERFORM R141-BEARB-WIEDERAUFRUF
      *    --------------------------------------------------------*
           WHEN OTHER
020296        MOVE CADR-A-AUFRUF-MODUS  TO ERR-VAR-ZEILE01(01:01)
              MOVE K-6                  TO ERR-ORT-LFD
              MOVE K-3                  TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    --------------------------------------------------------*
           END-EVALUATE
      *    --------------------------------------------------------*
           .
       R14-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R141-BEARB-WIEDERAUFRUF                                    *
      *------------------------------------------------------------*
      * INPUT : CADR-A-RC, CARD-BILDLAST                           *
      *                                                            *
      * OUTPUT: DS-STATUS, CADI-MELDUNG-PGM                        *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     SETZEN DES DS-STATUS ABHAENGIG VON CADR-A-RC UND       *
      *        CARD-BILDLAST                                       *
      *------------------------------------------------------------*
       R141-BEARB-WIEDERAUFRUF SECTION.
       R141-ANF.
           MOVE 'R141 ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    -------------------------------------------------------*
      *    * PRUEFEN CADR-A-RC
           EVALUATE TRUE
      *    ------------------------------------------*
           WHEN DR-A-RC-OK
              MOVE K-PGM-NAME              TO CADI-MELDUNG-PGM
              IF BILDLAST-JA
                 SET DS-BILDAUSGABE        TO TRUE
              ELSE
                 SET DS-BEARBEITUNG        TO TRUE
              END-IF
      *    ------------------------------------------*
           WHEN DR-A-RC-ABBRUCH
              MOVE K-PGM-NAME              TO CADI-MELDUNG-PGM
              MOVE DFHPF12                 TO Z-FTASTE
              SET DS-FTASTE-BEHANDELN      TO TRUE
      *    ------------------------------------------*
           WHEN DR-A-RC-NOK
      *       * FEHLERMELDUNG VOM DRIVER AUSGEBEN
      *       * CADI-MELDUNG-PGM NICHT VERSORGEN
      *       * CURSOR-POSITIONIERUNG:
      *       *    AUCH BEI 'BILDLAST-NUR-BEI-FEHLER'
              IF BILDLAST-NUR-BEI-FEHLER
                 SET BILDLAST-JA           TO TRUE
              END-IF
              SET DS-FUSS-VERSORGEN        TO TRUE
      *    ------------------------------------------*
           WHEN OTHER
              MOVE K-7                  TO ERR-ORT-LFD
              MOVE K-5                  TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    ------------------------------------------*
           END-EVALUATE
      *    -------------------------------------------------------*
           .
       R141-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R15-BILDER-ENTSICHERN                                      *
      *------------------------------------------------------------*
      * INPUT : TS-QUEUE(MAPS)                                     *
      *                                                            *
      * OUTPUT: LETZTER STAND DER MAPS                             *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     ALLE MAPS AUS TS-QUEUE(MAPS) ENTSICHERN                *
      *------------------------------------------------------------*
       R15-BILDER-ENTSICHERN SECTION.
       R15-ANF.
           MOVE 'R15  '                 TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * LESEN MAP-RAHMEN - FUSS  AUS TS-QUEUE
           MOVE CARD-TSQMA-NAME         TO Z-TSQ-NAME
           MOVE K-1                     TO Z-TSQ-ITEM
CCC005*    EXEC CICS READQ TS
CCC004*              QUEUE (Z-TSQ-NAME)
CCC004*              INTO  (MFI)
CCC004*              ITEM  (Z-TSQ-ITEM)
CCC004*              RESP  (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-8         TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF
      *
      *    * LESEN MASKENRUMPF-MAPS AUS TS-QUEUE
      *    * (ABHÄNGIG VON I-BILD-MAX)
      *
      *    * LESEN MAP 1 AUS TS-QUEUE
           IF I-BILD-MAX NOT < K-1
              MOVE K-1                  TO Z-TSQ-ITEM
              ADD  1                    TO Z-TSQ-ITEM
CCC005*       EXEC CICS READQ TS
CCC004*                 QUEUE (Z-TSQ-NAME)
CCC004*                 INTO  (M1I)
CCC004*                 ITEM  (Z-TSQ-ITEM)
CCC004*                 RESP  (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-9         TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
           END-IF
      *
      *    * LESEN MAP 2 AUS TS-QUEUE
           IF I-BILD-MAX NOT < K-2
              MOVE K-2                  TO Z-TSQ-ITEM
              ADD  1                    TO Z-TSQ-ITEM
CCC005*       EXEC CICS READQ TS
CCC004*                 QUEUE (Z-TSQ-NAME)
CCC004*                 INTO  (M2I)
CCC004*                 ITEM  (Z-TSQ-ITEM)
CCC004*                 RESP  (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-10        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
           END-IF
      *
      *    * LESEN MAP 3 AUS TS-QUEUE
           IF I-BILD-MAX NOT < K-3
              MOVE K-3                  TO Z-TSQ-ITEM
              ADD  1                    TO Z-TSQ-ITEM
CCC005*       EXEC CICS READQ TS
CCC004*                 QUEUE (Z-TSQ-NAME)
CCC004*                 INTO  (M3I)
CCC004*                 ITEM  (Z-TSQ-ITEM)
CCC004*                 RESP  (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-11        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
           END-IF
           .
       R15-EXIT.
           EXIT.

      *------------------------------------------------------------*
       R2-BILDEINGABE SECTION.
      *------------------------------------------------------------*
      * INPUT : EIBAID, AKT. MAP                                   *
      * OUTPUT: DS-STATUS,                                         *
      *         CARU-MAP-LAST                                      *
      * VERARBEITUNG:                                              *
      *     VERARBEITUNG ABH. VON FUNKTIONSTASTE:                  *
      *     - CLEAR                                                *
      *        --> VERZWEIGEN ZUR ERNEUTEN AUSGABE DER LETZTEN MAP *
      *     - PA1, PA2, PA3                                        *
      *        --> IGNORIEREN DER TASTEN;                          *
      *        --> DIREKT VERZWEIGEN ZUM RETURN(TRANSID)           *
      *     - REST                                                 *
      *        --> MAP EINLESEN                                    *
      *        --> VERZWEIGEN ZUR BEHANDLUNG DER FTASTE            *
      *------------------------------------------------------------*
       R2-ANF.
           MOVE 'R2   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           EVALUATE EIBAID
           WHEN DFHCLEAR
      *       * LETZTE MAP NOCHMALS ANZEIGEN (INCL. MAP)
              MOVE SPACE               TO CARU-MAP-LAST
              SET DS-BILDAUSGABE       TO TRUE
070297     WHEN DFHPA1
070297     WHEN DFHPA2
070297     WHEN DFHPA3
070297*       * FTASTE IGNORIEREN
070297        SET DS-RETURN            TO TRUE
           WHEN OTHER
      *       * FTASTE IN ARBEITSFELD UEBERTRAGEN
              MOVE EIBAID              TO Z-FTASTE

      *       * ABSOLUTE CURSOR-POS MERKEN
              MOVE EIBCPOSN            TO CARD-CURABS

      *       * MAPS EINLESEN
              PERFORM R21-MAP-EINLESEN

      *       * INIT FUSS-LAENGENFELDER UND ANDERE FELDER
              PERFORM R22-FUSS-INIT
      *       * FUSS-EINGABE UEBERTRAGEN IN MASKEN-ARBEITSBEREICH
              PERFORM R23-FUSS-MAPEING-UEBERTR

      *       * INIT LAENGENFELDER UND ANDERE FELDER
              PERFORM L1-INIT-LAENGENFELDER-ETC

      *       * EINGABE UEBERTRAGEN IN MASKEN-ARBEITSBEREICH
              PERFORM M1-MAPEINGABE-UEBERTRAGEN

              SET DS-FTASTE-BEHANDELN  TO TRUE
           END-EVALUATE
           .
       R2-EXIT.
           EXIT.

      *------------------------------------------------------------*
       R21-MAP-EINLESEN SECTION.
      *------------------------------------------------------------*
      * INPUT : S-BILDNR                                           *
      * OUTPUT: AKT. MAP                                           *
      * VERARBEITUNG:                                              *
      *   - RAHMEN-MAPS EINLESEN                                   *
      *   - ABHAENGIG VON BILDNR DIE AKTUELLE RUMPF-MAP EINLESEN   *
      *------------------------------------------------------------*
       R21-ANF.
           MOVE 'R21  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * MAPS EINLESEN
CCC005*    EXEC CICS RECEIVE INTO   (Z-EINBER)
CCC004*                      LENGTH (Z-EINBER-LEN)
CCC004*                      RESP   (C-CICS-RC)
CCC003*    END-EXEC

           IF C-CICS-RC NOT = DFHRESP(NORMAL)
      * ???
           AND C-CICS-RC NOT = DFHRESP(EOC)
              MOVE K-12        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

CCC005*    EXEC CICS RECEIVE MAP    (K-MAP-RAHMEN-KOPF)
CCC004*                      MAPSET (K-MAP-RAHMEN-KOPF)
CCC004*                      INTO   (BAT0001I)
CCC004*                      FROM   (Z-EINBER)
CCC004*                      RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
      * ???
           AND C-CICS-RC NOT = DFHRESP(MAPFAIL)
              MOVE K-13        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

CCC005*    EXEC CICS RECEIVE MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                      MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                      INTO   (MFXI)
CCC004*                      FROM   (Z-EINBER)
CCC004*                      RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
      * ???
           AND C-CICS-RC NOT = DFHRESP(MAPFAIL)
              MOVE K-14        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

      *    -------------------------------------------------*
      *    * LETZTE AUSGEGEBENE MAP EINLESEN
      *    * MAP-ZAHL ABHAENGIG VON S-BILDNR
           EVALUATE S-BILDNR
      *    -------------------------------------------------*
           WHEN K-1
CCC005*       EXEC CICS RECEIVE MAP    (CARU-MAP-LAST)
CCC004*                         FROM   (Z-EINBER)
CCC004*                         INTO   (M1XI)
CCC004*                         RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
      * ???
              AND C-CICS-RC NOT = DFHRESP(MAPFAIL)
                 MOVE K-15               TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    -------------------------------------------------*
           WHEN K-2
CCC005*       EXEC CICS RECEIVE MAP    (CARU-MAP-LAST)
CCC004*                         FROM   (Z-EINBER)
CCC004*                         INTO   (M2XI)
CCC004*                         RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
      * ???
              AND C-CICS-RC NOT = DFHRESP(MAPFAIL)
                 MOVE K-16               TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    -------------------------------------------------*
           WHEN K-3
CCC005*       EXEC CICS RECEIVE MAP    (CARU-MAP-LAST)
CCC004*                         FROM   (Z-EINBER)
CCC004*                         INTO   (M3XI)
CCC004*                         RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
      * ???
              AND C-CICS-RC NOT = DFHRESP(MAPFAIL)
                 MOVE K-17               TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    -------------------------------------------------*
           WHEN OTHER
              MOVE K-18                  TO ERR-ORT-LFD
              MOVE K-9                   TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    -------------------------------------------------*
           END-EVALUATE
      *    -------------------------------------------------*
           .
       R21-EXIT.
           EXIT.

      *------------------------------------------------------------*
       R22-FUSS-INIT SECTION.
      *------------------------------------------------------------*
      * INPUT : -                                                  *
      * OUTPUT: EINGABEFELDER DES FUSSES INITIALISIERT             *
      * VERARBEITUNG:                                              *
      *     INITIALISIEREN DER EINGABEFELDER DES FUSSES            *
      *------------------------------------------------------------*
       R22-ANF.
           MOVE 'R22  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * LOW-VALUE ZU SPACE UMSETZEN
           INSPECT MFXI REPLACING ALL LOW-VALUE BY SPACE
      *
      *    * INIT LAENGENFELDER (WG. CURSOR)
           MOVE ZERO                         TO ERTRANSL IN MFI,
                                                ERKEYL   IN MFI,
                                                ERCTVL   IN MFI,
                                                EROKL    IN MFI
           .
       R22-EXIT.
           EXIT.

      *------------------------------------------------------------*
       R23-FUSS-MAPEING-UEBERTR SECTION.
      *------------------------------------------------------------*
      * INPUT : FUSS-EINGABEN IN MFXI                              *
      * OUTPUT: FUSS-EINGABEN IN MFI                               *
      * VERARBEITUNG:                                              *
      *   - UEBERTRAGEN MAP-FUSS-EINGABEN NACH MFI                 *
060195*   - ERTRANSI-EINGABE AUF GROSS-BUCHSTABEN UMSETZEN         *
060195*   - EROKI-EINGABE AUF GROSS-BUCHSTABEN UMSETZEN            *
      *------------------------------------------------------------*
       R23-ANF.
           MOVE 'R23  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * UEBERTRAGEN FELDINHALTE IN ARBEITS- UND AUSGABEMASKE
           MOVE ERTRANSI IN MFXI             TO ERTRANSI IN MFI
           MOVE ERKEYI   IN MFXI             TO ERKEYI   IN MFI
           MOVE ERCTVI   IN MFXI             TO ERCTVI   IN MFI
           MOVE EROKI    IN MFXI             TO EROKI    IN MFI

030395* 060195*    * UMSETZEN KLEIN - GROSS
030395* 060195     IF ERTRANSI IN MFI = 'TRC0'
030395* 060195        MOVE 'TRC0'            TO ERTRANSI IN MFI
030395* 060195     END-IF
030395* 060195     IF ERTRANSI IN MFI = 'TRC1'
030395* 060195        MOVE 'TRC1'            TO ERTRANSI IN MFI
030395* 060195     END-IF
030395* 060195     IF ERTRANSI IN MFI = 'TRCK'
030395* 060195        MOVE 'TRCK'            TO ERTRANSI IN MFI
030395* 060195     END-IF
030395* 060195     IF ERTRANSI IN MFI = 'TRCL'
030395* 060195        MOVE 'TRCL'            TO ERTRANSI IN MFI
030395* 060195     END-IF
030395*
030395* 060195     IF EROKI IN MFI = 'N'
030395* 060195        MOVE 'N'               TO EROKI IN MFI
030395* 060195     END-IF
030395* 060195     IF EROKI IN MFI = 'J'
030395* 060195        MOVE 'J'               TO EROKI IN MFI
030395* 060195     END-IF

030395*    * UMSETZEN EINGABEWERTE IN GROSSBUCHSTABEN, DA IN EINIGEN
030395*    * TRANSAKTIONEN GROSS-KLEIN-SCHREIBUNG MÖGLICH IST
030395     INSPECT ERTRANSI IN MFI
030395             CONVERTING K-KLEINBUCHSTABEN TO K-GROSSBUCHSTABEN
030395     INSPECT EROKI    IN MFI
030395             CONVERTING K-KLEINBUCHSTABEN TO K-GROSSBUCHSTABEN
           .
       R23-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R3-FTASTE-BEHANDELN                                        *
      *------------------------------------------------------------*
      * INPUT : Z-FTASTE, AKT. MAP                                 *
      *                                                            *
      * OUTPUT: DS-STATUS, C-OP-TAB,                               *
      *         CADR-E-OP, CADR-E-TRANS-NEXT, CADD-OB, CADD-CTV,   *
      *         CARD-MELDRAHM-KEY, CADI-MELDUNG-KEY,               *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     PRUEFEN FTASTE DURCH RAHMEN                            *
      *     AUFRUF PRUEFEN-FTASTE (FACHFUNKTION)                   *
      *     RETURN-WERT BEARBEITEN                                 *
      *     FALLS FTASTE FALSCH ZU FEHLERMELDUNG VERZWEIGEN        *
      *     FALLS FTASTE UNBEKANNT ZUM DRIVER VERZWEIGEN           *
      *                                                            *
      *------------------------------------------------------------*
       R3-FTASTE-BEHANDELN SECTION.
       R3-ANF.
           MOVE 'R3   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      * ---------------------------------------------------------- *
      *    * PRUEFEN INPUT AUF TRACE-PARAMETER                     *
      * ---------------------------------------------------------- *
           SET FTASTE-RC-INIT            TO TRUE
           PERFORM ZR5-PRUEF-TRACE-INPUT

      *    * ABFRAGEN RC NACH PRUEFEN FTASTE DURCH RAHMEN
           PERFORM R31-FTASTE-RC-RAHMEN

           IF NOT DS-FTASTE-BEHANDELN
      *       * RAHMEN BEARBEITET FTASTE, NICHT DIE FACHFUNKTION !
              GO TO R3-EXIT
           END-IF
      *
      * ---------------------------------------------------------- *
      *    * PRUEFEN F-TASTE DURCH RAHMEN UNABH. VOM DIALOG-STATUS *
      * ---------------------------------------------------------- *
           SET FTASTE-RC-INIT            TO TRUE
           PERFORM ZR4-FT-DIALOG-STATUS-UNABH

      *    * ABFRAGEN RC NACH PRUEFEN FTASTE DURCH RAHMEN
           PERFORM R31-FTASTE-RC-RAHMEN

           IF NOT DS-FTASTE-BEHANDELN
      *       * RAHMEN BEARBEITET FTASTE, NICHT DIE FACHFUNKTION !
              GO TO R3-EXIT
           END-IF
      *
      * ---------------------------------------------------------- *
      *    * PRUEFEN F-TASTE DURCH RAHMEN ABH. VOM DIALOG-STATUS   *
      *    * UND VON DER DIALOG-EBENE                              *
      * ---------------------------------------------------------- *
           SET FTASTE-RC-INIT            TO TRUE
           PERFORM ZR2-FT-DIALOG-STATUS-ABH

      *    * ABFRAGEN RC NACH PRUEFEN FTASTE DURCH RAHMEN
           PERFORM R31-FTASTE-RC-RAHMEN
           IF NOT DS-FTASTE-BEHANDELN
      *       * RAHMEN BEARBEITET FTASTE, NICHT DIE FACHFUNKTION !
              GO TO R3-EXIT
           END-IF

      * --------------------------------------------------- *
      *    * PRUEFEN EINGEGEBENE F-TASTE DURCH FACHFUNKTION *
      * --------------------------------------------------- *
           INITIALIZE CARD-OP-TAB
           INITIALIZE CADI-OP-AKT

           SET FTASTE-RC-INIT            TO TRUE
           PERFORM T1-PRUEFEN-FTASTE
      *
      *    * ABFRAGEN RC VON PRUEFEN-FTASTE
           EVALUATE TRUE
           WHEN FTASTE-OK
              MOVE ZERO                  TO CARD-OP-IND
              SET DIALOG-STATUS-INARBEIT TO TRUE
              SET DS-BEARBEITUNG         TO TRUE
           WHEN FTASTE-ABBR
              SET DS-FUSS-VERSORGEN      TO TRUE
           WHEN FTASTE-ABBR-VERZW
              MOVE ERTRANSI IN MFI       TO CADR-E-TRANS-NEXT
              MOVE ERKEYI   IN MFI       TO CADU-OB
              MOVE ERCTVI   IN MFI       TO CADD-CTV
              PERFORM R32-FT-TO-DR-E-OP
              SET DS-VERZWEIGEN          TO TRUE
           WHEN FTASTE-ABBR-BILDLAST
              SET BILDLAST-JA            TO TRUE
              SET DS-FUSS-VERSORGEN      TO TRUE
           WHEN FTASTE-ABBR-VERZW-BILDLAST
           WHEN FTASTE-UNBEKANNT
              MOVE ERTRANSI IN MFI       TO CADR-E-TRANS-NEXT
              MOVE ERKEYI   IN MFI       TO CADU-OB
              MOVE ERCTVI   IN MFI       TO CADD-CTV
              PERFORM R32-FT-TO-DR-E-OP
              SET BILDLAST-JA            TO TRUE
              SET DS-VERZWEIGEN          TO TRUE
           WHEN FTASTE-BILDAUSGABE
              SET DS-BILDAUSGABE         TO TRUE
           WHEN FTASTE-BILDAUSGABE-BILDLAST
              SET BILDLAST-JA            TO TRUE
              SET DS-BILDAUSGABE         TO TRUE
           WHEN OTHER
              MOVE K-19                  TO ERR-ORT-LFD
              MOVE K-7                   TO ERR-MELD-NR
              MOVE S-FTASTE-RC           TO ERR-VAR
              PERFORM UR93-PROG-FEHLER
           END-EVALUATE
           .
       R3-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R31-FTASTE-RC-RAHMEN                                       *
      *------------------------------------------------------------*
      * INPUT : S-FTASTE-RC                                        *
      *                                                            *
      * OUTPUT: DS-STATUS,                                         *
      *         EVTL.: CARD-MELDRAHM-PGM, CARD-BILDLAST            *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     AUFRUF PRUEFEN-FTASTE (FACHFUNKTION)                   *
      *     RETURN-WERT VON PRUEFEN FTASTE DURCH RAHMEN BEARBEITEN *
      *                                                            *
      *------------------------------------------------------------*
       R31-FTASTE-RC-RAHMEN SECTION.
       R31-ANF.
           MOVE 'R31  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           EVALUATE TRUE
           WHEN FTASTE-OK
      *       * FTASTE WEITER PRUEFEN
              CONTINUE
           WHEN FTASTE-ABBR
              MOVE K-AT-PGM-NAME         TO CARD-MELDRAHM-PGM
              SET DS-FUSS-VERSORGEN      TO TRUE
           WHEN FTASTE-ABBR-VERZW
              SET DS-VERZWEIGEN          TO TRUE
           WHEN FTASTE-ABBR-BILDLAST
              MOVE K-AT-PGM-NAME         TO CARD-MELDRAHM-PGM
              SET BILDLAST-JA            TO TRUE
              SET DS-FUSS-VERSORGEN      TO TRUE
           WHEN FTASTE-ABBR-VERZW-BILDLAST
              SET BILDLAST-JA            TO TRUE
              SET DS-VERZWEIGEN          TO TRUE
           WHEN FTASTE-BILDAUSGABE
              SET DS-BILDAUSGABE         TO TRUE
           WHEN FTASTE-BILDAUSGABE-BILDLAST
              SET BILDLAST-JA            TO TRUE
              SET DS-BILDAUSGABE         TO TRUE
           WHEN OTHER
              MOVE K-20                  TO ERR-ORT-LFD
              MOVE K-7                   TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
           END-EVALUATE
           .
       R31-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R32-FT-TO-DR-E-OP                                          *
      *------------------------------------------------------------*
      * INPUT : Z-FTASTE                                           *
      *                                                            *
      * OUTPUT: CADR-E-OP                                          *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - VERSOFGEN DES DRIVER-OP-CODES ENTSPRECHEND DER         *
      *     BETAETIGTEN FUNKTIONSTASTE                             *
      *                                                            *
      *------------------------------------------------------------*
       R32-FT-TO-DR-E-OP SECTION.
       R32-ANF.
           MOVE 'R32  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           EVALUATE Z-FTASTE
           WHEN DFHENTER
              MOVE K-OP-ENTER         TO CADR-E-OP
           WHEN DFHPF1
              MOVE K-OP-F1            TO CADR-E-OP
           WHEN DFHPF2
              MOVE K-OP-F2            TO CADR-E-OP
           WHEN DFHPF3
              MOVE K-OP-F3            TO CADR-E-OP
           WHEN DFHPF4
              MOVE K-OP-F4            TO CADR-E-OP
           WHEN DFHPF5
              MOVE K-OP-F5            TO CADR-E-OP
           WHEN DFHPF6
              MOVE K-OP-F6            TO CADR-E-OP
           WHEN DFHPF7
              MOVE K-OP-F7            TO CADR-E-OP
           WHEN DFHPF8
              MOVE K-OP-F8            TO CADR-E-OP
           WHEN DFHPF9
              MOVE K-OP-F9            TO CADR-E-OP
           WHEN DFHPF10
              MOVE K-OP-F10           TO CADR-E-OP
           WHEN DFHPF11
              MOVE K-OP-F11           TO CADR-E-OP
           WHEN DFHPF12
              MOVE K-OP-F12           TO CADR-E-OP
           WHEN DFHPF13
              MOVE K-OP-F13           TO CADR-E-OP
           WHEN DFHPF14
              MOVE K-OP-F14           TO CADR-E-OP
           WHEN DFHPF15
              MOVE K-OP-F15           TO CADR-E-OP
           WHEN DFHPF16
              MOVE K-OP-F16           TO CADR-E-OP
           WHEN DFHPF17
              MOVE K-OP-F17           TO CADR-E-OP
           WHEN DFHPF18
              MOVE K-OP-F18           TO CADR-E-OP
           WHEN DFHPF19
              MOVE K-OP-F19           TO CADR-E-OP
           WHEN DFHPF20
              MOVE K-OP-F20           TO CADR-E-OP
           WHEN DFHPF21
              MOVE K-OP-F21           TO CADR-E-OP
           WHEN DFHPF22
              MOVE K-OP-F22           TO CADR-E-OP
           WHEN DFHPF23
              MOVE K-OP-F23           TO CADR-E-OP
           WHEN DFHPF24
              MOVE K-OP-F24           TO CADR-E-OP
           WHEN DFHPA1
              MOVE K-OP-PA1           TO CADR-E-OP
           WHEN DFHPA2
              MOVE K-OP-PA2           TO CADR-E-OP
           WHEN OTHER
              MOVE HIGH-VALUE         TO CADR-E-OP
           END-EVALUATE
           .
       R32-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R4-DIALOG-INIT                                             *
      *------------------------------------------------------------*
      * INPUT : DIALOG-STEUERFELDER,                               *
      *                                                            *
      * OUTPUT: DS-STATUS,                                         *
      *        INITIALISIERT:                                      *
      *         CADI-FTASTEN-PGM,                                  *
      *         TS-QUEUES FUER MAPS                                *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     DIALOG-STEUERFELDER INITIALISIEREN                     *
      *     TS-QUEUES INITIALISIEREN                               *
      *     AUFRUF BEARB-DIALOG-INIT (FACHMODUL)                   *
      *     AUSWERTEN RETURNCODE                                   *
      *                                                            *
      *------------------------------------------------------------*
       R4-DIALOG-INIT SECTION.
       R4-ANF.
           MOVE 'R4   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * DIALOG-INITIALISIERUNG *
           MOVE K-PGM-NAME          TO CADI-FTASTEN-PGM

      *    * --> MUSS BEREITS IM DRIVER INITIALISERT WERDEN !!!
      *    INITIALIZE                  CARD-OP-TAB
      *                                CARD-OP-IND
      *                                CADI-OP-AKT
      *                                CADI-S-ABSCHLUSS

      *****    TS-QUEUES INITIALISIEREN     *****

      *    * TS-QUEUE LOESCHEN
           MOVE CARD-TSQMA-NAME         TO Z-TSQ-NAME
CCC005*    EXEC CICS DELETEQ TS
CCC004*              QUEUE (Z-TSQ-NAME)
CCC004*              RESP  (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              IF C-CICS-RC = DFHRESP(QIDERR)
      *          KEINE TS-QUEUE VORHANDEN
                 CONTINUE
              ELSE
                 MOVE K-21        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
           END-IF

      *    * VORBELEGEN MAP-RAHMEN-FUSS IN TS-QUEUE
           MOVE LOW-VALUE         TO MFO
CCC005*    EXEC CICS WRITEQ TS
CCC004*              QUEUE  (Z-TSQ-NAME)
CCC004*              FROM   (MFO)
CCC004*              MAIN
CCC004*              RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-22        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

      *
      *    * VORBELEGEN DER MAP-ITEMS IN TS-QUEUE
      *    * MAP-ZAHL ABHAENGIG VON I-BILD-MAX
      *
      *    * VORBELEGEN MAP 1 IN TS-QUEUE
170299*    IF I-BILD-MAX NOT < K-1
              MOVE LOW-VALUE         TO M1O
CCC005*       EXEC CICS WRITEQ TS
CCC004*                 QUEUE  (Z-TSQ-NAME)
CCC004*                 FROM   (M1O)
CCC004*                 MAIN
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-23        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
170299*    END-IF
      *
      *    * VORBELEGEN MAP 2 IN TS-QUEUE
170299*    IF I-BILD-MAX NOT < K-2
              MOVE LOW-VALUE         TO M2O
CCC005*       EXEC CICS WRITEQ TS
CCC004*                 QUEUE  (Z-TSQ-NAME)
CCC004*                 FROM   (M2O)
CCC004*                 MAIN
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-24        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
170299*    END-IF
      *
      *    * VORBELEGEN MAP 3 IN TS-QUEUE
170299*    IF I-BILD-MAX NOT < K-3
              MOVE LOW-VALUE         TO M3O
CCC005*       EXEC CICS WRITEQ TS
CCC004*                 QUEUE  (Z-TSQ-NAME)
CCC004*                 FROM   (M3O)
CCC004*                 MAIN
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-25        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
170299*    END-IF

170299*>>>>>
      *    * VORBELEGEN MAP EURO-SWITCH
           MOVE LOW-VALUE         TO Z-EURO-SWITCH-EINBER
CCC005*    EXEC CICS WRITEQ TS
CCC004*              QUEUE  (Z-TSQ-NAME)
CCC004*              FROM   (Z-EURO-SWITCH-EINBER)
CCC004*              MAIN
CCC004*              RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-55        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF
170299*<<<<<
170299*>>>>>
      *    * VORBELEGEN MAP-FUSS EURO-SWITCH
           MOVE LOW-VALUE         TO Z-EURO-SWITCH-EINBER
CCC005*    EXEC CICS WRITEQ TS
CCC004*              QUEUE  (Z-TSQ-NAME)
CCC004*              FROM   (Z-EURO-SWITCH-EINBER)
CCC004*              MAIN
CCC004*              RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-26        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF
170299*<<<<<


      *
      *****    AUFRUF FACHFUNKTION   *******
      *
           SET OP-RC-INIT           TO TRUE
           PERFORM I1-BEARB-DIALOG-INIT

      *    * ABFRAGEN S-OP-RC
           EVALUATE TRUE
           WHEN OP-OK
              SET DS-BEARBEITUNG          TO TRUE
           WHEN OP-ABBR
              SET DS-FUSS-VERSORGEN       TO TRUE
           WHEN NOT OP-RC-DIALOG-INIT-GUELTIG
              MOVE K-27                   TO ERR-ORT-LFD
              MOVE K-7                    TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
           END-EVALUATE
170299*>>>>>
      *****    PRüFEN, OB PROGRAMM MIT EURO-SWITCH ARBEITET
      *
           SET CARD-EURO-SWITCH-NEIN        TO TRUE
           SET CARD-EURO-SWITCH-OHNE-FEHLER TO TRUE
           INITIALIZE CARD-EURO-SWITCH-STD-WAE
                      CARD-EURO-SWITCH-ALT-WAE
           PERFORM UR5-EURO-SWITCH-PRUEFEN
170299*<<<<<
           .
       R4-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R5-BEARBEITUNG                                             *
      *------------------------------------------------------------*
      * INPUT : COMMAREA, MAP(S), C-OP-TAB,                        *
      *                                                            *
      * OUTPUT: DS-STATUS,                                         *
      *         CARD-DIALOG-ZUSTAND, CADI-S-ABSCHLUSS,             *
      *         CADI-FTASTEN-ZUSTAND, CADI-FTASTEN-NR,             *
      *         CARD-MELDRAHM-KEY, CADI-MELDUNG-KEY,               *
      *         CADR-E-TRANS-NEXT, ...                             *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     STEUERUNG DER BEARBEITUNG DER OPERATIONSCODES          *
      *        AUS DER OPERATIONS-CODE-TABELLE                     *
      *     ABHAENGIG VOM C-BEARB-STATUS EVTL. UNTERBRECHUNG       *
      *        DER OPERATIONS-BEARBEITUNG                          *
      *                                                            *
      *------------------------------------------------------------*
       R5-BEARBEITUNG SECTION.
       R5-ANF.
           MOVE 'R5   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ

           PERFORM R51-BEARB-START
           PERFORM WITH TEST BEFORE
                   UNTIL BEARB-NOP
              PERFORM B1-BEARB-OPERATION
              PERFORM R52-BEARB-FORTSETZUNG
           END-PERFORM
           .
       R5-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R51-BEARB-START                                            *
      *------------------------------------------------------------*
      * INPUT : MAP(S), CARD-OP-TAB,                               *
      *                                                            *
      * OUTPUT: CARD-DIALOG-ZUSTAND, DS-STATUS                     *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     FUELLEN DES AKT. OPERATIONSFELDES AUS DER              *
      *        OPERATIONSCODE-TABELLE                              *
      *     FALLS OP-TABELLE LEER                                  *
      *        SETZEN VON BEARB-NOP DER OPERATIONS-BEARBEITUNG     *
      *                                                            *
      *------------------------------------------------------------*
       R51-BEARB-START SECTION.
       R51-ANF.
           MOVE 'R51  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET OP-RC-INIT                     TO TRUE
151093     SET ABSCHL-ANZEIGE-INI             TO TRUE

           IF CADI-OP-AKT NOT = SPACE
      *       * WIEDERAUFRUF NACH UNTERBRECHUNG
      *       * MIT WIEDEREINSTIEG IN LETZTE OPERATION
              SET BEARB-OP                    TO TRUE
              SET OP-STATUS-WIEDERAUFRUF      TO TRUE
           ELSE
              IF CARD-OP-IND < CARD-OP-MAX
      *          * ENDE OP-TABELLE NOCH NICHT ERREICHT
                 ADD 1                        TO CARD-OP-IND
                 IF CARD-OP(CARD-OP-IND) NOT = SPACE
      *             * AUFRUF DER ERSTEN ODER
      *             * - NACH UNTERBRECHUNG OHNE OP-UNTERBR-VERZWEIGEN -
      *             * DER FOLGENDEN OPERATION
                    MOVE CARD-OP(CARD-OP-IND) TO CADI-OP-AKT
                    SET OP-STATUS-ERSTAUFRUF  TO TRUE
                    SET BEARB-OP              TO TRUE
                 ELSE
                    SET DS-FUSS-VERSORGEN     TO TRUE
                    SET BEARB-NOP             TO TRUE
                 END-IF
              ELSE
                 SET DS-FUSS-VERSORGEN        TO TRUE
                 SET BEARB-NOP                TO TRUE
              END-IF
           END-IF
           .
       R51-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R52-BEARB-FORTSETZUNG                                      *
      *------------------------------------------------------------*
      * INPUT : S-OP-RC, CARD-OP-TAB                               *
      *                                                            *
      * OUTPUT: S-BEARB-STATUS, DS-STATUS,                         *
      *         CADI-OP-AKT                                        *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     AUSWERTUNG DES S-OP-RC DER FACHFUNKTION;               *
      *     FALLS KEINE UNTERBRECHUNG                              *
      *     UND WEITERE OPERATIONEN IN OP-TABELLE VORHANDEN        *
      *        FUELLEN VON CADI-OP-AKT                             *
      *                                                            *
      *------------------------------------------------------------*
       R52-BEARB-FORTSETZUNG SECTION.
       R52-ANF.
           MOVE 'R52  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    -------------------------------------------------*
      *    * AUSWERTUNG DES S-OP-RC DER FACHFUNKTION
           EVALUATE TRUE
      *    -------------------------------------------------*
           WHEN OP-OK
              IF CARD-OP-IND < CARD-OP-MAX
                 ADD 1                        TO CARD-OP-IND
                 IF CARD-OP(CARD-OP-IND) NOT = SPACE
      *             * WEITERE OPERATION VORHANDEN
                    MOVE CARD-OP(CARD-OP-IND) TO CADI-OP-AKT
                    SET OP-RC-INIT            TO TRUE
                    SET OP-STATUS-ERSTAUFRUF  TO TRUE
                    SET BEARB-OP              TO TRUE
                 ELSE
                    SET DS-FUSS-VERSORGEN     TO TRUE
                    SET BEARB-NOP             TO TRUE
                 END-IF
              END-IF
      *    -------------------------------------------------*
           WHEN OP-OK-VERZWEIGEN
              INITIALIZE CADI-OP-AKT
              SET BILDLAST-NUR-BEI-FEHLER     TO TRUE
              SET DS-VERZWEIGEN               TO TRUE
              SET BEARB-NOP                   TO TRUE
      *    -------------------------------------------------*
           WHEN OP-ABBR
              INITIALIZE CADI-OP-AKT
                         CARD-OP-TAB
              SET DS-FUSS-VERSORGEN           TO TRUE
              SET BEARB-NOP                   TO TRUE
      *    -------------------------------------------------*
           WHEN OP-ABBR-VERZWEIGEN
              INITIALIZE CADI-OP-AKT
                         CARD-OP-TAB
              SET DS-VERZWEIGEN               TO TRUE
              SET BEARB-NOP                   TO TRUE
      *    -------------------------------------------------*
           WHEN OP-ABBR-BILDLAST
              INITIALIZE CADI-OP-AKT
                         CARD-OP-TAB
              SET BILDLAST-JA                 TO TRUE
              SET DS-FUSS-VERSORGEN           TO TRUE
              SET BEARB-NOP                   TO TRUE
      *    -------------------------------------------------*
           WHEN OP-ABBR-VERZWEIGEN-BILDLAST
              INITIALIZE CADI-OP-AKT
                         CARD-OP-TAB
              SET BILDLAST-JA                 TO TRUE
              SET DS-VERZWEIGEN               TO TRUE
              SET BEARB-NOP                   TO TRUE
      *    -------------------------------------------------*
           WHEN OP-UNTERBR-VERZWEIGEN
              SET BILDLAST-NUR-BEI-FEHLER     TO TRUE
              SET DS-VERZWEIGEN               TO TRUE
              SET BEARB-NOP                   TO TRUE
      *    -------------------------------------------------*
           WHEN NOT OP-RC-BEARBEITUNG-GUELTIG
              MOVE K-28                       TO ERR-ORT-LFD
              MOVE K-7                        TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    -------------------------------------------------*
           END-EVALUATE
      *    -------------------------------------------------*
           .
       R52-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R6-FUSS-VERSORGEN                                          *
      *------------------------------------------------------------*
      * INPUT : CADI-S-ABSCHLUSS,                                  *
      *         CADI-FTASTEN-ZUSTAND, CADI-FTASTEN-NR,             *
      *         CARD-MELDRAHM-KEY, CADI-MELDUNG-KEY,               *
      *         CADI-FELDNAME                                      *
      *                                                            *
      * OUTPUT: DS-STATUS, FUSS DER BILDSCHIRMANZEIGE              *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     VERSORGEN DER FUSSZEILEN                               *
      *     ANSCHLIESSEND LOESCHEN DER MELDUNG-INPUT-FELDER        *
      *     BELASSEN DER CADI-FTASTEN-NR                           *
      *                                                            *
      *------------------------------------------------------------*
       R6-FUSS-VERSORGEN SECTION.
       R6-ANF.
           MOVE 'R6   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
170299*>>>>>
      *EURO-SWITCH
           EVALUATE TRUE                  ALSO TRUE
           WHEN CARD-EURO-SWITCH-URSPRUNG ALSO
                                          CARD-EURO-SWITCH-OHNE-FEHLER
                 EVALUATE S-BILDNR
                 WHEN K-1
                    MOVE M1O TO Z-EURO-SWITCH-EINBER
                 WHEN K-2
                    MOVE M2O TO Z-EURO-SWITCH-EINBER
                 WHEN K-3
                    MOVE M3O TO Z-EURO-SWITCH-EINBER
                 WHEN OTHER
                    CONTINUE
                 END-EVALUATE

      *          NUR DIE WÄHRUNGSTEXTE WERDEN GEFÜLLT
                 SET Z-EURO-SWITCH-NUR-TEXTE TO TRUE
                 PERFORM UR6-EURO-UMRECHNEN
                 EVALUATE TRUE
                 WHEN PO-DRAT0283-RC-OK
                    MOVE PO-DRAT0283-STD-WAE TO
                         CARD-EURO-SWITCH-STD-WAE
                    MOVE PO-DRAT0283-ALT-WAE TO
                         CARD-EURO-SWITCH-ALT-WAE
                    MOVE K-5 TO Z-TSQ-ITEM
                    PERFORM UR7-EURO-MASKE-SICHERN
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

                 WHEN OTHER
                      CONTINUE
                 END-EVALUATE

           WHEN CARD-EURO-SWITCH-UMRECHNEN ALSO
                                          CARD-EURO-SWITCH-OHNE-FEHLER

                 EVALUATE S-BILDNR
                 WHEN K-1
                    MOVE M1O TO Z-EURO-SWITCH-EINBER
                 WHEN K-2
                    MOVE M2O TO Z-EURO-SWITCH-EINBER
                 WHEN K-3
                    MOVE M3O TO Z-EURO-SWITCH-EINBER
                 WHEN OTHER
                    CONTINUE
                 END-EVALUATE
      *          MASKENBETRÄGE WERDEN UMGERECHNET UND WÄHRUNGSGTEXTE
      *          GEFÜLLT
                 SET Z-EURO-SWITCH-ALLES TO TRUE
                 PERFORM UR6-EURO-UMRECHNEN

                 IF PO-DRAT0283-RC-NOK
                    SET CARD-EURO-SWITCH-MIT-FEHLER TO TRUE
                 END-IF

                 EVALUATE TRUE

                   WHEN CARD-EURO-SWITCH-MIT-FEHLER
      *    UMRECHNUNG FEHLERHAFT
                        SET CARD-EURO-SWITCH-URSPRUNG TO TRUE
                        MOVE K-AT-PGM-NAME         TO CARD-MELDRAHM-PGM
                        MOVE K-E        TO CARD-MELDRAHM-TYP
                        MOVE 35         TO CARD-MELDRAHM-NR
                        SET FTASTE-ABBR TO TRUE
                        MOVE K-CURSOR   TO ERTRANSL IN MFI
                       SET CARD-EURO-SWITCH-MIT-FEHLER TO TRUE

                   WHEN OTHER

                       SET CARD-EURO-SWITCH-UMGERECHNET TO TRUE
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

                 END-EVALUATE

           WHEN CARD-EURO-SWITCH-ZURUECK ALSO
                                          CARD-EURO-SWITCH-OHNE-FEHLER
                 MOVE K-5 TO Z-TSQ-ITEM
                 PERFORM UR8-EURO-MASKE-ENTSICHERN

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

                 MOVE K-6 TO Z-TSQ-ITEM
                 PERFORM UR8F-EURO-FUSS-ENTSICHERN
           WHEN OTHER
                 CONTINUE
           END-EVALUATE
170299*<<<<<
      *
               PERFORM R61-HINWEIS-VERSORGEN
               PERFORM R62-CTV-ETC-VERSORGEN
               PERFORM R63-ABSCHLUSS-VERSORGEN

      *    * AENDERUNGEN AM FUSS - ABH. VOM DIALOG-STATUS
      *    * (NUR AUF OBERSTER DIALOG-EBENE)
200197*     IF CARD-DIALOG-SCHRITT-AKT = CARR-DIALOG-AKT
200197     IF CARR-UANW = ZERO     AND
200197        CARR-PAGE = 1
200197     THEN
              PERFORM ZR3-FUSS-DIALOG-STATUS-ABH
           END-IF

150395     PERFORM R64-FTASTEN-VERSORGEN

           EVALUATE TRUE
           WHEN CARD-EURO-SWITCH-URSPRUNG
                MOVE K-6 TO Z-TSQ-ITEM
                PERFORM UR7F-EURO-FUSS-SICHERN
           WHEN OTHER
                CONTINUE
           END-EVALUATE
      *

           EVALUATE TRUE                 ALSO
                                          TRUE
           WHEN CARD-EURO-SWITCH-ZURUECK ALSO
                                          CARD-EURO-SWITCH-OHNE-FEHLER
                 SET CARD-EURO-SWITCH-URSPRUNG TO TRUE
                 MOVE K-6 TO Z-TSQ-ITEM
                 PERFORM UR8F-EURO-FUSS-ENTSICHERN

           WHEN OTHER
                 CONTINUE
           END-EVALUATE

           SET DS-BILDAUSGABE                 TO TRUE

           .
       R6-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R61-HINWEIS-VERSORGEN                                      *
      *------------------------------------------------------------*
      * INPUT : CARD-MELDRAHM-KEY, CADI-MELDUNG-KEY,               *
      *         T-BILD-MAP, CADI-FELDNAME                          *
      *                                                            *
      * OUTPUT: HINWEIS-ZEILE IM MASKENFUSS                        *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   VERSORGEN DER HINWEISZEILE MIT FOLGENDER PRIORITÄT:      *
      *   1. HINWEIS-/FEHLERTEXT VOM RAHMEN/DRIVER                 *
      *   2. HINWEIS-/FEHLERTEXT VON FACHFUNKTION                  *
      *   3. KURZHILFE-ZEILE                                       *
      *   ANSCHLIESSEND LOESCHEN DER INPUT-FELDER                  *
      *                                                            *
      *------------------------------------------------------------*
       R61-HINWEIS-VERSORGEN SECTION.
       R61-ANF.
           MOVE 'R61  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           IF CARD-MELDRAHM-NR  NOT = ZERO

      *       * LESEN MELDUNG FUER RAHMEN/DRIVER
              MOVE CARD-MELDRAHM-KEY          TO PI-UPAT0003-MELDUNG-KEY

              SET PI-UPAT0003-OP-MELDUNG      TO TRUE
170299        MOVE CARD-EURO-SWITCH       TO PI-UPAT0003-S-EURO-SWITCH
              PERFORM R611-MELDUNG-AUFRUF
270995        PERFORM R612-VAR-TEXTE-FUELLEN
              MOVE PO-UPAT0003-HINWEIS        TO ARHINWEO IN MFO
030297        MOVE DFHBMASB                   TO ARHINWEA IN MFI

           ELSE
030297*        IF CADI-MELDUNG-NR NOT = ZERO
030297         IF CADI-MELDUNG-TYP NOT = SPACE

210897*             * LESEN MELDUNG FUER VSTATUS UND KTY-KNR
030297
210897
210897            EVALUATE CADI-MELDUNG-TYP
210897            WHEN K-V
210897                   PERFORM R613-MELDUNG-VSTAT
210897            WHEN K-K
210897                   PERFORM R614-MELDUNG-KTY-KNR
210897            WHEN OTHER
                    MOVE CADI-MELDUNG-KEY    TO PI-UPAT0003-MELDUNG-KEY
170299              MOVE CARD-EURO-SWITCH       TO
170299                   PI-UPAT0003-S-EURO-SWITCH
                    SET PI-UPAT0003-OP-MELDUNG   TO TRUE
                    PERFORM R611-MELDUNG-AUFRUF
270995              PERFORM R612-VAR-TEXTE-FUELLEN
                    MOVE PO-UPAT0003-HINWEIS     TO ARHINWEO IN MFO
030297              MOVE DFHBMASB                TO ARHINWEA IN MFI
210897            END-EVALUATE

              ELSE
                 IF CADI-FELDNAME NOT = SPACE

      *             * KURZHILFE-ZEILE LESEN
      *             * ZUNAECHST PRUEFEN, OB S-BILDNR PLAUSIBEL IST
                    IF S-BILDNR > I-BILD-MAX OR
                       S-BILDNR < K-1

      *                * PROGRAMMIERFEHLER
                       MOVE K-29              TO ERR-ORT-LFD
                       MOVE K-9               TO ERR-MELD-NR
                       PERFORM UR93-PROG-FEHLER
                    ELSE
                       SET I-BILD             TO S-BILDNR
                     MOVE T-BILD-MAP (I-BILD) TO PI-UPAT0003-HELP-MAP-ID
                    END-IF

                    MOVE CADI-FELDNAME     TO PI-UPAT0003-HELP-FELD-NAME
      *             * ERMITTELN KURZHILFE-ZEILE
                    SET PI-UPAT0003-OP-HELP   TO TRUE
170299              MOVE CARD-EURO-SWITCH       TO
170299                   PI-UPAT0003-S-EURO-SWITCH
                    PERFORM R611-MELDUNG-AUFRUF
270995              PERFORM R612-VAR-TEXTE-FUELLEN
                    MOVE PO-UPAT0003-HINWEIS TO ARHINWEO IN MFO
                    MOVE DFHBMASB          TO ARHINWEA IN MFI

                 ELSE
200601*>>>>>
090301*>>>>>
      *          IF CARD-FREMD-MELDUNG-NR  NOT = ZERO
      *
      *       * LESEN MELDUNG FREMDSYSTEM
      *           MOVE CARD-FREMD-MELDUNG-KEY TO PI-UPAT0003-MELDUNG-KEY
      *
      *             SET PI-UPAT0003-OP-MELDUNG      TO TRUE
      *             MOVE CARD-EURO-SWITCH TO PI-UPAT0003-S-EURO-SWITCH
      *             PERFORM R611-MELDUNG-AUFRUF
      *             PERFORM R612-VAR-TEXTE-FUELLEN
      *             MOVE PO-UPAT0003-HINWEIS        TO ARHINWEO IN MFO
      *             MOVE DFHBMASB                   TO ARHINWEA IN MFI
      *
      *          ELSE
      *             * KEINE HINWEISZEILE AUSZUGEBEN
      *             MOVE SPACE                TO ARHINWEO IN MFO
      *             MOVE DFHBMASK             TO ARHINWEA IN MFI
      *          END-IF
090301*<<<<<
200601*<<<<<
200601*>>>>>
310106*          IF CARD-CTV-MELDUNG  NOT = SPACE
310106           IF CADD-CTV-MELDUNG  NOT = SPACE

      *             ERMITTELN CTV-MELDUNG
                    PERFORM R65-CTV-MELDUNG-ERMITTELN

                    EVALUATE TRUE
                    WHEN PO-DRAT0344-RC-OK
                         MOVE PO-DRAT0344-KNR-BEZ   TO ARHINWEO IN MFO
                         MOVE DFHBMASB              TO ARHINWEA IN MFI

                    WHEN PO-DRAT0344-RC-NOT-FOUND
310106*                  STRING CARD-CTV-MELDUNG DELIMITED BY SIZE
310106                   STRING CADD-CTV-MELDUNG DELIMITED BY SIZE
                         ' NICHT DEFINIERTE MELDUNG' DELIMITED BY SIZE
                         INTO                          ARHINWEO IN MFO
                         MOVE DFHBMASB              TO ARHINWEA IN MFI

                    END-EVALUATE

      *             SET PI-UPAT0003-OP-MELDUNG      TO TRUE
      *             MOVE CARD-EURO-SWITCH TO PI-UPAT0003-S-EURO-SWITCH
      *             PERFORM R611-MELDUNG-AUFRUF
      *             PERFORM R612-VAR-TEXTE-FUELLEN
      *             MOVE PO-UPAT0003-HINWEIS        TO ARHINWEO IN MFO
      *             MOVE DFHBMASB                   TO ARHINWEA IN MFI
      *

                 ELSE
      *             * KEINE HINWEISZEILE AUSZUGEBEN
                    MOVE SPACE                TO ARHINWEO IN MFO
                    MOVE DFHBMASK             TO ARHINWEA IN MFI
                 END-IF

090301*<<<<<
200601*<<<<<
                 END-IF
              END-IF
           END-IF

           MOVE SPACE                         TO CARD-MELDRAHM-TYP
                                                 CADI-MELDUNG-TYP
200601*                                          CARD-FREMD-MELDUNG-TYP
310106*                                          CARD-CTV-MELDUNG
310106                                           CADD-CTV-MELDUNG
           MOVE ZERO                          TO CARD-MELDRAHM-NR
                                                 CADI-MELDUNG-NR
200601*                                          CARD-FREMD-MELDUNG-NR
           MOVE SPACE                         TO CADI-FELDNAME
           .
       R61-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R611-MELDUNG-AUFRUF                                        *
      *------------------------------------------------------------*
      * INPUT : PI-UPAT0003-EINGABE                                *
      * OUTPUT: PI-UPAT0003-AUSGABE                                *
      * VERARBEITUNG:                                              *
      *     AUFRUF UPAT0003 (MELDUNG ERMITTELN)                    *
      *     AUSWERTEN ERR-RC                                       *
      *------------------------------------------------------------*
       R611-MELDUNG-AUFRUF SECTION.
       R611-ANF.
           MOVE 'R611 ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
030297*    CALL 'UPAT0003' USING BY CONTENT   PI-UPAT0003-EINGABE
CCCPY1*    CALL K-UPAT0003 USING BY CONTENT   PI-UPAT0003-EINGABE
CCC004*                          BY REFERENCE PO-UPAT0003-AUSGABE
CCC004*                          BY REFERENCE ERR-ERROR-BEREICH
CCCPY2*    END-CALL
           EVALUATE TRUE
           WHEN ERR-RC-OK
              CONTINUE
           WHEN OTHER
              PERFORM UR99-SCHWERER-FEHLER
           END-EVALUATE
           .
       R611-EXIT.
           EXIT.

270995* GANZE SECTION NEU
      *------------------------------------------------------------*
       R612-VAR-TEXTE-FUELLEN SECTION.
       R612-ANF.
      *------------------------------------------------------------*
      * INPUT : PO-UPAT0003-HINWEIS, CADI-MELDUNG-VT-FELDER        *
      * OUTPUT: PO-UPAT0003-HINWEIS (MODIFIZIERT)                  *
      * VERARBEITUNG:                                              *
      *    PRÜFEN, OB HINWEISZEILE IDENTIFIER FÜR VAR-TEXTE ENTHÄLT*
      *    FALLS JA                                                *
      *       UNSTRING HINWEISZEILE (ALLE TEILE VOR, ZWISCHEN UND  *
      *          NACH DEN IDENTIFIERN)                             *
      *       STRING DER VARIABLEN UND FIXEN TEILE IN HINWEISZEILE *
      *------------------------------------------------------------*
           MOVE 'R612 ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           INITIALIZE                      Z-VT-ZAEHLER
           INSPECT PO-UPAT0003-HINWEIS
              TALLYING Z-VT-ZAEHLER
              FOR ALL '#'

           IF Z-VT-ZAEHLER > ZERO
              INITIALIZE Z-VT-FELDER
              UNSTRING PO-UPAT0003-HINWEIS
                DELIMITED BY ALL '#'
                INTO Z-VT-FELD1,
                     Z-VT-FELD2,
                     Z-VT-FELD3,
                     Z-VT-FELD4
              END-UNSTRING
              STRING Z-VT-FELD1        DELIMITED BY '   ',
                     CADI-MELDUNG-VT1  DELIMITED BY '   ',
                     Z-VT-FELD2        DELIMITED BY '   ',
                     CADI-MELDUNG-VT2  DELIMITED BY '   ',
                     Z-VT-FELD3        DELIMITED BY '   ',
                     CADI-MELDUNG-VT3  DELIMITED BY '   ',
                     Z-VT-FELD4        DELIMITED BY '   '
                INTO PO-UPAT0003-HINWEIS
              END-STRING
              INITIALIZE                   CADI-MELDUNG-VT-FELDER
           END-IF
           .
       R612-EXIT.
           EXIT.

210897* GANZE SECTION NEU
210897*------------------------------------------------------------*
210897 R613-MELDUNG-VSTAT SECTION.
210897 R613-ANF.
210897*------------------------------------------------------------*
210897* INPUT : PI-DRAT0149-PVST-VSTAT, PI-UPAT0018                *
210897* OUTPUT: PO-DRAT0149-PVST-MELDUNG                           *
210897* VERARBEITUNG:                                              *
210897*    - DYNAMISCHER CALL AUF UPAT0018, ZUR ERMITTLUNG         *
210897*      DES VSTATUS-TEXT                                      *
210897*------------------------------------------------------------*
210897     MOVE 'R613 ' TO ERR-ORT-SEC
210897     PERFORM ZR1-TRACE-KURZ
210897*
030297     MOVE CADI-MELDUNG-VSTAT           TO PI-DRAT0149-PVST-VSTAT
210897*
210897
030297     SET PI-UPAT0018-DRAT0149          TO TRUE
210897*
CCCPY1*    CALL K-UPAT0018 USING BY REFERENCE  PI-UPAT0018
CCC004*                                        PI-DRAT0149
CCC004*                                        PO-DRAT0149
CCC004*                                        ERR-ERROR-BEREICH
CCC004*                                        STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
210897
210897     IF ERR-RC-SCHWERER-FEHLER
210897        PERFORM UR99-SCHWERER-FEHLER
210897     END-IF
210897

210897     EVALUATE TRUE
           WHEN PO-DRAT0149-RC-OK
              MOVE PO-DRAT0149-PVST-TEXT       TO ARHINWEO IN MFO
           WHEN OTHER
              MOVE 'DPAT0001-149/UPAT0018:RETURN-CODE NICHT OK'
                                             TO ERR-VAR-ZEILE01
              MOVE PO-DRAT0149-RC            TO ERR-VAR-ZEILE02
              MOVE K-30                      TO ERR-ORT-LFD
              PERFORM UR93-PROG-FEHLER
           END-EVALUATE


210897     MOVE PO-DRAT0149-PVST-ATTRIBUT   TO ARHINWEA IN MFI
210897*
210897     .
210897 R613-EXIT.
210897     EXIT.

210897* GANZE SECTION NEU
210897*------------------------------------------------------------*
210897 R614-MELDUNG-KTY-KNR SECTION.
210897 R614-ANF.
210897*------------------------------------------------------------*
210897* INPUT : PI-DRAT0150-KTY UND KNR PI-UPAT0018                *
210897* OUTPUT: PO-DRAT0150-KTY-KNR-MELDUNG                        *
210897* VERARBEITUNG:                                              *
210897*    - DYNAMISCHER CALL AUF UPAT0018, ZUR ERMITTLUNG         *
210897*      DES KTY-NR MELDUNG                                    *
210897*------------------------------------------------------------*
210897     MOVE 'R614 ' TO ERR-ORT-SEC
210897     PERFORM ZR1-TRACE-KURZ
210897*
030297     MOVE CADI-MELDUNG-KTY             TO PI-DRAT0150-KTY
030297     MOVE CADI-MELDUNG-KNR             TO PI-DRAT0150-KNR
210897*
210897
030297     SET PI-UPAT0018-DRAT0150          TO TRUE
210897*
CCCPY1*    CALL K-UPAT0018 USING BY REFERENCE  PI-UPAT0018
CCC004*                                        PI-DRAT0150
CCC004*                                        PO-DRAT0150
CCC004*                                        ERR-ERROR-BEREICH
CCC004*                                        STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
210897
210897     IF ERR-RC-SCHWERER-FEHLER
210897        PERFORM UR99-SCHWERER-FEHLER
210897     END-IF
210897*
210897     EVALUATE TRUE
           WHEN PO-DRAT0150-RC-OK
              MOVE PO-DRAT0150-TEXT           TO ARHINWEO IN MFO
           WHEN OTHER
              MOVE 'DPAT0001-150/UPAT0018:RETURN-CODE NICHT OK'
                                             TO ERR-VAR-ZEILE01
              MOVE PO-DRAT0150-RC            TO ERR-VAR-ZEILE02
              MOVE K-31                      TO ERR-ORT-LFD
              PERFORM UR93-PROG-FEHLER
           END-EVALUATE

210897     MOVE PO-DRAT0150-ATTRIBUT   TO ARHINWEA IN MFI
210897     .
210897 R614-EXIT.
210897     EXIT.

      *------------------------------------------------------------*
      * R62-CTV-ETC-VERSORGEN                                      *
      *------------------------------------------------------------*
      * INPUT : CARD-DIALOG-SCHRITT-AKT                            *
      *                                                            *
      * OUTPUT: EINGABEFELDER DES FUSSES INITIALISIERT             *
      *         CTV-ANZEIGE ODER CTV-NICHT-ANZEIGE                 *
      * VERARBEITUNG:                                              *
      *     INITIALISIEREN DER EINGABEFELDER DES FUSSES            *
      *     PRÜFEN, OB DER TRANS MINDESTENS EIN BRIEF ÜBER         *
      *     SF14/KTY=193 ZUGEORDNET IST                            *
      *------------------------------------------------------------*
       R62-CTV-ETC-VERSORGEN SECTION.
       R62-ANF.
           MOVE 'R62  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
090301*>>>>>
      *
           MOVE SPACE                        TO ARCTVTXO IN MFO
           MOVE DFHBMASK                     TO ARCTVTXA IN MFI
           MOVE SPACE                        TO ERCTVO   IN MFO
           MOVE DFHBMASF                     TO ERCTVA   IN MFI

           SET  PI-ZWAT0008-DRAT0353    TO TRUE
           MOVE K-KTY-CTV-BRIEFE        TO PI-DRAT0353-KTY-NR
           MOVE CARD-DIALOG-SCHRITT-AKT TO PI-DRAT0353-KNR-BEZ-KURZ

CCCPY1*    CALL K-ZWAT0008 USING BY REFERENCE PI-ZWAT0008-FUNKTION
CCC004*                                       PI-DRAT0353
CCC004*                                       PO-DRAT0353
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

           IF ERR-RC-SCHWERER-FEHLER
              PERFORM UR99-SCHWERER-FEHLER
           END-IF

           EVALUATE TRUE

           WHEN PO-DRAT0353-RC-OK
           WHEN PO-DRAT0353-RC-MULT-ROWS
      *         MINDESTENS EIN BRIEF IST DER TRANSAKTION ZUGEORDNET
                SET CTV-ANZEIGE TO TRUE

           WHEN PO-DRAT0353-RC-NOT-FOUND
      *
                SET CTV-NICHT-ANZEIGE             TO TRUE

           END-EVALUATE
090301*<<<<<
           .
       R62-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R63-ABSCHLUSS-VERSORGEN                                    *
      *------------------------------------------------------------*
      * INPUT : CADI-S-ABSCHLUSS,                                  *
      *                                                            *
      * OUTPUT: ABSCHLUSS-FELD MIT INHALT (FALLS GEFORDERT)        *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     VERSORGEN DER ABSCHLUSS-FELDER IM MASKENFUSS           *
      *                                                            *
      *------------------------------------------------------------*
       R63-ABSCHLUSS-VERSORGEN SECTION.
       R63-ANF.
           MOVE 'R63  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      **** -----------------------------------------------*
      *    AUSWERTEN CADI-S-ABSCHLUSS
           EVALUATE TRUE
      *    -----------------------------------------------*
           WHEN ABSCHL-NICHT-ANZEIGE
              MOVE SPACE                     TO AROKTXTO IN MFO
              MOVE DFHBMASK                  TO AROKTXTA IN MFI
              MOVE SPACE                     TO EROKO    IN MFO
              MOVE DFHBMASF                  TO EROKA    IN MFI
      *    -----------------------------------------------*
           WHEN ABSCHL-ANZEIGE-N
              MOVE K-OK                      TO AROKTXTO IN MFO
              MOVE DFHBMASK                  TO AROKTXTA IN MFI
151093        IF NOT ABSCHL-ANZEIGE-UNVERAENDERT
                 MOVE K-N                    TO EROKO    IN MFO
151093        END-IF
              MOVE DFHBMFSE                  TO EROKA    IN MFI
      *    -----------------------------------------------*
           WHEN ABSCHL-ANZEIGE-J
              MOVE K-OK                      TO AROKTXTO IN MFO
              MOVE DFHBMASK                  TO AROKTXTA IN MFI
              MOVE K-CURSOR                  TO EROKL    IN MFI
151093        IF NOT ABSCHL-ANZEIGE-UNVERAENDERT
                 MOVE K-J                    TO EROKO    IN MFO
151093        END-IF
              MOVE DFHBMFSE                  TO EROKA    IN MFI
      *    -----------------------------------------------*
151093*     WHEN ABSCHL-ANZEIGE-UNVERAENDERT
151093*        CONTINUE
      *    -----------------------------------------------*
           WHEN OTHER
              MOVE K-32                      TO ERR-ORT-LFD
              MOVE K-10                      TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    --------------------------------------------------------*
           END-EVALUATE
      * ***--------------------------------------------------------*
           .
       R63-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R64-FTASTEN-VERSORGEN                                      *
      *------------------------------------------------------------*
      * INPUT : CADI-FTASTEN-ZUSTAND, CADI-FTASTEN-NR,             *
      *                                                            *
      * OUTPUT: FTASTEN-ZEILE IM MASKEN-FUSS                       *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     VERSORGEN DER FTASTEN-ZEILE                            *
      *     BELASSEN DER CADI-FTASTEN-NR                           *
      *                                                            *
      *------------------------------------------------------------*
       R64-FTASTEN-VERSORGEN SECTION.
       R64-ANF.
           MOVE 'R64  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           IF CADI-FTASTEN-NR NOT = ZERO
      *       * ÜBER CADI-FTASTEN-KEY DIE FTASTEN-ZEILE LESEN
              MOVE CADI-FTASTEN-KEY TO PI-UPAT0003-FTASTEN-KEY
              MOVE CARR-USITZG-ANZ  TO PI-UPAT0003-USITZG-ANZ
              MOVE K-USITZG-ANZ-MAX TO PI-UPAT0003-USITZG-ANZ-MAX
              MOVE CADI-S-ABSCHLUSS TO PI-UPAT0003-S-ABSCHLUSS
150395        MOVE CARD-S-CTV       TO PI-UPAT0003-S-CTV
170299        MOVE CARD-EURO-SWITCH TO PI-UPAT0003-S-EURO-SWITCH
170299*>>>>>  ERMITTELN WÄHRUNGSTEXTTEXT FÜR F20
                 EVALUATE TRUE
                 WHEN S-EURO-SWITCH-URSPRUNG
                 WHEN S-EURO-SWITCH-ZURUECK
170299              MOVE CARD-EURO-SWITCH-ALT-WAE TO
170299                     PI-UPAT0003-EURO-SWITCH-WAE

                 WHEN S-EURO-SWITCH-UMRECHNEN
                 WHEN S-EURO-SWITCH-UMGERECHNET
170299              MOVE CARD-EURO-SWITCH-STD-WAE TO
170299                   PI-UPAT0003-EURO-SWITCH-WAE

                 WHEN OTHER
                    MOVE K-ZERO TO PI-UPAT0003-EURO-SWITCH-WAE
                 END-EVALUATE
      *
      *       * ERMITTELN FTASTEN-ZEILE
              SET PI-UPAT0003-OP-FTASTEN        TO TRUE
030297*       CALL 'UPAT0003' USING BY CONTENT      PI-UPAT0003-EINGABE
CCCPY1*       CALL K-UPAT0003 USING BY CONTENT      PI-UPAT0003-EINGABE
CCC004*                             BY REFERENCE PO-UPAT0003-AUSGABE
CCC004*                             BY REFERENCE ERR-ERROR-BEREICH
CCCPY2*       END-CALL
              EVALUATE TRUE
              WHEN ERR-RC-OK
                 MOVE PO-UPAT0003-FTASTEN       TO ARFTASTO IN MFO
              WHEN OTHER
                 PERFORM UR99-SCHWERER-FEHLER
              END-EVALUATE
              MOVE DFHBMASK                     TO ARFTASTA IN MFI
           ELSE
              MOVE K-33                         TO ERR-ORT-LFD
              MOVE K-11                         TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
           END-IF
           .
       R64-EXIT.
           EXIT.
200601*>>>>>
      *------------------------------------------------------------*
      * R65-CTV-MELDUNG-ERMITTELN                                  *
      *------------------------------------------------------------*
      * INPUT : CADD-CTV-MELDUNG                                   *
      *                                                            *
      * OUTPUT: CTV-MELDUNG -> PO-DRAT0344-KNR-BEZ                 *
      *         PO-DRAT0344-RC                                     *
      * VERARBEITUNG:                                              *
      *     ERMITTELN CTV-MELDUNG ÜBER KTY=199                     *
      *------------------------------------------------------------*
       R65-CTV-MELDUNG-ERMITTELN SECTION.
       R65-ANF.
           MOVE 'R65  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *

           SET  PI-ZWAT0008-DRAT0344    TO TRUE
           MOVE K-KTY-CTV-MELDUNG       TO PI-DRAT0344-KTY-NR
310106*    MOVE CARD-CTV-MELDUNG        TO PI-DRAT0344-KNR-BEZ10
310106     MOVE CADD-CTV-MELDUNG        TO PI-DRAT0344-KNR-BEZ10

CCCPY1*    CALL K-ZWAT0008 USING BY REFERENCE PI-ZWAT0008-FUNKTION
CCC004*                                       PI-DRAT0344
CCC004*                                       PO-DRAT0344
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

           IF ERR-RC-SCHWERER-FEHLER
              PERFORM UR99-SCHWERER-FEHLER
           END-IF

           EVALUATE TRUE

           WHEN PO-DRAT0344-RC-OK
           WHEN PO-DRAT0344-RC-NOT-FOUND
      *
                CONTINUE

           WHEN OTHER
      *
              PERFORM UR99-SCHWERER-FEHLER

           END-EVALUATE
           .
       R65-EXIT.
           EXIT.


      *------------------------------------------------------------*
      * R7-BILDAUSGABE                                             *
      *------------------------------------------------------------*
      * INPUT : AKT. MAP                                           *
      *         TCTUA (WERTE FUER KOPF-AUSGABE)                    *
      *                                                            *
      * OUTPUT: DS-STATUS, MAP AUF BILDSCHIRM                      *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     VERSORGEN DER KOPFZEILEN                               *
      *     AUSGABE DER MAPS                                       *
      *                                                            *
      *------------------------------------------------------------*
       R7-BILDAUSGABE SECTION.
       R7-ANF.
           MOVE 'R7   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           PERFORM R71-KOPF-VERSORGEN
           PERFORM R72-TRANS-OB-INIT
           PERFORM R73-CURSOR-FUSS-PRUEFEN
030395*    * EVTL. INFO-DISPLAY IN FUSS-MASKE (ZEILE 21)
030395     PERFORM R75-INFO-DISPLAY
           PERFORM R74-BILD-SENDEN

           SET DS-RETURN                      TO TRUE
           .
       R7-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R71-KOPF-VERSORGEN                                         *
      *------------------------------------------------------------*
      * INPUT : TCTUA (WERTE FUER KOPF-AUSGABE)                    *
      *                                                            *
      * OUTPUT: GEFUELLTER BILDSCHIRM-KOPF                         *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     VERSORGEN DER KOPFFELDER                               *
      *                                                            *
      *------------------------------------------------------------*
       R71-KOPF-VERSORGEN SECTION.
       R71-ANF.
           MOVE 'R71  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           PERFORM R711-TITEL-VERSORG
      *    (IN SEPARATEM COPY-ELEMENT)

           MOVE CARR-DIALOG-AKT              TO ARDCODEO
           MOVE DFHBMASK                     TO ARDCODEA
           IF CARD-DIALOG-SCHRITT-AKT = CARR-DIALOG-AKT
              MOVE SPACE                     TO ARSCODEO
           ELSE
              MOVE CARD-DIALOG-SCHRITT-AKT   TO ARSCODEO
           END-IF
           MOVE DFHBMASK                     TO ARSCODEA                
           MOVE S-BILDNR                     TO ARBFAKTO                
           MOVE I-BILD-MAX                   TO Z-BFANZ-NUM             
           MOVE Z-BFANZ-NUM                  TO ARBFANZO                
070396     MOVE '('                          TO ARBFTX1O                
070396     MOVE ')'                          TO ARBFTX2O                
           MOVE DFHBMASK                     TO ARBILDFA                
           MOVE D-AKT-DATUM-KURZ             TO ARDATAKO                
           MOVE D-AKT-ZEIT-KURZ              TO ARTIMAKO                
070396     MOVE '/'                          TO ARDTITXO                
           MOVE DFHBMASK                     TO ARDTIAKA                
                                                                        
           IF CARR-USITZG-ANZ > ZERO
              MOVE K-U                       TO ARMODEUO
      ****    MOVE DFHBLINK                  TO ARMODEUA
              MOVE DFHBMASB                  TO ARMODEUA
           ELSE
              MOVE SPACE                     TO ARMODEUO
              MOVE DFHBMASK                  TO ARMODEUA
           END-IF

040797     MOVE CARU-CICS-NAME               TO ARSYSBZO
040797*    EVALUATE TRUE
      *    WHEN SYSTEM-PROD
      *       MOVE K-PROD                    TO ARSYSBZO
      *    WHEN SYSTEM-TEST
      *       MOVE K-TEST                    TO ARSYSBZO
      *    WHEN OTHER
      *       MOVE K-1                       TO ERR-ORT-LFD
      *       PERFORM UR93-PROG-FEHLER
040797*    END-EVALUATE
           MOVE DFHBMASK                     TO ARSYSBZA

260894*    MOVE TS-PERSNR                    TO ARPERSNO
260894     MOVE TS-USERID                    TO ARPERSNO
           MOVE DFHBMASK                     TO ARPERSNA

           IF CADU-CICS-DATUM = Z-AKT-DATUM-RECH
              MOVE SPACE                     TO ARBEZCIO
              MOVE SPACE                     TO ARDATCIO
           ELSE
              MOVE K-BEZ-CICS-DATUM          TO ARBEZCIO
              MOVE D-CICS-DATUM-KURZ         TO ARDATCIO
           END-IF
           MOVE DFHBMASK                     TO ARBEZCIA
           MOVE DFHBMASK                     TO ARDATCIA
           .
       R71-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R72-TRANS-OB-INIT                                          *
      *------------------------------------------------------------*
      * INPUT : -                                                  *
      *                                                            *
      * OUTPUT: INITIAL. ERTRANS, ERKEY                            *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     INITIALISIERUNG DER FUSSFELDER, DIE IMMER INITIALISIERT*
      *     WERDEN MUESSEN                                         *
      *                                                            *
      *------------------------------------------------------------*
       R72-TRANS-OB-INIT SECTION.
       R72-ANF.
           MOVE 'R72  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           MOVE SPACE                        TO ERTRANSO IN MFO
           MOVE DFHBMFSE                     TO ERTRANSA IN MFI

           MOVE SPACE                        TO ERKEYO   IN MFO
           MOVE DFHBMFSE                     TO ERKEYA   IN MFI
           .
       R72-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R73-CURSOR-FUSS-PRUEFEN                                    *
      *------------------------------------------------------------*
      * INPUT : ERTRANSL, ERKEYL, ERCTVL, EROKL                    *
      *                                                            *
      * OUTPUT: CARD-CURSOR-FUSS                                   *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     FALLS FUER MINDESTENS EIN MASKENFUSS-EINGABEFELD       *
      *     DER CURSOR GESETZT IST, SETZEN VON CURSOR-FUSS-JA      *
      *                                                            *
      *------------------------------------------------------------*
       R73-CURSOR-FUSS-PRUEFEN SECTION.
       R73-ANF.
           MOVE 'R73  '                  TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
170299*>>>>>
           EVALUATE TRUE
           WHEN CARD-EURO-SWITCH-UMGERECHNET
                 MOVE K-CURSOR TO ERTRANSL IN MFI
           WHEN OTHER
                 CONTINUE
           END-EVALUATE
170299*<<<<<
           IF BILDLAST-JA
              SET CURSOR-FUSS-CURABS     TO TRUE
           ELSE
              EVALUATE TRUE
              WHEN ERTRANSL IN MFI = K-CURSOR
              WHEN ERKEYL   IN MFI = K-CURSOR
              WHEN ERCTVL   IN MFI = K-CURSOR
              WHEN EROKL    IN MFI = K-CURSOR
                 SET CURSOR-FUSS-JA      TO TRUE
              WHEN OTHER
                 SET CURSOR-FUSS-NEIN    TO TRUE
              END-EVALUATE
           END-IF
      *    * INIT. FUER NAECHSTE RUNDE
           SET BILDLAST-NEIN             TO TRUE
           .
       R73-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R74-BILD-SENDEN                                            *
      *------------------------------------------------------------*
      * INPUT : MAP-KOPF, MAP-FUSS, AKT. RUMPF-MAP                 *
      *                                                            *
      * OUTPUT: MAPS AUF BILDSCHIRM                                *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     AUSGABE DER RAHMEN-MAPS UND DER AKT. RUMPF-MAP         *
      *                                                            *
      *------------------------------------------------------------*
       R74-BILD-SENDEN SECTION.
       R74-ANF.
           MOVE 'R74  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      ****  VERSORGEN Z-MAP-NEXT   ****
      *
           IF S-BILDNR > I-BILD-MAX
           OR S-BILDNR < K-1
      *       * PROGRAMMIERFEHLER
              MOVE K-34                       TO ERR-ORT-LFD
              MOVE K-9                        TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
           ELSE
              SET I-BILD                      TO S-BILDNR
              MOVE T-BILD-MAP (I-BILD)        TO Z-MAP-NEXT
           END-IF

      ****  SENDEN MAP-RAHMEN - KOPF  ****

           IF Z-MAP-NEXT = CARU-MAP-LAST
      *       * ALTE MAP --> DATAONLY
CCC005*       EXEC CICS SEND MAP    (K-MAP-RAHMEN-KOPF)
CCC004*                      MAPSET (K-MAP-RAHMEN-KOPF)
CCC004*                      FROM   (BAT0001O)
CCC004*                      DATAONLY
CCC004*                      ACCUM
CCC004*                      RESP   (C-CICS-RC)
CCC003*       END-EXEC
           ELSE
      *       * NEUE MAP --> ERASE
CCC005*       EXEC CICS SEND MAP    (K-MAP-RAHMEN-KOPF)
CCC004*                      MAPSET (K-MAP-RAHMEN-KOPF)
CCC004*                      FROM   (BAT0001O)
CCC004*                      ERASE
CCC004*                      ACCUM
CCC004*                      RESP   (C-CICS-RC)
CCC003*       END-EXEC
           END-IF
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-35        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF
      * ***-------------------------------------------------*
      *    * MAP-AKTUELL AUSGEBEN
      *    * MAP-QUELLE ABHAENGIG VON S-BILDNR
           EVALUATE S-BILDNR
      *    -------------------------------------------------*
           WHEN K-1
              IF Z-MAP-NEXT = CARU-MAP-LAST
      *          * ALTE MAP --> DATAONLY
CCC005*          EXEC CICS SEND MAP    (Z-MAP-NEXT)
CCC004*                         FROM   (M1O)
CCC004*                         CURSOR
CCC004*                         DATAONLY
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              ELSE
      *          * NEUE MAP
CCC005*          EXEC CICS SEND MAP    (Z-MAP-NEXT)
CCC004*                         FROM   (M1O)
CCC004*                         CURSOR
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              END-IF
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-36        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    -------------------------------------------------*
           WHEN K-2
              IF Z-MAP-NEXT = CARU-MAP-LAST
      *          * ALTE MAP --> DATAONLY
CCC005*          EXEC CICS SEND MAP    (Z-MAP-NEXT)
CCC004*                         FROM   (M2O)
CCC004*                         CURSOR
CCC004*                         DATAONLY
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              ELSE
      *          * NEUE MAP
CCC005*          EXEC CICS SEND MAP    (Z-MAP-NEXT)
CCC004*                         FROM   (M2O)
CCC004*                         CURSOR
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              END-IF
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-37        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    -------------------------------------------------*
           WHEN K-3
              IF Z-MAP-NEXT = CARU-MAP-LAST
      *          * ALTE MAP --> DATAONLY
CCC005*          EXEC CICS SEND MAP    (Z-MAP-NEXT)
CCC004*                         FROM   (M3O)
CCC004*                         CURSOR
CCC004*                         DATAONLY
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              ELSE
      *          * NEUE MAP
CCC005*          EXEC CICS SEND MAP    (Z-MAP-NEXT)
CCC004*                         FROM   (M3O)
CCC004*                         CURSOR
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              END-IF
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-38        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    -------------------------------------------------*
           WHEN OTHER
              MOVE K-39                  TO ERR-ORT-LFD
              MOVE K-9                   TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    -------------------------------------------------*
           END-EVALUATE
      * ***-------------------------------------------------*

      ****  SENDEN MAP-RAHMEN - FUSS  ****

           IF Z-MAP-NEXT = CARU-MAP-LAST
      *       * ALTE MAP --> DATAONLY

              EVALUATE TRUE
              WHEN CURSOR-FUSS-NEIN
      *          * CURSOR AUF FELD IN RUMPF-MASKE
CCC005*          EXEC CICS SEND MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                         MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                         FROM   (MFO)
CCC004*                         DATAONLY
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              WHEN CURSOR-FUSS-JA
      *          * CURSOR AUF FELD IN MAP-RAHMEN-FUSS
CCC005*          EXEC CICS SEND MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                         MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                         FROM   (MFO)
CCC004*                         CURSOR
CCC004*                         DATAONLY
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              WHEN CURSOR-FUSS-CURABS
      *          * CURSOR AUF LETZTE ABSOLUTE POSITION PLAZIEREN
CCC005*          EXEC CICS SEND MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                         MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                         FROM   (MFO)
CCC004*                         CURSOR (CARD-CURABS)
CCC004*                         DATAONLY
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              END-EVALUATE
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-40        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF

           ELSE
      *       * NEUE MAP --> INCL. MAP *

              EVALUATE TRUE
              WHEN CURSOR-FUSS-NEIN
      *          * CURSOR AUF FELD IN RUMPF-MASKE
CCC005*          EXEC CICS SEND MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                         MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                         FROM   (MFO)
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              WHEN CURSOR-FUSS-JA
      *          * CURSOR AUF FELD IN MAP-RAHMEN-FUSS
CCC005*          EXEC CICS SEND MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                         MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                         FROM   (MFO)
CCC004*                         CURSOR
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              WHEN CURSOR-FUSS-CURABS
      *          * CURSOR AUF LETZTE ABSOLUTE POSITION PLAZIEREN
CCC005*          EXEC CICS SEND MAP    (K-MAP-RAHMEN-FUSS)
CCC004*                         MAPSET (K-MAP-RAHMEN-FUSS)
CCC004*                         FROM   (MFO)
CCC004*                         CURSOR (CARD-CURABS)
CCC004*                         ACCUM
CCC004*                         RESP   (C-CICS-RC)
CCC003*          END-EXEC
              END-EVALUATE
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-41        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
           END-IF

      ****   EVTL. TRACE-DISPLAY AB ZEILE 25  ****

           PERFORM R741-TRACE-DISPLAY

      ****   SEND PAGE, DA ALLE SENDS OK   ****

CCC005*    EXEC CICS SEND  PAGE
CCC004*                    RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-42        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

           MOVE Z-MAP-NEXT               TO CARU-MAP-LAST
           .
       R74-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R741-TRACE-DISPLAY                                         *
      *------------------------------------------------------------*
      * INPUT : CARU-S-TRACE-DISPLAY, CARU-S-SYSTEM,               *
      *                                                            *
      * OUTPUT: TRACE-INFOS AB BILDSCHIRMZEILE 25                  *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - PRUEFEN, OB TRACE-DISPLAY GEWÜNSCHT UND ERLAUBT IST    *
      *   - GGF. AUFBEREITEN TRACE-INFOS                           *
      *          SENDEN TRACE-INFOS AB ZEILE 25                    *
      *                                                            *
      *------------------------------------------------------------*
       R741-TRACE-DISPLAY SECTION.
       R741-ANF.
           MOVE 'R741 ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           IF SYSTEM-TEST       AND
              TRACE-DISPLAY-JA

      *       * ERR-TRACE AUSGEBEN (ZEILE 25-27)
              MOVE ERR-TRACE             TO ARTRACEO
              MOVE DFHBMASK              TO ARTRACEA
              MOVE ERR-VAR               TO ARVARO
              MOVE DFHBMASK              TO ARVARA

CCC005*       EXEC CICS SEND MAP    (K-MAP-RAHMEN-TRACE)
CCC004*                      MAPSET (K-MAP-RAHMEN-TRACE)
CCC004*                      FROM   (BAT0009O)
CCC004*                      ACCUM
CCC004*                      RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-43               TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
           END-IF
           .
       R741-EXIT.
           EXIT.

030395* GANZE SECTION NEU
      *------------------------------------------------------------*
      * R75-INFO-DISPLAY                                           *
      *------------------------------------------------------------*
      * INPUT : CARU-S-TRACE-DISPLAY                               *
      * OUTPUT: GEFÜLLTES FELD ARINFO                              *
      * VERARBEITUNG:                                              *
      *     ABH. VON SCHALTER DAS FELD ARINFO MIT STRICHEN ODER    *
      *     INFOS FÜLLEN                                           *
      *------------------------------------------------------------*
       R75-INFO-DISPLAY SECTION.
       R75-ANF.
           MOVE 'R75  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           IF INFO-DISPLAY-JA
              MOVE K-PGM-NAME                TO ARINFOO  IN MFO (01:08)
              MOVE SPACE                     TO ARINFOO  IN MFO (09:02)
              MOVE T-BILDER-TAB              TO ARINFOO  IN MFO (11:21)
              MOVE SPACE                     TO ARINFOO  IN MFO (32:02)
              MOVE 'ZST:'                    TO ARINFOO  IN MFO (34:04)
              MOVE S-DIALOG-ZUSTAND          TO ARINFOO  IN MFO (38:03)
              MOVE SPACE                     TO ARINFOO  IN MFO (41:02)
              MOVE 'FT-RC:'                  TO ARINFOO  IN MFO (43:06)
              MOVE S-FTASTE-RC               TO ARINFOO  IN MFO (49:01)
              MOVE SPACE                     TO ARINFOO  IN MFO (50:02)
              MOVE 'OP-RC:'                  TO ARINFOO  IN MFO (52:06)
              MOVE S-OP-RC                   TO ARINFOO  IN MFO (58:01)
              MOVE SPACE                     TO ARINFOO  IN MFO (59:02)
              MOVE SPACE                     TO ARINFOO  IN MFO (61:19)
           ELSE
              MOVE K-STRICH-ZEILE            TO ARINFOO  IN MFO
           END-IF
           MOVE DFHBMASK                     TO ARINFOA  IN MFI
           .
       R75-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R8-FORTSETZUNG                                             *
      *------------------------------------------------------------*
      * INPUT : DS-STATUS, CARR-DIALOG-AKT, MAPS                   *
      *                                                            *
      * OUTPUT: TS-QUEUE(MAPS)                                     *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - AUFRUF BILDER-SICHERN                                  *
      *   - SICHERN ANDERER WERTE IN CA                            *
      *   - ABH. VOM DS-STATUS VERZWEIGUNG ZUM DRIVER (XCTL)       *
      *        ODER ZUM CICS (RETURN TRANSID)                      *
      *                                                            *
      *------------------------------------------------------------*
       R8-FORTSETZUNG SECTION.
       R8-ANF.
           MOVE 'R8   ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           PERFORM R81-BILDER-SICHERN

210197***  -----------------------------------------------------------
210197***  ACHTUNG: WENN SECTION NICHT GEFUNDEN WIRD, DANN BITTE COPY
210197***           'DPAT0050' (DUMMY-SECTION) IN DIALOG-PGM AUFNEHMEN
210197     PERFORM DS-USERDATEN-SICHERN
210197***  -----------------------------------------------------------

           MOVE S-DIALOG-ZUSTAND        TO CARD-DIALOG-ZUSTAND
           MOVE ERR-TRACE               TO CARU-TRACE
      *
      * ***--------------------------------------------------------*
      *    * XCTL  ODER  RETURN TRANSID
      *    * ABHAENGIG VON DS-STATUS
           EVALUATE TRUE
      *    --------------------------------------------------------*
           WHEN DS-RETURN

              SET KEIN-DRIVER-AUFRUF    TO TRUE

CCC005*       EXEC CICS RETURN TRANSID  (CARD-DIALOG-SCHRITT-AKT)
CCC004*                        COMMAREA (C-COMMAREA)
CCC004*                        LENGTH   (K-CA-LEN)
CCC004*                        RESP     (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-44              TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    --------------------------------------------------------*
           WHEN DS-VERZWEIGEN
CCC005*       EXEC CICS XCTL   PROGRAM  (CARR-DRIVER-PGM)
CCC004*                        COMMAREA (C-COMMAREA)
CCC004*                        LENGTH   (K-CA-LEN)
CCC004*                        RESP     (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-45        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
      *    --------------------------------------------------------*
           WHEN OTHER
              MOVE K-46           TO ERR-ORT-LFD
              MOVE K-5            TO ERR-MELD-NR
              PERFORM UR93-PROG-FEHLER
      *    --------------------------------------------------------*
           END-EVALUATE
      * ***--------------------------------------------------------*
           .
       R8-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * R81-BILDER-SICHERN                                         *
      *------------------------------------------------------------*
      * INPUT : MAPS                                               *
      *                                                            *
      * OUTPUT: TS-QUEUE(MAPS)                                     *
      *                                                            *
      * VERARBEITUNG:                                              *
      *     ALLE MAPS IN TS-QUEUE(MAPS)                            *
      *------------------------------------------------------------*
       R81-BILDER-SICHERN SECTION.
       R81-ANF.
           MOVE 'R81  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      *    * SCHREIBEN MAP-RAHMEN-FUSS IN TS-QUEUE
           MOVE CARD-TSQMA-NAME         TO Z-TSQ-NAME
           MOVE K-1                     TO Z-TSQ-ITEM
CCC005*    EXEC CICS WRITEQ TS
CCC004*              QUEUE  (Z-TSQ-NAME)
CCC004*              FROM   (MFO)
CCC004*              ITEM   (Z-TSQ-ITEM) REWRITE
CCC004*              MAIN
CCC004*              RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-47        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

      *
      *    * SCHREIBEN MASKENRUMPF-MAPS IN TS-QUEUE
      *    * (ABHÄNGIG VON I-BILD-MAX)
      *
      *    * SCHREIBEN MAP 1 IN TS-QUEUE
170299*    IF I-BILD-MAX NOT < K-1
              MOVE K-1                  TO Z-TSQ-ITEM
              ADD  1                    TO Z-TSQ-ITEM
CCC005*       EXEC CICS WRITEQ TS
CCC004*                 QUEUE  (Z-TSQ-NAME)
CCC004*                 FROM   (M1O)
CCC004*                 ITEM   (Z-TSQ-ITEM) REWRITE
CCC004*                 MAIN
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-48        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
170299*    END-IF

      *    * SCHREIBEN MAP 2 IN TS-QUEUE
170299*    IF I-BILD-MAX NOT < K-2
              MOVE K-2                  TO Z-TSQ-ITEM
              ADD  1                    TO Z-TSQ-ITEM
CCC005*       EXEC CICS WRITEQ TS
CCC004*                 QUEUE  (Z-TSQ-NAME)
CCC004*                 FROM   (M2O)
CCC004*                 ITEM   (Z-TSQ-ITEM) REWRITE
CCC004*                 MAIN
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-49        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
170299*    END-IF

      *    * SCHREIBEN MAP 3 IN TS-QUEUE
170299*    IF I-BILD-MAX NOT < K-3
              MOVE K-3                  TO Z-TSQ-ITEM
              ADD  1                    TO Z-TSQ-ITEM
CCC005*       EXEC CICS WRITEQ TS
CCC004*                 QUEUE  (Z-TSQ-NAME)
CCC004*                 FROM   (M3O)
CCC004*                 ITEM   (Z-TSQ-ITEM) REWRITE
CCC004*                 MAIN
CCC004*                 RESP   (C-CICS-RC)
CCC003*       END-EXEC
              IF C-CICS-RC NOT = DFHRESP(NORMAL)
                 MOVE K-50        TO ERR-ORT-LFD
                 PERFORM ZR92-CICS-FEHLER
              END-IF
170299*    END-IF
           .
       R81-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * ZR5-PRUEF-TRACE-INPUT                                      *
      *------------------------------------------------------------*
      * INPUT : Z-FTASTE, ERTRANSI,                                *
      *                                                            *
      * OUTPUT: FT-RC, EVTL. GESETZTE TRACE-PARAMETER              *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - PRUEFEN, OB EINE EINGABE BZGL. TRACE GEMACHT WURDE     *
      *   - GGF. ENTSPR. BEARBEITUNG AUSFUEHREN                    *
      *                                                            *
      *------------------------------------------------------------*
       ZR5-PRUEF-TRACE-INPUT SECTION.
       ZR5-ANF.
           MOVE 'ZR5  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET FTASTE-OK                    TO TRUE

           EVALUATE Z-FTASTE

           WHEN DFHENTER
      *       ---------------------------------------------------- *
      *       TRANS + DATFRG PRUEFEN AUF TRACE-TRANSAKTION         *
      *       ZUR EINSTELLUNG VON TRACE-PARAMETERN                 *
      *       ( JEWEILS PRUEFEN, OB NUR IN 'TEST' ERLAUBT !)       *
      *       ---------------------------------------------------- *
              EVALUATE ERTRANSI IN MFI
              WHEN SPACE
                 CONTINUE

              WHEN K-TRANS-TRACE-AN
030395           SET INFO-DISPLAY-JA              TO TRUE
                 IF SYSTEM-TEST
                    SET TRACE-DISPLAY-JA          TO TRUE
                 END-IF
                 SET FTASTE-BILDAUSGABE-BILDLAST  TO TRUE

              WHEN K-TRANS-TRACE-AUS
030395           SET INFO-DISPLAY-NEIN            TO TRUE
                 IF SYSTEM-TEST
                    SET TRACE-DISPLAY-NEIN        TO TRUE
                 END-IF
                 SET FTASTE-BILDAUSGABE-BILDLAST  TO TRUE

              WHEN K-TRANS-TRACE-KURZ
                 IF SYSTEM-TEST
                    SET TRACELEN-KURZ             TO TRUE
                 END-IF
                 SET FTASTE-BILDAUSGABE-BILDLAST  TO TRUE

              WHEN K-TRANS-TRACE-LANG
                 IF SYSTEM-TEST
                    SET TRACELEN-LANG             TO TRUE
                 END-IF
                 SET FTASTE-BILDAUSGABE-BILDLAST  TO TRUE

              WHEN OTHER
                 CONTINUE
              END-EVALUATE
           END-EVALUATE
           .
       ZR5-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * UR1-OP-TAB-FUELLEN                                         *
      *------------------------------------------------------------*
      * INPUT : Z-OP-TAB-FUELLWERT                                 *
      * OUTPUT: WEITERER OP-EINTRAG IN OP-TABELLE                  *
      * VERARBEITUNG:                                              *
      *   - EINTRAG DES UEBERGEBENEN OP-CODES IN DIE OP-TABELLE    *
      *   - FALLS OP-TABELLE SCHON VOLL, AUFRUF PROG-FEHLER        *
      *   ACHTUNG!!!                                               *
      *     DIE OP-CODES WERDEN IN DER REIHENFOLGE DES AUFRUFS     *
      *     DIESES MODULS IN DIE TABELLE EINGETRAGEN !!!           *
      *------------------------------------------------------------*
       UR1-OP-TAB-FUELLEN SECTION.
       UR1-ANF.
           MOVE 'UR1  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET I-OP-FUELL                 TO K-1
           SEARCH CARD-OP-ZEILE
                  VARYING I-OP-FUELL
              AT END
                 MOVE K-51                TO ERR-ORT-LFD
                 MOVE K-8                 TO ERR-MELD-NR
                 PERFORM UR93-PROG-FEHLER
              WHEN CARD-OP(I-OP-FUELL) = SPACE
                 MOVE Z-OP-TAB-FUELLWERT  TO CARD-OP(I-OP-FUELL)
           END-SEARCH
           .
       UR1-EXIT.
           EXIT.

200197* >> GANZE SECTION NEU
      *------------------------------------------------------------*
       UR2-OP-TAB-INI SECTION.
      *------------------------------------------------------------*
      * INPUT : -                                                  *
      * OUTPUT: OP-TAB LEER                                        *
      * VERARBEITUNG:                                              *
      *   - INITIALISIEREN DER OP-TABELLE                          *
      *   - RÜCKSETZTEN DES CURSORS                                *
      *------------------------------------------------------------*
       UR2-ANF.
           MOVE 'UR2  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           INITIALIZE                 CARD-OP-TAB
           MOVE ZERO                  TO CARD-OP-IND
           .
       UR2-EXIT.
           EXIT.

      *------------------------------------------------------------*
      * UR4-ROLLBACK                                               *
      *------------------------------------------------------------*
      * INPUT : -                                                  *
      *                                                            *
      * OUTPUT: -                                                  *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - ROLLBACK OHNE PROGRAMMABBRUCH                          *
      *------------------------------------------------------------*
       UR4-ROLLBACK SECTION.
       UR4-ANF.
           MOVE 'UR4  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
CCC005*    EXEC CICS SYNCPOINT
CCC004*              ROLLBACK
CCC003*    END-EXEC
           .
       UR4-EXIT.
           EXIT.
170299*>>>>>
      *------------------------------------------------------------*
      * UR5-EURO-SWITCH-PRUEFEN                                    *
      *------------------------------------------------------------*
      * INPUT : K-PGM-NAME                                         *
      *                                                            *
      * OUTPUT: CARD-EURO-SWITCH                                   *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - PRüFEN, OB PROGRAMM MIT EURO-SWITCH ARBEITET           *
      *------------------------------------------------------------*
       UR5-EURO-SWITCH-PRUEFEN SECTION.
       UR5-ANF.
           MOVE 'UR5  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      * PRüFEN
      *
           SET  PI-UPAT0023-DRAT0291 TO TRUE
           MOVE K-PGM-NAME           TO PI-DRAT0291-PGNAME

CCCPY1*    CALL K-UPAT0023 USING BY REFERENCE PI-UPAT0023-FUNKTION
CCC004*                                       PI-DRAT0291
CCC004*                                       PO-DRAT0291
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

           IF ERR-RC-SCHWERER-FEHLER
              PERFORM UR99-SCHWERER-FEHLER
           END-IF

           EVALUATE TRUE

           WHEN PO-DRAT0291-RC-AKTIV

              SET CARD-EURO-SWITCH-URSPRUNG TO TRUE

           WHEN OTHER

                 CONTINUE
           END-EVALUATE
           .
       UR5-EXIT.
           EXIT.

170299*<<<<<
170299*>>>>>
      *------------------------------------------------------------*
      * UR6-EURO-UMRECHNEN                                  *
      *------------------------------------------------------------*
      * INPUT : Z-EURO-SWITCH-EINBER, Z-EURO-SWITCH-AKTION         *
      *                                                            *
      * OUTPUT: Z-EURO-SWITCH-EINBER MIT UMGERECHNETEN BETRäGEN    *
      *         CARD-EURO-SWITCH                                   *
      * VERARBEITUNG:                                              *
      *   - UMRECHNEN BETRäGE AUF BILDSCHRIRM IN BETRäGE           *
      *------------------------------------------------------------*
       UR6-EURO-UMRECHNEN SECTION.
       UR6-ANF.
           MOVE 'UR6  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           SET  PI-UPAT0023-DRAT0283 TO TRUE
           MOVE K-PGM-NAME           TO PI-DRAT0283-PROGRAMM
           MOVE S-BILDNR             TO PI-DRAT0283-BILDNR
           MOVE Z-EURO-SWITCH-EINBER TO PI-DRAT0283-MASKE

           EVALUATE TRUE
           WHEN Z-EURO-SWITCH-ALLES
                SET PI-DRAT0283-ALLES TO TRUE
           WHEN Z-EURO-SWITCH-NUR-TEXTE
                SET PI-DRAT0283-NUR-TEXTE TO TRUE
           WHEN Z-EURO-SWITCH-NUR-BETRAEGE
                SET PI-DRAT0283-NUR-BETRAEGE TO TRUE
           WHEN OTHER
MHTODO*         HIER WäR EIN ABBRUCH ANGEBRACHT
                CONTINUE
           END-EVALUATE

CCCPY1*    CALL K-UPAT0023 USING BY REFERENCE PI-UPAT0023-FUNKTION
CCC004*                                       PI-DRAT0283
CCC004*                                       PO-DRAT0283
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                          BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL

           IF ERR-RC-SCHWERER-FEHLER
              PERFORM UR99-SCHWERER-FEHLER
           END-IF

           EVALUATE TRUE

           WHEN PO-DRAT0283-RC-OK

                MOVE PO-DRAT0283-MASKE TO Z-EURO-SWITCH-EINBER

           WHEN PO-DRAT0283-RC-NOK
      *    UMRECHNUNG FEHLERHAFT
                CONTINUE
           WHEN OTHER
                MOVE K-52                TO ERR-ORT-LFD
                PERFORM UR93-PROG-FEHLER
           END-EVALUATE

           .
       UR6-EXIT.
           EXIT.
170299*<<<<<
170299*>>>>>
      *------------------------------------------------------------*
      * UR7-EURO-MASKE-SICHERN                                     *
      *------------------------------------------------------------*
      * INPUT : Z-EURO-SWITCH-EINBER                               *
      *                                                            *
      * OUTPUT: TSQ MIT MASKE                                      *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - SICHERN MASKE IN TSQ                                   *
      *------------------------------------------------------------*
       UR7-EURO-MASKE-SICHERN SECTION.
       UR7-ANF.
           MOVE 'UR7  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           MOVE K-5 TO Z-TSQ-ITEM
CCC005*    EXEC CICS WRITEQ TS
CCC004*              QUEUE  (Z-TSQ-NAME)
CCC004*              FROM   (Z-EURO-SWITCH-EINBER)
CCC004*              ITEM   (Z-TSQ-ITEM) REWRITE
CCC004*              MAIN
CCC004*              RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-53        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

           .
       UR7-EXIT.
           EXIT.
170299*<<<<<
170299*>>>>>
      *------------------------------------------------------------*
      * UR7F-EURO-FUSS-SICHERN                                     *
      *------------------------------------------------------------*
      * INPUT : MFO                                                *
      *                                                            *
      * OUTPUT: TSQ MIT FUSS                                       *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - SICHERN FUSS IN  TSQ                                   *
      *------------------------------------------------------------*
       UR7F-EURO-FUSS-SICHERN SECTION.
       UR7F-ANF.
           MOVE 'UR7F ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           MOVE K-6 TO Z-TSQ-ITEM
CCC005*    EXEC CICS WRITEQ TS
CCC004*              QUEUE  (Z-TSQ-NAME)
CCC004*              FROM   (MFO)
CCC004*              ITEM   (Z-TSQ-ITEM) REWRITE
CCC004*              MAIN
CCC004*              RESP   (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-56        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

           .
       UR7F-EXIT.
           EXIT.
170299*<<<<<
170299*>>>>>
      *------------------------------------------------------------*
      * UR8-EURO-MASKE-ENTSICHERN                                  *
      *------------------------------------------------------------*
      * INPUT : Z-EURO-SWITCH-TSQ-ITEM                             *
      *                                                            *
      * OUTPUT: Z-EURO-SWITCH-EINBER                               *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - ENTSICHERN MASKE IN TSQ                                *
      *------------------------------------------------------------*
       UR8-EURO-MASKE-ENTSICHERN SECTION.
       UR8-ANF.
           MOVE 'UR8  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           MOVE K-5 TO Z-TSQ-ITEM
CCC005*    EXEC CICS READQ TS
CCC004*              QUEUE (Z-TSQ-NAME)
CCC004*              INTO  (Z-EURO-SWITCH-EINBER)
CCC004*              ITEM  (Z-TSQ-ITEM)
CCC004*              RESP  (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-54        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

           .
       UR8-EXIT.
           EXIT.
170299*<<<<<
170299*>>>>>
      *------------------------------------------------------------*
      * UR8F-EURO-FUSS-ENTSICHERN                                  *
      *------------------------------------------------------------*
      * INPUT :                                                    *
      *                                                            *
      * OUTPUT: MFO                                                *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - ENTSICHERN MASKE IN TSQ                                *
      *------------------------------------------------------------*
       UR8F-EURO-FUSS-ENTSICHERN SECTION.
       UR8F-ANF.
           MOVE 'UR8F ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
           MOVE K-6 TO Z-TSQ-ITEM
CCC005*    EXEC CICS READQ TS
CCC004*              QUEUE (Z-TSQ-NAME)
CCC004*              INTO  (MFO)
CCC004*              ITEM  (Z-TSQ-ITEM)
CCC004*              RESP  (C-CICS-RC)
CCC003*    END-EXEC
           IF C-CICS-RC NOT = DFHRESP(NORMAL)
              MOVE K-57        TO ERR-ORT-LFD
              PERFORM ZR92-CICS-FEHLER
           END-IF

           .
       UR8-EXIT.
           EXIT.
170299*<<<<<
090301*>>>>>
      *------------------------------------------------------------*
      * UR9-CTV-CADU-OB-ERMITTELN                                  *
      *------------------------------------------------------------*
      * INPUT : CARU-MAP-LAST                                      *
      *                                                            *
      * OUTPUT: CADU-OB                                            *
      *                                                            *
      * VERARBEITUNG:                                              *
      *   - ERMITTELN ORDNUNGSBEGRIFF AUS AKTUELLER MAP            *
      *------------------------------------------------------------*
       UR9-CTV-CADU-OB-ERMITTELN SECTION.
       UR9-ANF.
           MOVE 'UR9  ' TO ERR-ORT-SEC
           PERFORM ZR1-TRACE-KURZ
      *
      * ERMITTELN
MHTEST*    ERMITTELN ÜBER KTY! INFO: MAP, ANFANG OB, M?XI
MHTEST     EVALUATE TRUE
           WHEN CARU-MAP-LAST = 'BVB0005'

                MOVE M1XI            TO Z-EINBER
                MOVE Z-EINBER(16:07) TO CADU-OB
100701*>>>>>
      *    WHEN CARU-MAP-LAST = 'BVB0015'
      *
      *         MOVE M1XI            TO Z-EINBER
      *         MOVE Z-EINBER(20:07) TO CADU-OB
      *
      *    WHEN CARU-MAP-LAST = 'BVB0016'
      *
      *         MOVE M2XI            TO Z-EINBER
      *         MOVE Z-EINBER(16:07) TO CADU-OB
100701*<<<<<
           WHEN CARU-MAP-LAST = 'BVB0017'

                MOVE M1XI            TO Z-EINBER
                MOVE Z-EINBER(16:07) TO CADU-OB

           WHEN CARU-MAP-LAST = 'BVB0020'

                MOVE M1XI            TO Z-EINBER
                MOVE Z-EINBER(16:07) TO CADU-OB

           WHEN OTHER

                CONTINUE

           END-EVALUATE
           .
       UR9-EXIT.
           EXIT.

170299*<<<<<

      **************************************************************
      * COPY-STRECKEN                                              *
      **************************************************************
      * ZR1-TRACE-KURZ SECTION
           COPY DPAT0022.

      *
      ******* ENDE COPY-MEMBER DPAT0001 (DIALOG-RAHMENSTEUERUNG) ******
CCC009*    EJECT
