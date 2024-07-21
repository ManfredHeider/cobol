      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0069.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DPAT0069.CPv  $
      *
      *   Rev 2.3   Mar 12 2020 11:34:54   t13ae34
      *AAT00081: Abbruchroutine ILBOABN0 durch CEE3ABD ersetzen
      *
      *   Rev 2.2   Feb 25 2020 11:42:56   t13ae34
      *FAT00051: Logischer Fehler im PMAT0000 - potentielle Endloss
      *chleife - Anpassung DPAT0069.cpy
      *
      *   Rev 2.1   Jan 24 2017 08:26:58   t13ac20
      *AAT00056: Auslagerung DB2-Befehle
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Mar 1998 17:03:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *****************************************************************
      *    COPY-MEMBER: DPAT0069                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: DB2-BEFEHLE DURCHFÜHREN                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SWO                                              *
      *    DATUM:    23.01.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAT00056!23.01.17! NEU                            ! SWO       *
      * FAT00051!25.02.20! IN SECTION BR04-ROLLBACK       ! HEIDER    *
      *         !        ! ERSETZEN AUFRUF U01 DURCH BR05 !           *
      * AAT00081!11.03.20! UMBENENNUNG BR05               ! HEIDER    *
      *         !        !                                !           *
      *                                                               *
      *****************************************************************
      *
      * ROUTINEN: - BR02-ERM-TIMESTAMP
      *           - BR03-COMMIT
      *           - BR04-ROLLBACK
      *
      *---------------------------------------------------------------*
       BR02-ERM-TIMESTAMP SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT: BR-Z-LAUFDATUM-TS-DB2
      * VERARBEITUNG:
      *         - DPAT0614-UPAT0042 MIT AUFRUF U01 BEI ABBRUCH
      *---------------------------------------------------------------*
           MOVE 'BR02-'                  TO ERR-ORT-SEC
      *
           SET  PI-UPAT0042-DRAT0614-DB2-TIME TO TRUE
           INITIALIZE PO-DRAT0614
      *
CCCPY1*    CALL K-UPAT0042 USING BY REFERENCE PI-UPAT0042
CCC004*                                       PI-DRAT0614
CCC004*                                       PO-DRAT0614
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              MOVE K-1                   TO ERR-ORT-LFD
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAT0614-RC-OK
              MOVE PO-DRAT0614-TIMESTAMP TO BR-Z-LAUFDATUM-TS-DB2
           WHEN OTHER
              MOVE 'FEHLER AUFRUF UPAT0042/DRAT0614'
                                         TO ERR-VAR-ZEILE03
              MOVE 'UNBEKANNTER RC: '    TO ERR-VAR-ZEILE04
              MOVE PO-DRAT0614-RC        TO ERR-VAR-ZEILE04(17:3)
              MOVE K-2                   TO ERR-ORT-LFD
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-EVALUATE
           .
       BR02-EXIT.
           EXIT.
      *
      *---------------------------------------------------------------*
       BR03-COMMIT SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT: -
      * VERARBEITUNG:
      *         - DPAT0615-UPAT0042 MIT AUFRUF U01 BEI ABBRUCH
      *---------------------------------------------------------------*
           MOVE 'BR03-'                  TO ERR-ORT-SEC
      *
           SET  PI-UPAT0042-DRAT0615-COMMIT TO TRUE
           INITIALIZE PO-DRAT0615
      *
CCCPY1*    CALL K-UPAT0042 USING BY REFERENCE PI-UPAT0042
CCC004*                                       PI-DRAT0615
CCC004*                                       PO-DRAT0615
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              MOVE K-1                   TO ERR-ORT-LFD
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAT0615-RC-OK
              CONTINUE
           WHEN OTHER
              MOVE 'FEHLER AUFRUF UPAT0042/DRAT0615'
                                         TO ERR-VAR-ZEILE03
              MOVE 'UNBEKANNTER RC: '    TO ERR-VAR-ZEILE04
              MOVE PO-DRAT0615-RC        TO ERR-VAR-ZEILE04(17:3)
              MOVE K-2                   TO ERR-ORT-LFD
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-EVALUATE
           .
       BR03-EXIT.
           EXIT.
      *
      *---------------------------------------------------------------*
       BR04-ROLLBACK SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT: -
      * VERARBEITUNG:
      *         - DPAT0616-UPAT0042 MIT AUFRUF BR05 BEI ABBRUCH
      *---------------------------------------------------------------*
           MOVE 'BR04-'                  TO ERR-ORT-SEC
      *
           SET  PI-UPAT0042-DRAT0616-ROLLBACK TO TRUE
           INITIALIZE PO-DRAT0616
      *
CCCPY1*    CALL K-UPAT0042 USING BY REFERENCE PI-UPAT0042
CCC004*                                       PI-DRAT0616
CCC004*                                       PO-DRAT0616
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              MOVE K-1                   TO ERR-ORT-LFD
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM BR05-DISPLAY-ABBRUCH
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAT0616-RC-OK
              CONTINUE
           WHEN OTHER
              MOVE 'FEHLER AUFRUF UPAT0042/DRAT0616'
                                         TO ERR-VAR-ZEILE03
              MOVE 'UNBEKANNTER RC: '    TO ERR-VAR-ZEILE04
              MOVE PO-DRAT0616-RC        TO ERR-VAR-ZEILE04(17:3)
              MOVE K-2                   TO ERR-ORT-LFD
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM BR05-DISPLAY-ABBRUCH
           END-EVALUATE
           .
       BR04-EXIT.
           EXIT.
      *
      *---------------------------------------------------------------*
       BR05-DISPLAY-ABBRUCH SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT: -
      * VERARBEITUNG:
      *          DISPLAY
      * HINWEIS: AUFRUF U01-ABBRUCH WURDE ERSETZT DURCH BR05-ABBRUCH
      *          UM EINE POTENTIELLE ENDLOSSCHLEIFE ZU VERMEIDEN
      *---------------------------------------------------------------*
           MOVE 'BR05-'                  TO ERR-ORT-SEC
      *
           DISPLAY ' '
           DISPLAY '*** ACHTUNG ABBRUCH IM ABBRUCHHANDLING !  *****'
           DISPLAY ' '
           .
       BR05-EXIT.
           EXIT.
      *
      *------------------------ ENDE DPAT0069 ------------------------*
      *

