      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAT0070.CPv  $
      *
      *   Rev 2.1   Feb 10 2017 12:26:24   t13ac20
      *AAT00056: Auslagerung DB2-Befehle für Uprorahmen
      *
      *   Rev 2.0   Feb 09 2017 14:29:04   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *****************************************************************
      *    COPY-MEMBER: DPAT0070                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: DB2-BEFEHLE DURCHFÜHREN FÜR UPRO              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SWO                                              *
      *    DATUM:    09.02.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAT00056!09.02.17!2.1 ! NEU                       ! SWO       *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      * ROUTINEN: - XR02-ERM-TIMESTAMP
      *
      *---------------------------------------------------------------*
       XR02-ERM-TIMESTAMP SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT: BR-Z-LAUFDATUM-TS-DB2
      * VERARBEITUNG:
      *         -
      *---------------------------------------------------------------*
           MOVE 'XR02-'                  TO ERR-ORT-SEC
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
              GO TO XST-EXIT
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
              PERFORM XR93-UPRO-PROG-FEHLER
              GO TO XST-EXIT
           END-EVALUATE
           .
       XR02-EXIT.
           EXIT.

