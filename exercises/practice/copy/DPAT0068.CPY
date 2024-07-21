      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAT0068.CPv  $
      *
      *   Rev 2.1   Jan 13 2017 15:29:50   t13ac20
      *AAT00056: Ermittlung Datum, Uhrzeit
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Mar 1998 17:03:36   P13SP99
      *Initial revision.
      *
      *****************************************************************
      *    COPY-MEMBER: DPAT0068                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: LAUFDATUM UND -UHRZEIT ERMITTELN,             *
      *                 SOWIE GENERIERUNGSDATUM (UMWANDLUNGSDATUM)    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SWO                                              *
      *    DATUM:    13.01.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAT00056!13.01.17!2.1 ! NEU                       ! SWO       *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      * ROUTINEN: - BR01-ERM-LAUFDATUM
      *
      *---------------------------------------------------------------*
       BR01-ERM-LAUFDATUM SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT: -
      * VERARBEITUNG:
      *         -
      *---------------------------------------------------------------*
           MOVE 'BR01-'                  TO ERR-ORT-SEC
      *
           MOVE FUNCTION CURRENT-DATE(1:8) TO BR-Z-LAUFDATUM-ISO1
           MOVE FUNCTION CURRENT-DATE(9:)  TO BR-Z-LAUFZEIT-B
           MOVE WHEN-COMPILED              TO BR-Z-GENDATUM-B
           IF BR-Z-GD-JHR-B > 88
              MOVE 19 TO BR-Z-GD-JAHR-A(1:2)
           ELSE
              MOVE 20 TO BR-Z-GD-JAHR-A(1:2)
           END-IF
           MOVE BR-Z-LD-TAG-ISO1  TO BR-Z-LD-TAG-EUR
           MOVE BR-Z-LD-MON-ISO1  TO BR-Z-LD-MON-EUR
           MOVE BR-Z-LD-JAHR-ISO1 TO BR-Z-LD-JAHR-EUR
           MOVE BR-Z-LZ-STD-B     TO BR-Z-LZ-STD-A
           MOVE BR-Z-LZ-MIN-B     TO BR-Z-LZ-MIN-A
           MOVE BR-Z-LZ-SEK-B     TO BR-Z-LZ-SEK-A
           MOVE BR-Z-LZ-HSEK-B    TO BR-Z-LZ-HSEK-A
           MOVE BR-Z-LD-TAG-ISO1  TO BR-Z-LD-TAG-ISO2
           MOVE BR-Z-LD-MON-ISO1  TO BR-Z-LD-MON-ISO2
           MOVE BR-Z-LD-JAHR-ISO1 TO BR-Z-LD-JAHR-ISO2
           MOVE BR-Z-LD-JAHR-ISO1 TO BR-Z-LD-JAHR-TS
           MOVE BR-Z-LD-MON-ISO1  TO BR-Z-LD-MON-TS
           MOVE BR-Z-LD-TAG-ISO1  TO BR-Z-LD-TAG-TS
           MOVE BR-Z-LZ-STD-B     TO BR-Z-LD-STD-TS
           MOVE BR-Z-LZ-MIN-B     TO BR-Z-LD-MIN-TS
           MOVE BR-Z-LZ-SEK-B     TO BR-Z-LD-SEK-TS
           MOVE BR-Z-LZ-HSEK-B    TO BR-Z-LD-HSEK-TS
           MOVE BR-Z-GD-TAG-B     TO BR-Z-GD-TAG-A
           MOVE BR-Z-GD-MON-B     TO BR-Z-GD-MON-A
           MOVE BR-Z-GD-JHR-B     TO BR-Z-GD-JAHR-A(3:2)
           MOVE BR-Z-GD-STD-B     TO BR-Z-GZ-STD-A
           MOVE BR-Z-GD-MIN-B     TO BR-Z-GZ-MIN-A
           MOVE BR-Z-GD-SEK-B     TO BR-Z-GZ-SEK-A
           .
       BR01-EXIT.
           EXIT.

