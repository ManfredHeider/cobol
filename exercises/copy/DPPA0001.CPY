      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPPA0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPPA0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Apr 11 1997 18:19:38   T13AE19
      *No change.
      *
      *   Rev 1.1   Mar 06 1997 14:12:52   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:16:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPPA0001                                      *
      *    --------------------------------------------------------   *
      *    DAISY: MODULT ZUR ID-ERMITTLUNG (ERW. TIMESTAMP)           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    09.09.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !09.09.96!    ! Neuerstellung             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *  - DIESES COPY ENTHÄLT DIE BEFEHLE ZUR ERMITTLUNG EINER
      *    WARTUNGSFREIEN ID AUS EINER COBOL DATUM/ZEIT KOMBINATION.
      *  - DIES MODUL SOLL IM BATCH AUS PERFORMANCE-GRÜNDEN NUR 1 MAL
      *    AUFGERUFEN WERDEN; ANSCHLIESSEND IST AUF DIE VOLLSTÄNDIGE
      *    ID JEWEILS DER WERT 10 ZU ADDIEREN.
      *  - IM ONLINE WIRD DIESES MODUL JEDESMAL AUFGERUFEN; DIE LETZTE
      *    STELLE WIRD IM ONLINE JEWEILS MIT 5 BELEGT, SOFERN GLEICHE
      *    SCHLÜSSEL ENTSTEHEN, IST DIES DANN INTERN ZU LÖSEN
      *  - DAS JAHR IM SCHLÜSSEL WIRD GRUNDSÄTZLICH AUF EINEN WERT
      *    GESETZT, DER AUCH NACH DER JAHRTAUSENDWENDE BESTAND HAT.
      *  - ZUGEHÖRIGE COPYSTRECKE: DWPA0001
      *
      ****************************************************************
       DPPA0001-ID-VERGABE SECTION.
      *
           ACCEPT DWPA0001-COBOL-DAY   FROM DAY
           IF DWPA0001-COBOL-DAY-JJ > K-90
              COMPUTE DWPA0001-COBOL-DAY-JJ =
                      DWPA0001-COBOL-DAY-JJ - K-90
              END-COMPUTE
           ELSE
              ADD 10 TO DWPA0001-COBOL-DAY-JJ
           END-IF
      *
           ACCEPT DWPA0001-COBOL-TIME  FROM TIME
           IF STAT-BATCH
              MOVE ZERO                TO   DWPA0001-ID-UNR
           ELSE
              MOVE K-5                 TO   DWPA0001-ID-UNR
           END-IF
           .
      *
       DPPA0001-EXIT.
           EXIT.
      *
      ******* ENDE COPY-MEMBER **************************************
