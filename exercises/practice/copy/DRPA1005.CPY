      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1005.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1005.CPv  $
      *
      *   Rev 2.2   Jul 16 2015 15:08:40   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.1   25 Apr 2000 10:17:54   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   May 27 1999 11:58:46   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   08 Jul 1998 08:51:32   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1005                                      *
      *    --------------------------------------------------------   *
      *    PAMS-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.07.98!1.1 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      * APA00012!31.07.98!1.2 ! Prefix aufgenommen        ! Jäger    *
      *         !        !    !                           !          *
      * APA00084!14.03.00!2.1 ! STUFENNUMMER GEÄNDERT     ! KRUFF    *
      * APA01932!13.04.15!2.2 ! UMSATZSTEUER ERGÄNZT      ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *                                                              *
      ****************************************************************
 
       35 PAMS-STRUKTUR-DATEN.
          40 PEBD-ST-NR                  PIC S9(009) BINARY.
          40 PEBD-DIFF-PROV-STF          PIC S9(004) BINARY.
          40 KNR-EMP-TYP                 PIC S9(004) BINARY.
          40 PDPE-BUCH-RELEVANT          PIC  X.
          40 PDPE-EINH                   PIC S9(6)V99    PACKED-DECIMAL.
          40 PDPE-NT-PBTRG-BRWG          PIC S9(009)V99  PACKED-DECIMAL.
          40 PDPE-PROV-RCK-BRWG          PIC S9(009)V99  PACKED-DECIMAL.
          40 KNR-AUSZ-WAEHRUNG           PIC S9(004)     BINARY.
          40 PDPE-NT-PBTRG-AZWG          PIC S9(009)V99  PACKED-DECIMAL.
          40 PDPE-PROV-RCK-AZWG          PIC S9(009)V99  PACKED-DECIMAL.
130415    40 PDPE-UST-PBTRG-BRWG         PIC S9(009)V99  PACKED-DECIMAL.
130415    40 PDPE-UST-RCK-BRWG           PIC S9(009)V99  PACKED-DECIMAL.
130415    40 PDPE-UST-PBTRG-AZWG         PIC S9(009)V99  PACKED-DECIMAL.
130415    40 PDPE-UST-RCK-AZWG           PIC S9(009)V99  PACKED-DECIMAL.
130415    40 PDPE-KZ-UST-BER             PIC  X.
130415    40 PABR-ABRECH-NR              PIC  X(015).
      ******* ENDE COPY-MEMBER *************************************

