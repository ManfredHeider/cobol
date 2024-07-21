      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0485.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0485.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:07:38   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.0   May 26 1999 16:59:34   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:42:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0485  Tabellen-Element für PDPE           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    26.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !26.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * apa00012!18.08.98! 02 ! PDPT-ID und Positionsnumm.! Parent   *
      *         !        !    ! entfernt ( Denormalisier) !          *
      * APA01932!03.06.15! 2.1! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
180898*          35 PDPE-KEY.
180898*             40 PDPT-ID          PIC S9(015)     PACKED-DECIMAL.
180898*             40 POSITIONS-NR     PIC S9(004)     BINARY.
180898           35 ST-NR               PIC S9(009)     BINARY.
                 35 NT-PROV-BTRG        PIC S9(009)V99  PACKED-DECIMAL.
                 35 PROV-RCK            PIC S9(009)V99  PACKED-DECIMAL.
                 35 EINH                PIC S9(006)V99  PACKED-DECIMAL.
                 35 NT-PROV-BTRG-WHG    PIC S9(009)V99  PACKED-DECIMAL.
                 35 PROV-RCK-WHG        PIC S9(009)V99  PACKED-DECIMAL.
                 35 KTY-WAEHRUNG-AUSZ   PIC S9(004)     BINARY.
                 35 KNR-WAEHRUNG-AUSZ   PIC S9(004)     BINARY.
                 35 BUCH-RELEVANT       PIC  X(001).
030615           35 UST-BTRG-BRWG       PIC S9(009)V9(2) PACKED-DECIMAL.
030615           35 UST-RCK-BRWG        PIC S9(009)V9(2) PACKED-DECIMAL.
030615           35 UST-BTRG-AZWG       PIC S9(009)V9(2) PACKED-DECIMAL.
030615           35 UST-RCK-AZWG        PIC S9(009)V9(2) PACKED-DECIMAL.
030615           35 KZ-UST-BER          PIC  X(001).

