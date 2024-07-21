      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0960.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0960.CPv  $
      *
      *   Rev 2.1   20 Sep 2000 13:34:14   T13AE34
      *AAN00264-Umbenennen PEWP-ERHG-BEITRAG
      *in PEWP-BEWERT-BETRAG
      *
      *   Rev 2.0   Feb 27 1999 14:29:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 15:15:32   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.0   01 Apr 1998 14:18:34   P13SP99
      *Initial revision.
      *
      *****************************************************************
      *    COPY-MEMBER: DRPA0960                                      *
      *    --------------------------------------------------------   *
      *    PEW-PRODUKT-DATEN                                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    19.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * PEW-REL.!19.05.98! 01 ! NEU                       ! WOLFF    *
      * 2.0     !        !    !                           !          *
      * AAN00264!07.07.00!2.1 ! PEWP-BEWERT-BETRAG ->      !HEIDER    *
      *         !        !    ! PEWP-BEWERT-BETRAG        !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN DER PEW-PRODUKT-DATEN       *
      *                                                              *
      ****************************************************************
      *
             40 PEW-PRODUKT.
                45 PROD-PROD-SCHL                   PIC S9(004) BINARY.
                45 PEWP-TARIF                       PIC  X(012).
                45 PEWP-LAUFZEIT                    PIC S9(004) BINARY.
                45 PEWP-ABLAUF                      PIC  X(010).
                45 PRKO-KOND-NR-GSP                 PIC S9(004) BINARY.
                45 PRKO-KOND-NR-GRP                 PIC S9(004) BINARY.
                45 PRKO-KOND-NR-EINH                PIC S9(004) BINARY.
                45 PEWP-BEITRAG                     PIC S9(009)V99
                                                        PACKED-DECIMAL.
                45 PEWP-SUMME                       PIC S9(009)V99
                                                        PACKED-DECIMAL.
                45 PEWP-AVERM                       PIC S9(009) BINARY.
                45 PEWP-BETR                        PIC S9(009)V99
                                                        PACKED-DECIMAL.
                45 PEWP-BETR-AVERM                  PIC S9(009)V99
                                                        PACKED-DECIMAL.
                45 PEWP-EINH                        PIC S9(006)V99
                                                        PACKED-DECIMAL.
                45 PEWP-EINH-AVERM                  PIC S9(006)V99
                                                        PACKED-DECIMAL.
                45 PEWP-RMELD-DAT                   PIC  X(010).
                45 PEWP-BEWERT-BETRAG               PIC S9(009)V99
                                                        PACKED-DECIMAL.
                45 KNR-WAEHRUNG                     PIC S9(004) BINARY.
                45 PEWP-BTO-BEITRAG                 PIC S9(009)V99
                                                        PACKED-DECIMAL.
                45 PEWP-VSTAT                       PIC  X(004).
                45 PEWP-VSTAT-UNR                   PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
