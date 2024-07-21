      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0085.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0085.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 29 1995 15:41:36   T13AE26
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0085, TABELLE DER KNR-GRUPPEN             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MOLL                                             *
      *    DATUM:    11.12.1995                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * AFVB0251!10.12.95!    ! NEU                       ! Moll      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *   BESCHREIBUNG DER TABELLE:
      *
      *   Es wird festgelegt, welche Mitarbeiter-Stufe(n) (MS-Nr)
      *   welche(n) Gruppe(n) von Zusätzen zugeordnet ist(wird).
      *
      *   Die Gruppen werden durch ihre KNR-Nummer
      *   (zur KTY-Nr 63 / KTY-GR-TYP) charakterisiert.
      *
      *
      ***************************************************************
      * -------------------------------------------------------------
      * MA-Stufe <-> Gruppe
      * -------------------------------------------------------------
       01 ZK-ZUSATZ-VALUES.
          05 ZK-ZUSATZ-TAB-VALUE.
      *                  ** AL-PROBE **
             10 ZK-ZUSATZ.
                15                      PIC  S9(04)   COMP VALUE 161.
                15                      PIC  S9(04)   COMP VALUE 001.

       01 ZK-ZUSA-TABELLE REDEFINES ZK-ZUSATZ-VALUES.
          05 ZK-ZUSA-TAB OCCURS 1 INDEXED BY ZK-ZUSA-TAB-IND.
             10 ZK-ZUSA.
                15 ZK-ZUSA-STF          PIC  S9(04)   COMP.
                15 ZK-ZUSA-KGR          PIC  S9(04)   COMP.
      ******** ENDE COPY-MEMBER *************************************
