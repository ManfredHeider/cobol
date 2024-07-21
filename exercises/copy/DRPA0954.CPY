      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0954.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0954.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 15:13:36   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.0   01 Apr 1998 14:18:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0954                                      *
      *    --------------------------------------------------------   *
      *    PEW-ANTRAG AUS ANTRAGS-TDS                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    14.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * PEW-REL.!14.05.98! 01 ! NEU                       ! WOLFF    *
      * 2.0     !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE AUS DEN ANTRAGS-TDS-DATEN         *
      *    ERSTELLTEN DATEN FÜR PEW-ANTRAG.                          *
      *                                                              *
      ****************************************************************
      *
             30 PEW-ANTRAG-AUS-TDS.
                35 ANTRAG-DATEN.
                   40 GES                           PIC S9(004) BINARY.
                   40 PIN                           PIC S9(009) BINARY.
                   40 VNR                           PIC  X(015).
                   40 VOR-VNR-KZ                    PIC  X(001).
                   40 KUNDE-NAME                    PIC  X(025).
                   40 KUNDE-VNAME                   PIC  X(020).
                   40 KNR-WAEHRUNG                  PIC S9(004) BINARY.
                   40 KNR-ZAHLWEISE                 PIC S9(004) BINARY.
                   40 DIR                           PIC S9(004) BINARY.
                   40 BEGINN                        PIC  X(010).
                   40 AN-AUFN-DAT                   PIC  X(010).
                   40 STRUK-DAT                     PIC  X(010).
                   40 AN-ARB-DAT                    PIC  X(010).
                   40 PVST-VSTAT                    PIC  X(004).
                   40 PVST-VSTAT-UNR                PIC S9(004) BINARY.
                   40 STRUK-MAN-AEND-KEZ            PIC  X(001).
                35 PRODUKT-TABELLE.
                   40 PRODUKT-ZEILE                 OCCURS 15.
                      45 PROD-SCHL                  PIC S9(004) BINARY.
                      45 TARIF                      PIC  X(012).
                      45 LAUFZEIT                   PIC S9(004) BINARY.
                      45 ABLAUF                     PIC  X(010).
                      45 BEITRAG                    PIC S9(009)V99
                                                        PACKED-DECIMAL.
                      45 SUMME                      PIC S9(009)V99
                                                        PACKED-DECIMAL.
                      45 BTO-BEITRAG                PIC S9(009)V99
                                                        PACKED-DECIMAL.
                      45 KNR-WAEHRUNG               PIC S9(004) BINARY.
                      45 EFF-BEITRAG                PIC S9(009)V99
                                                        PACKED-DECIMAL.
                35 STRUKTUR-TABELLE.
                   40 STRUKTUR-ZEILE                OCCURS 20.
                      45 MANR                       PIC S9(009) BINARY.
                      45 PRZ                        PIC  X(001).
                      45 EMP-STUFE                  PIC  X(004).
                      45 DIFF-PROV-STUFE            PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
