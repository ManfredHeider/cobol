      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0909.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\DRPA0909.CPv  $
      *
      *   Rev 2.4   22 Nov 2000 17:33:04   T13EX44
      *REDFINES für LFDNR
      *
      *   Rev 2.3   07 Nov 2000 09:47:36   T13EX44
      *Erweiterung der Tabelle
      *
      *   Rev 2.2   Jun 24 1999 17:55:10   T13EX21
      *APA00012 - PVCS-Kopf korrigiert
      *
      *   Rev 2.1   Jun 22 1999 16:41:26   T13EX44
      *Korrektur Kopf (Gross- nach
      *Kleinschreibung)
      *
      *   Rev 1.0   10 Mar 1998 15:35:28   P13SP99
      *Initial Revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0909  TABELLE MIT AHGSEG-DATEN           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OPRITZ                                          *
      *    DATUM:    11.03.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.03.98! 01 ! NEUERSTELLUNG             ! OPRITZ   *
      * APA00111!17.10.00!2.03! ERWEITERUNG AUF 1000      ! SCHEIDEL *
      *         !        !    ! ZEILEN                    !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE ZWISCHENSPEICHER-TABELLE          *
      *    FÜR DIE GELESENEN DATEN AUS AHGSEG (OPAL) IN UPPA0091     *
      *                                                              *
      ****************************************************************
      *                                                              *
      * A C H T U N G :                                              *
      *    BEI AENDERUNGEN AN DER TABELLENZEILE HINSICHTLICH DER     *
      *    GESAMTLAENGE  M U S S  AUCH DIE VARIABLE                  *
      *                                                              *
      *                   'P2AHGSEG-ZEILE-ZW'                        *
      *                                                              *
      *    ANGEPASST WERDEN!!!!!!!!!!                                *
      *                                                              *
      *    DIESE STEHT AM ENDE DIESES COPIES                         *
      *                                                              *
      *                                                              *
      ****************************************************************
           05 MAX-USED-P2              PIC S9(004)       BINARY.
           05 P2ABHSEG-TABELLE.
            10 P2AHGSEG-ZEILE          OCCURS 1000 TIMES
                                       INDEXED BY IX-P2.
             15 P2-ZUSATZDATEN.
              20 P2-FUER-DAISY.
               25 PRINTE               PIC S9(015)       PACKED-DECIMAL.
               25 PRINTE-VERSION       PIC S9(004)       BINARY.
               25 PVPT-ID              PIC S9(015)       PACKED-DECIMAL.
               25 PVPT-VERSION         PIC S9(004)       BINARY.
               25 PMBE-ID              PIC S9(015)       PACKED-DECIMAL.
               25 PMBE-VERSION         PIC S9(004)       BINARY.
               25 PMPR-ID              PIC S9(015)       PACKED-DECIMAL.
               25 PMPR-VERSION         PIC S9(004)       BINARY.
               25 PMPT-ID              PIC S9(015)       PACKED-DECIMAL.
               25 PMPT-VERSION         PIC S9(004)       BINARY.
               25 PDPT-ID              PIC S9(015)       PACKED-DECIMAL.
               25 PEST-ID              PIC S9(015)       PACKED-DECIMAL.
              20 P2-AUS-BESTANDS-DTA.
               25 P2JAHRESNETTOPRAEMIE PIC  9(007)V9(02).
              20 P2-AUS-HISTORIENDATEI.
               25 P2VERTRAGSBEGINN     PIC  9(006).
               25 P2VERTRAGSABLAUF     PIC  9(006).
              20 P2-FUER-PMBE.
               25 P2ORIGINAEREMELDUNG  PIC  X(001).
               25 P2KUMINDEX           PIC S9(004)       BINARY.
              20 P2-FUER-AP-FP-TRENNUNG.
               25 P2ORIGINALFAELLIG    PIC  9(007)       PACKED-DECIMAL.
             15 P2-DATEN.
              20 P2SA                  PIC  X(002).
              20 P2KEY.
               25 P2GES                PIC  X(002).
               25 P2INTVSNR            PIC  9(015).
               25 P2KFAELLIG           PIC  9(006).
               25 P2PRODSCHL           PIC  9(002).
               25 P2LFDNR              PIC  9(002).
               25 P2LFDNR-BI           REDEFINES P2LFDNR
                                       PIC S9(004)       BINARY.
              20 P2SATZ.
               25 P2EKRIT.
                30 P2EPGM              PIC  X(008).
                30 P2EDATUM            PIC  9(007)       PACKED-DECIMAL.
                30 P2EZEIT             PIC  9(007)       PACKED-DECIMAL.
                30 P2PSNR              PIC  9(007)       PACKED-DECIMAL.
               25 P2FREIKZ             PIC  X(001).
               25 P2VSNR               PIC  9(015)       PACKED-DECIMAL.
               25 P2FAELLIG            PIC  9(007)       PACKED-DECIMAL.
               25 P2BUCHDATPART        PIC  9(007)       PACKED-DECIMAL.
               25 P2BUCHDATAML         PIC  9(007)       PACKED-DECIMAL.
               25 P2TERMIN             PIC  9(007)       PACKED-DECIMAL.
               25 P2FILLER             PIC  X(017).
               25 P2BAD                REDEFINES P2FILLER.
                30 P2BAD-LAUFZEIT      PIC  9(002).
                30 P2BAD-REST          PIC  X(015).
               25 P2BFG                REDEFINES P2FILLER.
                30 P2BFG-KDINH.
                 35 P2BFG-KDAVERM      PIC  9(008).
                30 P2BFG-KDDAT         PIC  9(006).
                30 P2BFG-REST          PIC  X(003).
               25 P2ROLAND             REDEFINES P2FILLER.
                30 P2ROLFILL           PIC  X(008).
                30 P2IPBETR            PIC  9(007)V9(02) PACKED-DECIMAL.
                30 P2IPPROVS           PIC  9(002)V9(01) PACKED-DECIMAL.
                30 P2LAUFZEIT          PIC  X(002).
               25 P2GES91              REDEFINES P2FILLER.
                30 P2GES91-GRUNDPROV   PIC  9(002)V9(03).
                30 P2GES91-REST        PIC  X(012).
               25 P2LFDNRE             PIC  9(002).
               25 P2PRODART            PIC  9(001).
               25 P2PROVPFLSU          PIC S9(007)V9(02) PACKED-DECIMAL.
               25 P2PRORATASU          PIC S9(007)V9(02) PACKED-DECIMAL.
               25 P2PROVSATZ           PIC S9(003)V9(01) PACKED-DECIMAL.
               25 P2GESPROVBETR        PIC S9(007)V9(02) PACKED-DECIMAL.
               25 P2EDKZ               PIC  X(001).
               25 P2DDRKZ              PIC  X(001).
               25 P2AUSKZ              PIC  X(001).
               25 P2FILLER2            PIC  X(007).
               25 P2STRUKANZ           PIC  9(002).
               25 P2STRUKTABELLE.
                30 P2STRUKTAB          OCCURS 020 TIMES
                                       INDEXED BY IX-P2STRUKTAB.
                 35 P2VBNR             PIC  9(007)       PACKED-DECIMAL.
                 35 P2STRUKSTU         PIC  9(003).
                 35 P2DIFFPROV         PIC S9(003)V9(01) PACKED-DECIMAL.
                 35 P2PROVBETR         PIC S9(005)V9(02) PACKED-DECIMAL.
                 35 P2PROVRUECK        PIC S9(005)V9(02) PACKED-DECIMAL.
                 35 P2PROVEINH         PIC S9(006)V9(01) PACKED-DECIMAL.
      *
          01 P2AHGSEG-ZEILE-ZW         PIC  X(624).
