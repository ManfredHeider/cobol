      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1508.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1508.CPv  $
      *
      *   Rev 2.2   Jul 11 2002 08:28:12   t13ex43
      *APA00204 : Redefinition P2FILLER an OPAL3 angepaßt
      *
      *   Rev 2.1   09 Nov 2001 15:39:52   t13ex43
      *APA00168: Migration DIT Österreich und Bank Austria Invest
      *
      *   Rev 2.0   04 Jul 2001 11:10:48   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1508  TABELLE MIT AHGSEG-DATEN (OPAL3)   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    06.11.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00168!06.11.01!2.01! NEU                       ! KLEEMEIER*
      * APA00204!11.07.02!2.02! P2FILLER FUER OPAL3-AUFBAU! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE ZWISCHENSPEICHER-TABELLE          *
      *    FÜR DIE GELESENEN DATEN AUS AHGSEG (OPAL3) IN UPPA0096    *
      *    (DROP0005)                                                *
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
110702****     25 P2BAD                REDEFINES P2FILLER.
110702****      30 P2BAD-LAUFZEIT      PIC  9(002).
110702****      30 P2BAD-REST          PIC  X(015).
110702****     25 P2BFG                REDEFINES P2FILLER.
110702****      30 P2BFG-KDINH.
110702****       35 P2BFG-KDAVERM      PIC  9(008).
110702****      30 P2BFG-KDDAT         PIC  9(006).
110702****      30 P2BFG-REST          PIC  X(003).
110702****     25 P2ROLAND             REDEFINES P2FILLER.
110702****      30 P2ROLFILL           PIC  X(008).
110702****      30 P2IPBETR            PIC  9(007)V9(02) PACKED-DECIMAL.
110702****      30 P2IPPROVS           PIC  9(002)V9(01) PACKED-DECIMAL.
110702****      30 P2LAUFZEIT          PIC  X(002).
110702****     25 P2GES91              REDEFINES P2FILLER.
110702****      30 P2GES91-GRUNDPROV   PIC  9(002)V9(03).
110702****      30 P2GES91-REST        PIC  X(012).
110702         25 P2AVBG               REDEFINES P2FILLER.
110702          30 P2AVBG-REST         PIC X(04).
110702          30 P2HAFTZEIT-BEGINN   PIC 9(06) PACKED-DECIMAL.
110702          30 P2HAFTZEIT-ENDE     PIC 9(06) PACKED-DECIMAL.
110702          30 P2AVBG-REST2        PIC X(05).
110702         25 P2GES91              REDEFINES P2FILLER.
110702          30 P2GES91-GRUNDPROV   PIC 9(02)V999.
110702          30 P2GES91-REST        PIC X(12).
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
               25 P2FILLER3            PIC  X(030).
               25 P2ALLIANZ REDEFINES P2FILLER3.
                30 P2JNBEITR           PIC  9(007)V99 PACKED-DECIMAL.
                30 P2INKSTD            PIC  9(006)    PACKED-DECIMAL.
                30 P2TARIF             PIC  X(004).
                30 P2BESTSCHL          PIC  X(002).
                30 P2GES25-EH-AUFSCHLAG
                                       PIC  X(001).
                30 P2ALLIANZ-REST      PIC  X(014).
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
          01 P2AHGSEG-ZEILE-ZW         PIC  X(654).
      *
