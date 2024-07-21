      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1476.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA1476.CPv  $
      *
      *   Rev 2.8   Apr 12 2018 09:24:54   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - Copy
      * JWA
      *
      *   Rev 2.7   Apr 04 2018 15:35:34   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.6   Sep 29 2015 12:21:50   t13ex67
      *APA02154: Ges. 50 VP in der prod. SST
      *
      *   Rev 2.5   Oct 08 2012 10:46:26   t13ae46
      *APA01462: Sonderkennzeichen im Central-Provisions-DTA
      *
      *   Rev 2.4   19 May 2006 10:02:10   t13ae46
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.3   Mar 20 2006 08:38:32   t13ex44
      *APA00414: Erweiterung der Felder für Haftungszeitangaben bei
      * Central
      *
      *   Rev 2.2   Apr 18 2005 09:06:22   t13ex44
      *APA00341: Umstellung Provisionsbelieferung Central von CVIS
      *auf KOMPAS
      *
      *   Rev 2.1   13 Mar 2001 16:42:36   T13EX25
      *APA00131; PEG Central
      *
      *   Rev 2.0   09 Jan 2001 10:22:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1438  PEG-DATENSATZ FUER CENTRAL GES 50
      *    --------------------------------------------------------
      *    ERSTELLT: JAEGER
      *    DATUM:    28.02.01
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!28.02.01!2.1 ! NEU                       !JAEGER    *
      * APA00341!22.02.05!2.2 ! KOMPLETT UMSTRUKTURIERT   !SCHEIDEL  *
      * APA00414!12.01.06!2.3 ! HAFTZEITDAUER UND FAELLIG-!SCHEIDEL  *
      *         !        !    ! KEITSDAUER VIERSTELLIG    !          *
      * FPA00846!11.05.06!2.4 ! SONDERHAFT-KZ HINZU       !WANG      *
      * APA01462!07.09.12!2.5 ! SONDERKENNZEICHEN HINZU   !WANG      *
      * APA02154!29.09.15!2.6 ! VERSICHERTE PERSON HINZU  !STORCH    *
      * APA02579!22.03.18!2.7 ! PRODUKT-PG AUF 8 STELLEN  !WANG      *
      *         !        !2.8 ! ERWEITERT                 !          *
      *         !        !    !                           !          *
      *                                                              *
      *ENDE-LOGBUCH                                                  *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    Central-DTA's.                                            *
      *                                                              *
      ****************************************************************
      *
             30 CENTRAL-DATENSATZ.
                35 VNR                    PIC  9(015).
220205          35 VVNR1                  PIC  9(015).
220205          35 VVNR2                  PIC  9(015).
220205          35 VVNR3                  PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
220205          35 VERTRAG-ABLAUF         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 FAELLIG-BIS            PIC  X(010).
220205*         35 FAELLIG-DAUER          PIC  9(003).
                35 AVERM                  PIC  9(007).
                35 PGEB-GES               PIC  9(002).
                35 KUNDE-NNAME            PIC  X(025).
                35 KUNDE-VNAME            PIC  X(020).
                35 ABR-DAT-PG             PIC  X(010).
220205          35 EINL-DAT-PG            PIC  X(010).
220205*         35 PIN                    PIC  9(009).
220205          35 PIN                    PIC  9(007).
220205*         35 ZAHLWEISE              PIC  9(002).
220205          35 ZAHLWEISE              PIC  9(003).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
220205*         35 PGV-PG                 PIC  X(004).
220205          35 SYSTEM-KZ              PIC  X(003).
220205          35 GV-TIMESTAMP           PIC  X(026).
120106*         35 FAELLIG-DAUER          PIC  9(003).
120106          35 FAELLIG-DAUER          PIC  9(004).
                35 HAFTZEITBEGINN         PIC  X(010).
220205          35 HAFTZEITABLAUF         PIC  X(010).
120106*         35 HAFTZEITDAUER          PIC  9(003).
120106          35 HAFTZEITDAUER          PIC  9(004).
120106*         35 FILLER                 PIC  X(074).
110506          35 SONDERHAFT-KZ          PIC  X(002).
110506*         35 FILLER                 PIC  X(072).
070912          35 SONDERKENNZEICHEN      PIC  X(002).
290915          35 VERSICHERTE-PERSON     PIC  X(002).
070912*         35 FILLER                 PIC  X(070).
070912*         35 FILLER                 PIC  X(068).
290915          35 FILLER                 PIC  X(066).
220205          35 TAB-INDEX              PIC  9(002).
220205          35 PRODUKT-TABELLE.
220318*            40 PRODUKT-ZEILE       OCCURS 50.
220318             40 PRODUKT-ZEILE       OCCURS 45.
220205*         35 PRODUKTIONSART         PIC  9(003).
220205                45 PGV-PG           PIC  9(004).
220205                45 BUCHREG          PIC  X(005).
220318                45 FILLER           PIC  X(004).
220205                45 PRODUKT-PG       PIC  X(004).
220205*         35 PPFL-BEITRAG           PIC S9(009) PACKED-DECIMAL.
220205                45 PPFL-BETRAG      PIC S9(011)V9(02).
220205*         35 PROV-SATZ              PIC  9(007).
220205                45 PROV-SATZ        PIC S9(005)V9(02).
220205*         35 PROV-BETRAG            PIC S9(009) PACKED-DECIMAL.
                      45 PROV-BETRAG      PIC S9(011)V9(02).
220205                45 JAHRES-NETTO-PR  PIC S9(011)V9(02).
220205                45 VERTR-T-VERSION  PIC  9(015).
220205*         35 FILLER                 PIC  X(036).



