      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1785.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1785.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:59:40   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Jul 16 2015 15:09:40   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.1   Oct 08 2014 15:19:26   t13ex67
      *APA01702: Erweiter. WEPA0008 um Historien-Tab. (TBPA0313/315
      */317/322)
      *
      *   Rev 2.0   Dec 13 2013 11:28:58   t13ae39
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1785  FUNKTION ALLE MELD ZU PMPT-ID+VERS.*
      *    -------------------------------------------------------   *
      *    ERSTELLT: STRAUSS                                         *
      *    DATUM:    13.12.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01702!13.12.13! 2.0! NEUERSTELLUNG             ! STRAUSS  *
      * APA01932!29.06.15! 2.2! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02208!01.02.16! 2.3! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1785.
          05 PI-DRPA1785-PROGRAMM         PIC  X(008).
          05 PI-DRPA1785-UFKT             PIC  9(002).
          05 PI-DRPA1785-DATEN.
             10 PI-DRPA1785-PGEB-GES      PIC S9(004)  BINARY.
             10 PI-DRPA1785-PMPT-ID       PIC S9(015)  PACKED-DECIMAL.
             10 PI-DRPA1785-PMPT-VERSION  PIC S9(004)  BINARY.
      *
       01 PO-DRPA1785.
          05 PO-DRPA1785-RC               PIC  X(003).
             88 PO-DRPA1785-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1785-RC-LEER                    VALUE 'LER'.
             88 PO-DRPA1785-RC-EOF                     VALUE 'EOF'.
          05 PO-DRPA1785-PDPT-PDPE.
             10 PO-DRPA1785-PDPT.
                15 PO-DRPA1785-PDPT-ID             PIC S9(15)V COMP-3.
                15 PO-DRPA1785-PMPT-ID             PIC S9(15)V COMP-3.
                15 PO-DRPA1785-PMPT-VERSION        PIC S9(4) COMP.
                15 PO-DRPA1785-PEST-ID             PIC S9(15)V COMP-3.
                15 PO-DRPA1785-PDPT-BUCH-RFOLGE    PIC S9(4) COMP.
                15 PO-DRPA1785-PDPT-MA-ABR-DAT     PIC X(10).
                15 PO-DRPA1785-KNR-BUCHUNGS-STAND  PIC S9(4) COMP.
                15 PO-DRPA1785-KNR-BUCHUNGSART     PIC S9(4) COMP.
                15 PO-DRPA1785-KNR-BUCHUNGS-GRUND  PIC S9(004) BINARY.
                15 PO-DRPA1785-KNR-KZ-EH-MAX       PIC S9(004) BINARY.
                15 PO-DRPA1785-KNR-BERCH-WAEHRUNG  PIC S9(4) COMP.
                15 PO-DRPA1785-PDPT-DGV            PIC S9(4) COMP.
                15 PO-DRPA1785-PDPT-PRODART        PIC S9(4) COMP.
                15 PO-DRPA1785-PDPT-FIRST-INSERT   PIC X(26).
                15 PO-DRPA1785-PDPT-FIRST-KENNUNG  PIC X(8).
                15 PO-DRPA1785-PDPT-LAST-UPDATE    PIC X(26).
                15 PO-DRPA1785-PDPT-KENNUNG        PIC X(8).
                15 PO-DRPA1785-MAX-USED-PDPE       PIC S9(4) COMP.
                15 PO-DRPA1785-PDPE-DATEN.
                  20 PO-DRPA1785-PDPE-DAT-LEN  PIC S9(4) USAGE COMP.
290615*           20 PO-DRPA1785-PDPE-DAT-TEXT PIC X(720).
290615            20 PO-DRPA1785-PDPE-DAT-TEXT PIC X(1220).
010216          15 PO-DRPA1785-PDPT-UST-JAHR       PIC S9(4) COMP.
      *
      *---------------------------------------------------------------*
 

