      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1052.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1052.CPv  $
      *
      *   Rev 2.3   Feb 10 2016 08:45:58   t13ae37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Feb 03 2016 14:58:36   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.1   Jul 16 2015 15:08:52   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.0   May 26 1999 15:45:50   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 14:01:08   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1052  FUNKTION ERMITTELN ALLE PDPT'S     *
      *                           ZUR PMPT-ID UND PMPT-VERSION       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    28.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!28.08.98! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      * apa00012! 1.09.98! 1.1! Kennung,Last- und First-Up! Parent   *
      *         !        !    ! date und drpa0571 entfernt!          *
      * APA01932!24.04.15! 2.1! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02208!01.02.16! 2.2! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      * APA02208!10.02.16! 2.3! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1052.
          05 PI-DRPA1052-PROGRAMM                   PIC  X(008).
          05 PI-DRPA1052-UFKT-CODE                  PIC  9(002).
          05 PI-DRPA1052-DATEN.
             10 PI-DRPA1052-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1052-PMPT-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1052.
          05 PO-DRPA1052-RC                         PIC  X(003).
             88 PO-DRPA1052-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1052-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1052-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1052-PDPT.
             10 PO-DRPA1052-PDPT-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA1052-PMPT-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA1052-PMPT-VERSION         PIC S9(4) COMP.
             10 PO-DRPA1052-PEST-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA1052-PDPT-BUCH-RFOLGE     PIC S9(4) COMP.
             10 PO-DRPA1052-PDPT-MA-ABR-DAT      PIC X(10).
             10 PO-DRPA1052-KNR-BUCHUNGS-STAND   PIC S9(4) COMP.
             10 PO-DRPA1052-KNR-BUCHUNGSART      PIC S9(4) COMP.
             10 PO-DRPA1052-KNR-BUCHUNGS-GRUND   PIC S9(004) BINARY.
             10 PO-DRPA1052-KNR-KZ-EH-MAX        PIC S9(004) BINARY.
             10 PO-DRPA1052-KNR-BERCH-WAEHRUNG   PIC S9(4) COMP.
             10 PO-DRPA1052-PDPT-DGV             PIC S9(4) COMP.
             10 PO-DRPA1052-PDPT-PRODART         PIC S9(4) COMP.
             10 PO-DRPA1052-PDPT-FIRST-INSERT    PIC X(26).
             10 PO-DRPA1052-PDPT-FIRST-KENNUNG   PIC X(8).
             10 PO-DRPA1052-PDPT-LAST-UPDATE     PIC X(26).
             10 PO-DRPA1052-PDPT-KENNUNG         PIC X(8).
100216       10 PO-DRPA1052-PDPT-UST-JAHR        PIC S9(004) BINARY.
          05 PO-DRPA1052-PDPE.
             10 PO-DRPA1052-PDPT-PDPE-DATEN.
                15 PO-DRPA1052-PDPT-PDPE-DAT-LEN PIC S9(4) USAGE COMP.
240415*         15 PO-DRPA1052-PDPT-PDPE-DAT-TEXT PIC X(720).
240415          15 PO-DRPA1052-PDPT-PDPE-DAT-TEXT PIC X(1220).
010216*      10 PO-DRPA1052-PDPT-UST-JAHR        PIC S9(004) BINARY.
 
010998*         COPY DRPA0571.
010998*         35 PDPT-KENNUNG                     PIC X(08).
010998*         35 PDPT-LAST-UPDATE                 PIC X(26).
010998*         35 PDPT-FIRST-INSERT                PIC X(26).
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

