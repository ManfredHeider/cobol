      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0765.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0765.CPv  $
      *
      *   Rev 2.3   Feb 03 2016 14:58:16   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.2   Jul 16 2015 15:08:14   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.1   May 27 1999 12:16:24   T13EX26
      *NT-Umstellung
      *
      *   Rev 2.0   May 26 1999 16:58:08   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:46:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0765  FUNKTION ERMITTELN ALLE PDPT'S     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      * APA00012!02.09.98! 02 ! NESTED COPY ENTFERNT (DN) ! PARENT   *
      * APA01932!29.06.15! 2.2! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02207!01.02.16! 2.3! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0765.
          05 PI-DRPA0765-PROGRAMM                   PIC  X(008).
          05 PI-DRPA0765-UFKT-CODE                  PIC  9(002).
          05 PI-DRPA0765-DATEN.
             10 PI-DRPA0765-PMPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0765-PMPT-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA0765.
          05 PO-DRPA0765-RC                         PIC  X(003).
             88 PO-DRPA0765-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0765-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0765-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0765-PDPT.
             10 PO-DRPA0765-PDPT-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA0765-PMPT-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA0765-PMPT-VERSION         PIC S9(4) COMP.
             10 PO-DRPA0765-PEST-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA0765-PDPT-BUCH-RFOLGE     PIC S9(4) COMP.
             10 PO-DRPA0765-PDPT-MA-ABR-DAT      PIC X(10).
             10 PO-DRPA0765-KNR-BUCHUNGS-STAND   PIC S9(4) COMP.
             10 PO-DRPA0765-KNR-BUCHUNGSART      PIC S9(4) COMP.
             10 PO-DRPA0765-KNR-BUCHUNGS-GRUND   PIC S9(004) BINARY.
             10 PO-DRPA0765-KNR-KZ-EH-MAX        PIC S9(004) BINARY.
             10 PO-DRPA0765-KNR-BERCH-WAEHRUNG   PIC S9(4) COMP.
             10 PO-DRPA0765-PDPT-DGV             PIC S9(4) COMP.
             10 PO-DRPA0765-PDPT-PRODART         PIC S9(4) COMP.
             10 PO-DRPA0765-PDPT-FIRST-INSERT    PIC X(26).
             10 PO-DRPA0765-PDPT-FIRST-KENNUNG   PIC X(8).
             10 PO-DRPA0765-PDPT-LAST-UPDATE     PIC X(26).
             10 PO-DRPA0765-PDPT-KENNUNG         PIC X(8).
             10 PO-DRPA0765-PDPT-PDPE-DATEN.
                15 PO-DRPA0765-PDPT-PDPE-DAT-LEN PIC S9(4) USAGE COMP.
290615*         15 PO-DRPA0765-PDPT-PDPE-DAT-TEXT PIC X(720).
290615          15 PO-DRPA0765-PDPT-PDPE-DAT-TEXT PIC X(1220).
010216       10 PO-DRPA0765-PDPT-UST-JAHR        PIC S9(4) COMP.
020998*   05 PO-DRPA0765-DATEN.
020998*      10 PO-DRPA0765-PDPT.
020998*         COPY DRPA0571.
      ******* ENDE COPY-MEMBER **************************************
 

