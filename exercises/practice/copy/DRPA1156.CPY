      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1156.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1156.CPv  $
      *
      *   Rev 2.4   Feb 03 2016 14:58:42   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.3   Jul 16 2015 15:09:00   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.2   May 27 1999 16:04:36   T13EX26
      *Prod-Üb
      *
      *   Rev 2.1   May 27 1999 15:55:38   T13EX26
      *MFE die 2.
      *
      *   Rev 2.0   May 26 1999 16:58:24   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   21 Oct 1998 14:01:44   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1156  FUNKTION "LESEN LETZTES PDPT ZU    *
      *                                           EINEM PMPT         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    28.01.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!28.01.99! 01 ! NEUERSTELLUNG             ! PARENT   *
      * APA01932!21.04.15! 2.1! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02202!01.02.16! 2.2! EINFÜHRUNG STEUERJAHR     ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1156.
          05 PI-DRPA1156-DATEN.
             10 PI-DRPA1156-PMPT-ID              PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1156.
          05 PO-DRPA1156-RC                      PIC  X(003).
             88 PO-DRPA1156-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1156-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1156-PDPT.
             10 PO-DRPA1156-PDPT-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA1156-PMPT-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA1156-PMPT-VERSION         PIC S9(4) COMP.
             10 PO-DRPA1156-PEST-ID              PIC S9(15)V COMP-3.
             10 PO-DRPA1156-PDPT-BUCH-RFOLGE     PIC S9(4) COMP.
             10 PO-DRPA1156-PDPT-MA-ABR-DAT      PIC X(10).
             10 PO-DRPA1156-KNR-BUCHUNGS-STAND   PIC S9(4) COMP.
             10 PO-DRPA1156-KNR-BUCHUNGSART      PIC S9(4) COMP.
             10 PO-DRPA1156-KNR-BUCHUNGS-GRUND   PIC S9(004) BINARY.
             10 PO-DRPA1156-KNR-KZ-EH-MAX        PIC S9(004) BINARY.
             10 PO-DRPA1156-KNR-BERCH-WAEHRUNG   PIC S9(4) COMP.
             10 PO-DRPA1156-PDPT-DGV             PIC S9(4) COMP.
             10 PO-DRPA1156-PDPT-PRODART         PIC S9(4) COMP.
             10 PO-DRPA1156-PDPT-FIRST-INSERT    PIC X(26).
             10 PO-DRPA1156-PDPT-FIRST-KENNUNG   PIC X(8).
             10 PO-DRPA1156-PDPT-LAST-UPDATE     PIC X(26).
             10 PO-DRPA1156-PDPT-KENNUNG         PIC X(8).
             10 PO-DRPA1156-PDPT-PDPE-DATEN.
                15 PO-DRPA1156-PDPT-PDPE-DAT-LEN PIC S9(4) USAGE COMP.
210415*         15 PO-DRPA1156-PDPT-PDPE-DAT-TEXT PIC X(720).
210415          15 PO-DRPA1156-PDPT-PDPE-DAT-TEXT PIC X(1220).
010216       10 PO-DRPA1156-PDPT-UST-JAHR        PIC S9(4) BINARY.
      ******* ENDE COPY-MEMBER **************************************
 

