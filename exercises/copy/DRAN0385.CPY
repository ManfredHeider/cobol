      *****************************************************************
      *    COPY-MEMBER: DRAN0385                                      *
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0385                    *
      *    ELEKTRONISCHE DATEN CAPITALBANK (33)                       *
      *                                                              
      *    <SELECT ALL ÜBER GES + VORSCHLAGSNUMMER>                 
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090!06.07.21! NEU                          ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  PI-DRAN0385.
           05  PI-DRAN0385-KEY.
             10 PI-DRAN0385-CABA-HVANTNUM      PIC  X(09).
      *
       01  PO-DRAN0385.
           05  PO-DRAN0385-RC                   PIC  X(03).
               88 PO-DRAN0385-RC-OK                  VALUE 'OK '.
               88 PO-DRAN0385-RC-NOT-FOUND           VALUE 'NFD'.
      *
           05  PO-DRAN0385-DATEN.
             10 PO-DRAN0385-CABA-PIN            PIC S9(09) COMP.
             10 PO-DRAN0385-CABA-AVERM          PIC S9(09) COMP.
             10 PO-DRAN0385-CABA-AVERM-PZ       PIC  X(001).
             10 PO-DRAN0385-CABA-GEWERBE-VB     PIC S9(09) COMP.
             10 PO-DRAN0385-CABA-VM             PIC S9(09) COMP.
             10 PO-DRAN0385-CABA-VM-PZ          PIC  X(01).
             10 PO-DRAN0385-CABA-DIR            PIC S9(04) COMP.
             10 PO-DRAN0385-CABA-ANTRAGSTELLER.
                15 PO-DRAN0385-CABA-AS-TABELLE OCCURS 5.
                   20 PO-DRAN0385-CABA-AS-ZUNAME     PIC X(26).
                   20 PO-DRAN0385-CABA-AS-VORNAME    PIC X(26).
                   20 PO-DRAN0385-CABA-AS-PLZ        PIC X(06).
                   20 PO-DRAN0385-CABA-AS-ORT        PIC X(30).
                   20 PO-DRAN0385-CABA-AS-LKZ        PIC X(03).
                   20 PO-DRAN0385-CABA-AS-GEB-DAT    PIC X(10).
                   20 PO-DRAN0385-CABA-AS-STAA       PIC X(03).
                   20 PO-DRAN0385-CABA-AS-BRANCHE    PIC X(40).
                   20 PO-DRAN0385-CABA-AS-BERUFSGRP  PIC X(40).
                   20 PO-DRAN0385-CABA-AS-PEP        PIC X(01).
                   20 PO-DRAN0385-CABA-AS-KD-BID     PIC S9(18)V COMP-3.
             
             10 PO-DRAN0385-CABA-DEPOT-NR      PIC S9(11)V COMP-3.
             10 PO-DRAN0385-CABA-GESAMT-EH     PIC S9(06)V9 COMP-3.   
             10 PO-DRAN0385-CABA-DYNAMIK       PIC S9(03)V9(03) COMP-3.
             10 PO-DRAN0385-CABA-ESIGN         PIC  X(01).
             10 PO-DRAN0385-CABA-VERSION       PIC  X(04).
             10 PO-DRAN0385-CABA-ANTRAG-DAT    PIC  X(10).
             10 PO-DRAN0385-CABA-KENNUNG-INS   PIC  X(08).
             10 PO-DRAN0385-CABA-ZEIT-INSERT   PIC  X(26).
             10 PO-DRAN0385-CABA-KENNUNG-UPD   PIC  X(08).
             10 PO-DRAN0385-CABA-ZEIT-UPDATE   PIC  X(26).
 
           05 PO-DRAN0385-PROD-DATEN-TABELLE.
             10 PO-DRAN0385-PRODUKT-ELEMENT OCCURS 15 TIMES.
                20 PO-DRAN0385-CABA-PROD-SCHL PIC S9(04) COMP.
                20 PO-DRAN0385-CABA-SUMME     PIC S9(09)V9(02) COMP-3.
                20 PO-DRAN0385-CABA-ZAHLWEISE PIC S9(04) COMP.
                20 PO-DRAN0385-CABA-LAUFZEIT  PIC S9(4) COMP.
                20 PO-DRAN0385-CABA-EINHEITEN PIC S9(06)V9(02) COMP-3.
                20 PO-DRAN0385-CABA-ZUSATZ-1  PIC S9(10)V9(03) COMP-3.
                20 PO-DRAN0385-CABA-ZUSATZ-2  PIC S9(10)V9(03) COMP-3.

      *****************************************************************
      *---------------------- ENDE XXXX0000 --------------------------* 
      *****************************************************************
