      *****************************************************************
      *    COPY-MEMBER: DRAN0381                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *                                                              
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0381
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<                                          
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090!28.06.21! NEU                          ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  PI-DRAN0381.

             10 PI-DRAN0381-CABA-HVANTNUM       PIC  X(09).

      *
       01  PO-DRAN0381.
           05  PO-DRAN0381-RC                   PIC  X(03).
               88 PO-DRAN0381-RC-OK             VALUE 'OK '.
               88 PO-DRAN0381-RC-NOT-FOUND      VALUE 'NFD'.
      *
           05  PO-DRAN0381-DATEN.
             10 PO-DRAN0381-CABA-PIN            PIC S9(09) COMP.
             10 PO-DRAN0381-CABA-AVERM          PIC S9(09) COMP.
             10 PO-DRAN0381-CABA-AVERM-PZ       PIC  X(01).
             10 PO-DRAN0381-CABA-GEWERBE-VB     PIC S9(09) COMP.
             10 PO-DRAN0381-CABA-VM             PIC S9(09) COMP.
             10 PO-DRAN0381-CABA-VM-PZ          PIC  X(01).
             10 PO-DRAN0381-CABA-DIR            PIC S9(04) COMP.
             10 PO-DRAN0381-CABA-ANTRAGSTELLER.
                15 PO-DRAN0381-CABA-AS-TABELLE OCCURS 5.
                   20 PO-DRAN0381-CABA-AS-ZUNAME     PIC X(26).
                   20 PO-DRAN0381-CABA-AS-VORNAME    PIC X(26).
                   20 PO-DRAN0381-CABA-AS-PLZ        PIC X(06).
                   20 PO-DRAN0381-CABA-AS-ORT        PIC X(30).
                   20 PO-DRAN0381-CABA-AS-LKZ        PIC X(03).
                   20 PO-DRAN0381-CABA-AS-GEB-DAT    PIC X(10).
                   20 PO-DRAN0381-CABA-AS-STAA       PIC X(03).
                   20 PO-DRAN0381-CABA-AS-BRANCHE    PIC X(40).
                   20 PO-DRAN0381-CABA-AS-BERUFSGRP  PIC X(40).
                   20 PO-DRAN0381-CABA-AS-PEP        PIC X(01).
                   20 PO-DRAN0381-CABA-AS-KD-BID     PIC S9(18)V COMP-3.
             10 PO-DRAN0381-CABA-DEPOT-NR      PIC S9(11)V COMP-3.
             10 PO-DRAN0381-CABA-GESAMT-EH     PIC S9(06)V9 COMP-3.   
             10 PO-DRAN0381-CABA-DYNAMIK       PIC S9(03)V9(03) COMP-3.
             10 PO-DRAN0381-CABA-ANTRAG-DAT    PIC  X(010).
             10 PO-DRAN0381-CABA-ESIGN         PIC  X(01).
             10 PO-DRAN0381-CABA-VERSION       PIC  X(04).
             10 PO-DRAN0381-CABA-KENNUNG-INS   PIC  X(08).
             10 PO-DRAN0381-CABA-ZEIT-INSERT   PIC  X(26).
             10 PO-DRAN0381-CABA-KENNUNG-UPD   PIC  X(08).
             10 PO-DRAN0381-CABA-ZEIT-UPDATE   PIC  X(26).
      *****************************************************************
      *---------------------- ENDE DRAN0381 --------------------------* 
      *****************************************************************
