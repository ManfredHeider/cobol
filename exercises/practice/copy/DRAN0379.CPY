      *****************************************************************
      *    COPY-MEMBER: DRAN0379                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAN0379 INSERT TBAN0031    *
      *    --------------------------------------------------------   *
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1090!25.06.21! NEU                          ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAN0379
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAN0379.
           05  PI-DRAN0379-KEY.

             10 PI-DRAN0379-CABA-HVANTNUM       PIC  X(09).

           05  PI-DRAN0379-DATEN.

             10 PI-DRAN0379-CABA-PIN            PIC S9(09) COMP.
             10 PI-DRAN0379-CABA-VB-GES         PIC S9(04).
             10 PI-DRAN0379-CABA-AVERM          PIC S9(09) COMP.
             10 PI-DRAN0379-CABA-AVERM-PZ       PIC  X(01).
             10 PI-DRAN0379-CABA-GEWERBE-VB     PIC S9(09) COMP.
             10 PI-DRAN0379-CABA-VM             PIC S9(09) COMP.
             10 PI-DRAN0379-CABA-VM-PZ          PIC  X(01).
             10 PI-DRAN0379-CABA-VM-ZUNA        PIC  X(26).
             10 PI-DRAN0379-CABA-VM-VORNA       PIC  X(26).
             10 PI-DRAN0379-CABA-DIR            PIC S9(03).
             10 PI-DRAN0379-CABA-ANTRAGSTELLER.
                15 PI-DRAN0379-CABA-AS-TABELLE OCCURS 5.
                   20 PI-DRAN0379-CABA-AS-ZUNAME  PIC  X(26).
                   20 PI-DRAN0379-CABA-AS-VNAME   PIC  X(26).
                   20 PI-DRAN0379-CABA-AS-PLZ     PIC  X(06).
                   20 PI-DRAN0379-CABA-AS-ORT     PIC  X(30).
                   20 PI-DRAN0379-CABA-AS-LKZ     PIC  X(03).
                   20 PI-DRAN0379-CABA-AS-GEB-DAT PIC  X(10).
                   20 PI-DRAN0379-CABA-AS-STAA    PIC  X(03).
                   20 PI-DRAN0379-CABA-AS-BRA     PIC  X(40).
                   20 PI-DRAN0379-CABA-AS-BGRP    PIC  X(40).
                   20 PI-DRAN0379-CABA-AS-PEP     PIC  X(01).
                   20 PI-DRAN0379-CABA-AS-KD-BID  PIC S9(18)V COMP-3.
             10 PI-DRAN0379-CABA-DEPOT-NR         PIC  9(11).
             10 PI-DRAN0379-CABA-EINH-GESAMT      PIC S9(06)V9(01) COMP.
             10 PI-DRAN0379-CABA-DYNAMIK          PIC S9(03)V9(03) COMP.
             10 PI-DRAN0379-CABA-ANTRAG-DAT       PIC  X(10).
             10 PI-DRAN0379-CABA-E-SIGN           PIC  X(01).
             10 PI-DRAN0379-CABA-VERSION          PIC  X(04).
             10 PI-DRAN0379-CABA-KENNUNG-INS      PIC  X(08).
             10 PI-DRAN0379-CABA-KENNUNG-UP       PIC  X(08).
             10 PI-DRAN0379-CABA-XGDV.
                 49 PI-DRAN0379-CABA-XGDV-LEN     PIC S9(04)  COMP.   
                 49 PI-DRAN0379-CABA-XGDV-TEXT    PIC X(29500).
      *
       01  PO-DRAN0379.
           05  PO-DRAN0379-RC                     PIC  X(003).
               88 PO-DRAN0379-RC-OK              VALUE 'OK '.
               88 PO-DRAN0379-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      *****************************************************************
      *---------------------- ENDE DRAN0379 --------------------------* 
      *****************************************************************
