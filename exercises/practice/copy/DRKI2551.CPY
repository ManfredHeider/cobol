      * ***************************************************************
      *
      *    Anteile per 31.12.xxxx speichern (Guthabenanteil/GHA)
      *
      *    SATZLÄNGE  94
      *
      *    Suffix -N : Nullif-Indikator-Felder
      *
      *    LOGBUCH
      * --------!-------------------------------------------!----------
      * Auftrag ! Datum  ! Text                             ! Name
      * --------!-------------------------------------------!----------
      * AKI01094!09.12.11! Neuerstellung                    ! MDI
      *---------!-------------------------------------------!----------
      *DBO-10080!10.08.22!Feldlängenänderung für            ! 
      *         !        !GHAGUTHABENANT S9(007)V9(004) auf !
      *         !        !11B                               !u131774
      * ---------------------------------------------------------------
      * ENDE-LOGBUCH
      *****************************************************************
         01  DRKI2551.
             10 DRKI2551-GES              PIC S9(004) COMP.
             10 DRKI2551-VNR              PIC  X(015).
             10 DRKI2551-VNR-N            PIC  X(001).
             10 DRKI2551-VERTRAGSID       PIC  X(026).
             10 DRKI2551-GHAPRODLFDNR     PIC S9(004) COMP.
             10 DRKI2551-GHALFDNR         PIC S9(004) COMP.
             10 DRKI2551-GHAFONDID        PIC S9(004) COMP.
             10 DRKI2551-FONDISIN         PIC  X(012).
             10 DRKI2551-FONDISIN-N       PIC  X(001).
             10 DRKI2551-FONDWKN          PIC  X(006).
             10 DRKI2551-GHAGUTHABEN      PIC S9(009)V9(002) COMP-3.
             10 DRKI2551-GHAGUTHABEN-N    PIC  X(001).
             10 DRKI2551-GHAGUTHABENANT   PIC S9(007)V9(004) COMP-3.
             10 DRKI2551-GHAGUTHABENANT-N PIC  X(001).
             10 DRKI2551-GHALIEFERDATUM   PIC  X(010).
             10 DRKI2551-GHALIEFERDATUM-N PIC  X(001).

