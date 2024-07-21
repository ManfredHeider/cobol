      ****************************************************************
      *    COPY-MEMBER: DRKI0281   VARIABLEN  FÜR UPRO UPKI0042      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO                                             *
      *    DATUM:    13.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00095!23.02.00!      DB-OPERATION ZÄHLEN       ! WHO      *
      *         !        !      FÜR REPLIKATIONSZÄHLER    !          *
      * -------------------------------------------------------------*
      * AKI00162!14.11.02!      Neue Felder DIT           !WHO       *
      *         !        !      DEUTSCHE BANK,            !          *
      *         !        !      NETTOBETRAG               !          *
      * -------------------------------------------------------------*
      * AKI00249!13.10.04!      GHBGUTHABENANT jetzt      !WHO       *
      *         !        !      4 Nachkommastellen        !          *
      *--------------------------------------------------------------*
      *DBO-10227!11.08.22!Längenänderung                  !u131774   *
      *         !        !GHBGUTHABENANT von 10 auf 11 B  !          * 
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 DRKI0281-GES.
          05 DRKI0281-FKT                   PIC  9(02).
             88 DRKI0281-INSERT                  VALUE 01.
             88 DRKI0281-READ                    VALUE 02.

          05 DRKI0281-DATEN.
             10 DRKI0281-GHBVERTRAGSID      PIC  X(13).
             10 DRKI0281-GHBGUTHABPRODLFDNR PIC S9(4) COMP.
             10 DRKI0281-GHBFONDID          PIC S9(4) COMP.
             10 DRKI0281-GHBGHALFDNR        PIC S9(4) COMP.
             10 DRKI0281-GHBBUCHUNGLFDNR    PIC S9(4) COMP.
             10 DRKI0281-GHBBETRAGLETZTBUCH PIC S9(9)V9(2) COMP-3.
             10 DRKI0281-GHBBETRLBUWKZ      PIC  X(01).
             10 DRKI0281-GHBBUCHUNGSDATUM   PIC  X(10).
             10 DRKI0281-GHBGUTHABENANT     PIC S9(7)V9(4) COMP-3.
             10 DRKI0281-GHBKZFINHLETZTBUID PIC S9(4) COMP.
             10 DRKI0281-GHBSTEUBETRLETZTBU PIC S9(9)V9(2) COMP-3.
             10 DRKI0281-GHBSTLETZTBUWKZ    PIC  X(01).

141102       10 DRKI0281-GHBNETTOBETRAG     PIC S9(9)V9(2) COMP-3.
141102       10 DRKI0281-GHBNETTOBETWKZ     PIC  X(01).


          05 DRKI0281-RC                    PIC  X(03).
             88 DRKI0281-RC-OK                   VALUE 'OK '.

          05 DRKI0281-DB-OP-Z               PIC  9(01).
