      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00887!22.05.09! 2.1! NEU                       ! RAHNER   *
      * FAN00909!27.11.09!    ! DATUM+UHRZEIT AUS TDS-SATZ! RAHNER   *
      * AAN01032!23.01.12! 2.2! ÜBERARBEITUNG IM RAHMEN   !          *
      *         !        !    ! GELDWÄSCHE/COMPILANCE     ! RAHNER   *
      * AAN01045!23.01.12! 2.3! ZEICHNER KONZESSION       ! RAHNER   *
      * AAN01051!15.05.12! 2.4!INTERNE ID-NUMMERN GENERALI! RAHNER   *
      * AAN01354!20.03.18! 2.5! WEITERE INHABER ÖSTERR.   ! RAHNER   *
      *                                                              *
      * AENDERUNGEN:                                                 *
      * AUFTRAG     ! DATUM  ! AENDERUNG                  ! NAME     *
      * -------------------------------------------------------------*
      * ANTRAG-1090 !12.08.21! ELEKTR. ANTRAG GES.33      ! RAHNER   *
      * ANTRAG-1928 !21.03.23! NEU DEVISENINLÄNDER        ! RAHNER   *
      *             !        !                            !          *
      *             !        !                            !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 WIEN-SATZ-AUS.
          05 WIEN-SATZ                     PIC X(1433).
      *
       01 WIEN-SATZ-DATEN.
            10 WIEN-SATZ-DATEN-SATZART     PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-1     PIC X(1).
            10 WIEN-SATZ-DATEN-VBNR        PIC 9(7).
            10 WIEN-SATZ-DATEN-SEMIK-2     PIC X(1).
            10 WIEN-SATZ-DATEN-GEWERBE-VBNR PIC 9(7).
            10 WIEN-SATZ-DATEN-SEMIK-3     PIC X(1).
            10 WIEN-SATZ-DATEN-VMNR        PIC 9(7).
            10 WIEN-SATZ-DATEN-SEMIK-4     PIC X(1).
            10 WIEN-SATZ-DATEN-PIN         PIC 9(15).
            10 WIEN-SATZ-DATEN-SEMIK-5     PIC X(1).
            10 WIEN-SATZ-DATEN-VNR-ANTR    PIC 9(15).
            10 WIEN-SATZ-DATEN-SEMIK-6     PIC X(1).
            10 WIEN-SATZ-DATEN-VORSCHL-NR  PIC X(9).
            10 WIEN-SATZ-DATEN-SEMIK-7     PIC X(1).
            10 WIEN-SATZ-DATEN-KD-VORNAME  PIC X(26).
            10 WIEN-SATZ-DATEN-SEMIK-8     PIC X(1).
            10 WIEN-SATZ-DATEN-KD-ZUNAME   PIC X(26).
            10 WIEN-SATZ-DATEN-SEMIK-9     PIC X(1).
            10 WIEN-SATZ-DATEN-KD-NUMMER   PIC X(18).
            10 WIEN-SATZ-DATEN-SEMIK-10    PIC X(1).
            10 WIEN-SATZ-DATEN-KD-GEB-DAT  PIC X(10).
            10 WIEN-SATZ-DATEN-SEMIK-11    PIC X(1).
            10 WIEN-SATZ-DATEN-KD-SOZVNR   PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-12    PIC X(1).
            10 WIEN-SATZ-DATEN-KD-AUSL-KZ  PIC X(3).
            10 WIEN-SATZ-DATEN-SEMIK-13    PIC X(1).
            10 WIEN-SATZ-DATEN-KD-PLZ      PIC X(6).
            10 WIEN-SATZ-DATEN-SEMIK-14    PIC X(1).
            10 WIEN-SATZ-DATEN-KD-ORT      PIC X(21).
            10 WIEN-SATZ-DATEN-SEMIK-15    PIC X(1).
            10 WIEN-SATZ-DATEN-KD-STRASSE  PIC X(26).
            10 WIEN-SATZ-DATEN-SEMIK-16    PIC X(1).
            10 WIEN-SATZ-DATEN-PGEB-GES    PIC 9(2).
            10 WIEN-SATZ-DATEN-SEMIK-17    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-1 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-18    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-2 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-19    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-3 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-20    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-4 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-21    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-5 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-22    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-6 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-23    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-7 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-24    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-8 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-25    PIC X(1).
            10 WIEN-SATZ-DATEN-PROD-SCHL-9 PIC X(4) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-26    PIC X(1).
            10 WIEN-SATZ-DATEN-TARIF       PIC X(10).
            10 WIEN-SATZ-DATEN-SEMIK-27    PIC X(1).
            10 WIEN-SATZ-DATEN-AN-BEITRAG  PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-28    PIC X(1).
            10 WIEN-SATZ-DATEN-BEITRAG-SUM PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-29    PIC X(1).
            10 WIEN-SATZ-DATEN-SUMME       PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-30    PIC X(1).
            10 WIEN-SATZ-DATEN-AN-BEARB-GEB PIC X(5) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-31    PIC X(1).
            10 WIEN-SATZ-DATEN-AN-ZAHLW    PIC X(3)  JUST.
            10 WIEN-SATZ-DATEN-SEMIK-32    PIC X(1).
            10 WIEN-SATZ-DATEN-EINMERLAEGE PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-33    PIC X(1).
            10 WIEN-SATZ-DATEN-AN-BEGINN.
               15 WIEN-SATZ-DATEN-B-TAG    PIC X(2).
               15 WIEN-SATZ-DATEN-B-P1     PIC X(1).
               15 WIEN-SATZ-DATEN-B-MONAT  PIC X(2).
               15 WIEN-SATZ-DATEN-B-P2     PIC X(1).
               15 WIEN-SATZ-DATEN-B-JAHRH  PIC X(2).
               15 WIEN-SATZ-DATEN-B-JAHR   PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-34    PIC X(1).
            10 WIEN-SATZ-DATEN-ABLAUF.
               15 WIEN-SATZ-DATEN-A-TAG    PIC X(2).
               15 WIEN-SATZ-DATEN-A-P1     PIC X(1).
               15 WIEN-SATZ-DATEN-A-MONAT  PIC X(2).
               15 WIEN-SATZ-DATEN-A-P2     PIC X(1).
               15 WIEN-SATZ-DATEN-A-JAHRH  PIC X(2).
               15 WIEN-SATZ-DATEN-A-JAHR   PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-35    PIC X(1).
            10 WIEN-SATZ-DATEN-PZD         PIC X(3) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-36    PIC X(1).
            10 WIEN-SATZ-DATEN-AN-JNP      PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-37    PIC X(1).
            10 WIEN-SATZ-DATEN-PRAEM-SUM   PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-38    PIC X(1).
            10 WIEN-SATZ-DATEN-EINHEITEN   PIC X(10) JUST.
            10 WIEN-SATZ-DATEN-SEMIK-39    PIC X(1).
            10 WIEN-SATZ-DATEN-ANTRAGS-ART PIC X(3).
            10 WIEN-SATZ-DATEN-SEMIK-40    PIC X(1).
            10 WIEN-SATZ-DATEN-ANTRAG-DAT.
               15 WIEN-SATZ-DATEN-AD-TAG   PIC X(2).
               15 WIEN-SATZ-DATEN-AD-P1    PIC X(1).
               15 WIEN-SATZ-DATEN-AD-MONAT PIC X(2).
               15 WIEN-SATZ-DATEN-AD-P2    PIC X(1).
               15 WIEN-SATZ-DATEN-AD-JAHRH PIC X(2).
               15 WIEN-SATZ-DATEN-AD-JAHR  PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-41    PIC X(1).
            10 WIEN-SATZ-DATEN-STRUK-DAT.
               15 WIEN-SATZ-DATEN-SD-TAG   PIC X(2).
               15 WIEN-SATZ-DATEN-SD-P1    PIC X(1).
               15 WIEN-SATZ-DATEN-SD-MONAT PIC X(2).
               15 WIEN-SATZ-DATEN-SD-P2    PIC X(1).
               15 WIEN-SATZ-DATEN-SD-JAHRH PIC X(2).
               15 WIEN-SATZ-DATEN-SD-JAHR  PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-42    PIC X(1).
            10 WIEN-SATZ-DATEN-ERF-DAT.
               15 WIEN-SATZ-DATEN-ED-TAG   PIC X(2).
               15 WIEN-SATZ-DATEN-ED-P1    PIC X(1).
               15 WIEN-SATZ-DATEN-ED-MONAT PIC X(2).
               15 WIEN-SATZ-DATEN-ED-P2    PIC X(1).
               15 WIEN-SATZ-DATEN-ED-JAHRH PIC X(2).
               15 WIEN-SATZ-DATEN-ED-JAHR  PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-43    PIC X(1).
            10 WIEN-SATZ-DATEN-STATISI-DAT.
               15 WIEN-SATZ-DATEN-ST-TAG   PIC X(2).
               15 WIEN-SATZ-DATEN-ST-P1    PIC X(1).
               15 WIEN-SATZ-DATEN-ST-MONAT PIC X(2).
               15 WIEN-SATZ-DATEN-ST-P2    PIC X(1).
               15 WIEN-SATZ-DATEN-ST-JAHRH PIC X(2).
               15 WIEN-SATZ-DATEN-ST-JAHR  PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-44    PIC X(1).
            10 WIEN-SATZ-DATEN-ERFASSER-ID PIC X(7).
            10 WIEN-SATZ-DATEN-SEMIK-45    PIC X(1).
            10 WIEN-SATZ-DATEN-TIMESTAMP.
               15 WIEN-SATZ-DATEN-TAG      PIC X(2).
               15 WIEN-SATZ-DATEN-P1       PIC X(1).
               15 WIEN-SATZ-DATEN-MONAT    PIC X(2).
               15 WIEN-SATZ-DATEN-P2       PIC X(1).
               15 WIEN-SATZ-DATEN-JAHRH    PIC X(2).
               15 WIEN-SATZ-DATEN-JAHR     PIC X(2).
               15 WIEN-SATZ-DATEN-SPACE    PIC X(1).
               15 WIEN-SATZ-DATEN-STUNDE   PIC X(2).
               15 WIEN-SATZ-DATEN-DOPPELP1 PIC X(1).
               15 WIEN-SATZ-DATEN-MINUTE   PIC X(2).
               15 WIEN-SATZ-DATEN-DOPPELP2 PIC X(1).
               15 WIEN-SATZ-DATEN-SEKUNDE  PIC X(2).
            10 WIEN-SATZ-DATEN-SEMIK-46    PIC X(1).
            10 WIEN-SATZ-DATEN-STAATS-ANG  PIC X(5).
            10 WIEN-SATZ-DATEN-SEMIK-47    PIC X(1).
            10 WIEN-SATZ-DATEN-PEP-STATUS  PIC X(3).
            10 WIEN-SATZ-DATEN-SEMIK-48    PIC X(1).
            10 WIEN-SATZ-DATEN-BRANCHE     PIC X(40).
            10 WIEN-SATZ-DATEN-SEMIK-49    PIC X(1).
            10 WIEN-SATZ-DATEN-BERUFSGRP   PIC X(40).
            10 WIEN-SATZ-DATEN-SEMIK-50    PIC X(1).
            10 WIEN-SATZ-DATEN-GEN-ANT-NR  PIC Z(14)9.
            10 WIEN-SATZ-DATEN-SEMIK-51    PIC X(1).
            10 WIEN-SATZ-DATEN-GEN-B-ANT-NR PIC Z(14)9.
            10 WIEN-SATZ-DATEN-SEMIK-52    PIC X(1).
            10 WIEN-SATZ-DATEN-WEITERE-INH OCCURS 4.
               15 WIEN-S-DATEN-W-INH-KD-VORNAME  PIC X(26).
               15 WIEN-SATZ-DATEN-SEMIK1-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-KD-ZUNAME   PIC X(26).
               15 WIEN-SATZ-DATEN-SEMIK2-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-KD-GEB-DAT  PIC X(10).
               15 WIEN-SATZ-DATEN-SEMIK3-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-KD-AUSL-KZ  PIC X(3).
               15 WIEN-SATZ-DATEN-SEMIK4-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-KD-PLZ      PIC X(6).
               15 WIEN-SATZ-DATEN-SEMIK5-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-KD-ORT      PIC X(21).
               15 WIEN-SATZ-DATEN-SEMIK6-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-STAATS-ANG  PIC X(5).
               15 WIEN-SATZ-DATEN-SEMIK7-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-PEP-STATUS  PIC X(3).
               15 WIEN-SATZ-DATEN-SEMIK8-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-BRANCHE     PIC X(40).
               15 WIEN-SATZ-DATEN-SEMIK9-INH     PIC X(1).
               15 WIEN-S-DATEN-W-INH-BERUFSGRP   PIC X(40).
               15 WIEN-SATZ-DATEN-SEMIK10-INH    PIC X(1).
      *** DIE SEMIK-53 BIS 92 SIND NUR IM PMAN0076 DEFINITON
      *** KOPFZEILE.DAHER WIRD MIT SEMIK-93 WEITERGEMACHT        
            10 WIEN-SATZ-DATEN-AKT-ANTR-STA      PIC Z(1)9.
            10 WIEN-SATZ-DATEN-SEMIK-93          PIC X(1).
            10 WIEN-SATZ-DATEN-KD-NUMMER-2       PIC X(18).
            10 WIEN-SATZ-DATEN-SEMIK-94          PIC X(1).
            10 WIEN-SATZ-DATEN-WEITER-INH-DIL OCCURS 5.
               15 WIEN-S-DATEN-W-INH-DIL         PIC  X(004).
               15 WIEN-SATZ-DATEN-SEMIK1-INH-DIL PIC  X(001).
      *** DIE SEMIK-95 BIS 99 SIND NUR IM PMAN0076 DEFINITON
      *** KOPFZEILE.DAHER WIRD MIT SEMIK-100 WEITERGEMACHT        
            10 WIEN-SATZ-DATEN-FILLER            PIC  X(030).
      *

      ******************************************************************
      *    *** LAENGE 1433 BYTES ***
      ******************************************************************



