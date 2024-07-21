      *---------------------------------------------------------------*
      *
      *    COPY-MEMBER FUER MASKENFELDER ANTRAG
      *    ------------------------------------
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!21.11.06!2.1  ! NEU                      ! HEIDER   *
      * AAN00655!27.12.06!2.2  ! WEITERE FELDER           ! HEIDER   *
      * AAN00655!08.01.07!2.3  ! EDVERS STATT EDIREK      ! HEIDER   *
      * AAN00655!20.02.07!2.6  ! ERBEGD                   ! HEIDER   *
      * AAN00753!12.07.07!2.8  ! EPIN AUF 9 STELLEN ERW.  ! HEIDER   *
      * AAN00837!24.09.08!2.9  ! EINFÜGEN FILLER          ! RAHNER   *
      * AAN00824!24.11.08!2.11 ! ELEKTR. ANTRAG GES.   26 ! RAHNER   *
      * AAN00824!25.11.08!2.11 ! ELEKTR. ANTRAG GES.16    ! RAHNER   *
      * AAN01033!14.12.11!2.13 ! DATEN GELDWÄSCHE ECT.    ! RAHNER   *
      * AAN01104!        !     ! KONSUMBONUS              ! RAHNER   *
      * AAN01073!07.06.13!2.15 ! SEPA-MANDAT              ! HEIDER   *
      * AAN01073!25.07.13!2.16 ! SEPA (IBAN)              ! RAHNER   *
      * AAN01073!24.10.13!2.17 ! SEPA (BIC)               ! RAHNER   *
      * AAN01198!05.05.15!2.18 ! ELEKTR. ANTRAG GES. 90   ! RAHNER   *
      * AAN01198!11.08.15!2.19 ! ANPASSUNGEN FÜR GES. 90  ! RAHNER   *
      * AAN01425!21.08.19!2.23 ! ANZ.BARREN GES. 79       ! RAHNER   *
      * AAN01449!02.12.19!     ! FELDNAMEN ALLGEMEINGÜLIG ! HEIDER   *
      *         !        !     !                          !          *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME    *
      * ------------------------------------------------------------*
      * ANTRAG-1930!        ! NEU FELD DEVISENINLÄNDER    ! RAHNER  *
      *            !        !                             !         *
      *            !        !                             !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
           20  DRAN0178-MASKENFELDER-ANTRAG.

               25 DRAN0178-TABELLE OCCURS 15.

                  30 EAMTKZ         PIC  X(011).
                  30 EANART         PIC  X(001).
                  30 EAUFND         PIC  X(006).
                  30 EAUSL          PIC  X(003).
                  30 EBEITR         PIC  X(009)  JUST.
                  30 EBERPR         PIC  X(001).
                  30 EBETR1         PIC  X(009)  JUST.
                  30 EBETR2         PIC  X(009)  JUST.
                  30 EVERMP REDEFINES EBETR2
                                    PIC  X(009)  JUST.
                  30 EBLZ           PIC  X(008).
                  30 EBUZBJ         PIC  X(001).
                  30 EBUZRJ         PIC  X(001).
                  30 EBZDAU         PIC  X(003).
                  30 EBZEND         PIC  X(006).
                  30 EDVERS         PIC  X(001).
                  30 EDYNAJ         PIC  X(001).
                  30 EDYNPR         PIC  X(007).
                  30 EEIGEN         PIC  X(001).
                  30 EEINH          PIC  X(007)  JUST.
                  30 EFGNR          PIC  X(017).
                  30 EGEBUR         PIC  X(004).
                  30 EGESNR         PIC  X(002).
                  30 EHANDJ         PIC  X(001).
                  30 EHANDB         PIC  X(001).
                  30 EHANDT         PIC  X(001).
                  30 EHANDF         PIC  X(001).
                  30 EHANDL         PIC  X(001).
                  30 EHANDV         PIC  X(001).
                  30 EHAND2         PIC  X(001).
                  30 EKMBEG         PIC  X(006).
                  30 EKMJ           PIC  X(006).
                  30 EKOLST         PIC  X(001).
                  30 EKTONR         PIC  X(010)  JUST.
                  30 ELAUFZ         PIC  X(004).
                  30 EPESTR         PIC  X(006).
                  30 EPIN           PIC  X(009).
                  30 EPINPR         PIC  X(002).
                  30 EPLZ           PIC  X(005).
                  30 EPROD          PIC  X(004)  JUST.
                  30 EPZVEJ         PIC  X(001).
                  30 ERBALT         PIC  X(003).
                  30 ESTATI         PIC  X(006).
                  30 ESTATU         PIC  X(002).
                  30 ESUMME         PIC  X(009)  JUST.
                  30 ETARIF         PIC  X(015).
                  30 EUZVEJ         PIC  X(001).
                  30 EVBNR          PIC  X(007).
                  30 EVBPRZ         PIC  X(001).
                  30 EVBZEI         PIC  X(007).
                  30 EVEBEG         PIC  X(006).
                  30 EVEDAU         PIC  X(003).
                  30 EVEEND         PIC  X(006).
                  30 EVMNR          PIC  X(007).
                  30 EVNR           PIC  X(015).
                  30 EVORSC         PIC  X(017).
                  30 EWAEHR         PIC  X(001).
                  30 AWAEHR         PIC  X(003).
                  30 EZAHLW         PIC  X(001).
                  30 EZWP           PIC  X(001).
                  30 AZAHLW         PIC  X(004).
                  30 AVORLF         PIC  X(001).
                  30 AVORLF-NUM REDEFINES
                     AVORLF         PIC  9(001).
                  30 EKDNAM         PIC  X(026).
                  30 EKDVNA         PIC  X(026).
                  30 EKDORT         PIC  X(021).
                  30 ESPART         PIC  X(001).
                  30 ERBEGD         PIC  X(006).
                  30 EVBGW          PIC  X(007).
                  30 EVBGWP         PIC  X(001).
                  30 EKDTIT         PIC  X(026).
                  30 EKDSTR         PIC  X(026).
                  30 EKDSVN         PIC  X(010).
                  30 EKDKNR         PIC  X(015).
                  30 EKDBLZ         PIC  X(015).
                  30 EKFZBN         PIC  X(020).
                  30 EANREDE REDEFINES EKFZBN PIC X(20).
                  30 EKDSTAA        PIC  X(003).
                  30 EKDBGRP        PIC  X(040).
                  30 DVM-TELEFON REDEFINES EKDBGRP.
                     35 DVM-FA-TELE        PIC X(030).
                     35 DVM-FA-TELE-FILLER PIC X(010).
                  30 EKDBRAN        PIC  X(040).
                  30 DVM-FAX REDEFINES EKDBRAN.
                     35 DVM-FA-FAX         PIC X(030).
                     35 DVM-FA-FAX-FILLER  PIC X(010).
                  30 EKDPEP              PIC X(001).
                  30 EKDGEB              PIC X(006).
                  30 ESEPAM              PIC X(001).
                  30 EKONBON             PIC X(001).
                  30 EEINRGEB REDEFINES
                     EKONBON             PIC X(001).
                  30 EKDDIL   REDEFINES
                     EKONBON             PIC X(001).
                  30 EVNIBAN             PIC X(034).
                  30 EBIC                PIC X(011).
                  30 DVM-VNR-PZ          PIC X(002).
                  30 DVM-FA-RF           PIC X(003).
                  30 DVM-FA-NAME1        PIC X(030).
                  30 DVM-FA-NAME2        PIC X(030).
                  30 DVM-FA-STRASSE      PIC X(045).
                  30 EKDSTRA REDEFINES DVM-FA-STRASSE PIC X(045).
                  30 DVM-FA-PLZ          PIC X(015).
                  30 DVM-FA-ORT          PIC X(050).
                  30 DVM-FA-POSTF        PIC X(010).
                  30 DVM-FA-PLZ-POSTF    PIC X(005).
                  30 DVM-FA-ORT-POSTF    PIC X(040).
                  30 DVM-FA-MOBIL        PIC X(030).
                  30 DVM-FA-EMAIL        PIC X(050).
                  30 EKDMAIL REDEFINES DVM-FA-EMAIL PIC X(050).
                  30 DVM-FA-KZ-VST       PIC X(001).
                  30 DVM-FA-ST-NR        PIC X(015).
                  30 DVM-FA-HREG-AMTG    PIC X(050).
                  30 DVM-FA-HREG-NR      PIC X(030).
                  30 DVM-FA-INH          PIC X(100).
                  30 DVM-FA-KIH-VN       PIC X(030).
                  30 DVM-FA-KIH-VN-EKTOIVN REDEFINES DVM-FA-KIH-VN.
                     35 EKTOIVN          PIC X(030).
                  30 DVM-FA-KIH-ZN       PIC X(030).
                  30 DVM-FA-KIH-ZN-EKTOIZN REDEFINES DVM-FA-KIH-ZN.
                     35 EKTOIZN          PIC X(30).
                  30 DVM-FA-KIH-ABW-STR  PIC X(045).
                  30 DVM-FA-KIH-ABW-PLZ  PIC X(015).
                  30 DVM-FA-KIH-ABW-ORT  PIC X(050).
                  30 DVM-FA-GELDINST     PIC X(058).
                  30 DVM-KZ-FAC-OPT      PIC X(001).
                  30 DVM-UMSATZ-JAHR     PIC X(009).
                  30 DVM-KZ-GARANT       PIC X(001).
                  30 DVM-KZ-GW-GES       PIC X(001).
                  30 DVM-KZ-N-ERKL       PIC X(001).
                  30 DVM-SEPA-M-BEG      PIC X(006).
                  30 GEIGER-ANZ-BARREN   PIC X(004).
                  30 FILLER              PIC X(001).
      *********** ENDE DRAN0178 ********************

